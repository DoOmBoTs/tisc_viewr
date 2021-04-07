#' Wrangle pfl files
#'
#' Description here...
#'
#' @param file_paths description...
#' @param model_names description...
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
#'
wrangle_pfl <- function(file_paths, model_names){

  future::plan(future::multicore)

  pfl_text_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = file_paths$pfl, pattern = .x)) %>%
    purrr::map(~ purrr::map(.x, readr::read_lines))

  pfl_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = file_paths$pfl, pattern = .x)) %>%
    purrr::map(~ purrr::map(.x, ~ tibble::as_tibble(utils::read.table(.x, header = FALSE, sep = "\t", stringsAsFactors = FALSE))))

  prm_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = file_paths$PRM, pattern = .x)) %>%
    purrr::map(function(x){ first(x) %>% readr::read_lines() })

  unit_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = file_paths$UNIT, pattern = .x)) %>%
    purrr::map(function(x){ purrr::map(x,function(y){ y %>% readr::read_lines()}) })

  rm(file_paths)
  gc()

  bedrock_density <- furrr::future_map_dbl(prm_data, function(x){
    gc()
    stringr::str_subset(string = x, pattern = "^denscrust") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.numeric()
  })

  # is this being used anywhere?
  # init_final_time <- furrr::future_map(prm_data, function(x){
  #   stringr::str_subset(string = x, pattern = "^Timeini|^Timefinal") %>%
  #     stringr::str_extract(pattern = "\\d+") %>%
  #     as.numeric()
  # })

  rm(prm_data)
  gc()

  # is this being used anywhere?
  # fault_time <- furrr::future_map(unit_data, function(x){
  #   purrr::map(x, function(y){
  #     stringr::str_subset(string = y, pattern = "^time|^time_stop") %>%
  #       stringr::str_extract(pattern = "\\d+") %>%
  #       as.numeric()
  #   })
  # })

  rm(unit_data)
  gc()
  # Process pfl files ----

  col_ids <- furrr::future_map2(pfl_text_data, bedrock_density, function(x, y){
    gc()
    purrr::map(x, function(pfl){
      stringr::str_subset(string = pfl, pattern = "Densities") %>%
        stringr::str_extract_all(pattern = "\\d+")
    }) %>%
      purrr::map(function(bd){
        a <- unlist(bd, recursive = TRUE) %>%
          as.numeric()
        max(which(a == y), na.rm = TRUE) + 3
      })
  })

  rm(bedrock_density)
  rm(pfl_text_data)
  gc()

  # Extract columns of interest from the original pfl files ----

  new_pfl_data <- furrr::future_map2(pfl_data, col_ids, function(pfl, col){
    gc()
    purrr::map2(pfl[-1], col[-1],  ~ .x[c(2, .y)])
  }) %>%
    furrr::future_map(function(x){
      purrr::reduce(x, dplyr::left_join, by = "V2") %>%
        stats::setNames(c("Y", paste0("Z", 2:ncol(.))))
    })

  rm(pfl_data)
  rm(col_ids)
  gc()

  names(new_pfl_data) <- model_names

  rm(model_names)
  gc()

  furrr::future_map(new_pfl_data, function(x){
    gc()
    x %>%
      tidyr::pivot_longer(-Y,
                   names_to = "timestep",
                   names_prefix="Z_",
                   values_to = "elevation") %>%
      dplyr::mutate(timestep = forcats::fct_inorder(timestep)) %>%
      dplyr::group_by(timestep) %>%
      dplyr::summarise(find_basin_data(Y = Y, elevation_vector = elevation, n = 100)) %>%
      tidyr::drop_na() %>%
      dplyr::ungroup()
  })

}



