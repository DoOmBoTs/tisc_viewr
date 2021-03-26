#' get_well_tops
#'
#' @param models_well_location output from models_well_location
#' @param file_paths paths to all files within the root directory
#' @param model_names names of the models being analyzed
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
get_well_tops <- function(models_well_location, file_paths, model_names){

  # models_well_location
  # file_paths$pfl
  # file_paths$PRM
  # file_paths$UNIT
  # model_names

  pfl_text_data <- purrr::map(model_names, ~ stringr::str_subset(string = file_paths$pfl, pattern = .x)) %>%
    purrr::map(~ purrr::map(.x, read_lines))

  pfl_data <- purrr::map(model_names, ~ stringr::str_subset(string = file_paths$pfl, pattern = .x)) %>%
    purrr::map(~ map(.x, ~ tibble::as_tibble(utils::read.table(.x, header = FALSE, sep = "\t", stringsAsFactors = FALSE))))

  prm_data <- purrr::map(file_paths$PRM, read_lines)

  unit_data <- purrr::map(model_names, ~ stringr::str_subset(string = file_paths$UNIT, pattern = .x)) %>%
    purrr::map(~ purrr::map(.x, read_lines))

  bedrock_density <- purrr::map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^denscrust") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.numeric()
  })

  col_ids <- purrr::map2(pfl_text_data, bedrock_density, function(x, y){

    purrr::map(x, function(pfl){
      stringr::str_subset(string = pfl, pattern = "Densities") %>%
        stringr::str_extract_all(pattern = "\\d+")
    }) %>%
      purrr::map(function(bd){ #bd: bedrock density
        a <- unlist(bd, recursive = TRUE) %>%
          as.numeric()
        last_bd <- max(which(a == y), na.rm = TRUE) + 3
        3:(last_bd-1)
      })
  })

  # why is this going to NULL?
  out <- purrr::pmap(
    .l = list(pfl = pfl_data, well_loc = models_well_location, cols = col_ids),
    function(pfl, well_loc, cols){

      row_data <- purrr::pmap(list(a = pfl, b = well_loc, c = cols), function(a, b, c){
        a[a$V2 == b, -c]
      })

      n_cols <- max(purrr::map_dbl(row_data, ncol), na.rm = TRUE)

      purrr::map_dfr(row_data, function(x){
        x <- as.numeric(x)
        d <- as.data.frame(t(c(x, rep(NA_real_, n_cols - length(x)))))
        colnames(d) <- c("X", "Y", "bedrock", glue::glue("sediment{1:(n_cols-3)}"))
        d
      }) %>%
        dplyr::mutate(timestep = dplyr::row_number()) %>%
        dplyr::relocate(timestep, .before = X)
      }
  )

  names(out) <- model_names

  out

}
