#' Title
#'
#' @param models_well_location
#' @param pfl_files
#' @param prm_files
#' @param unit_files
#' @param model_names
#'
#' @return
#' @export
#'
#' @examples
get_well_tops <- function(models_well_location, pfl_files, prm_files, unit_files, model_names){

  # models_well_location
  # pfl_files
  # prm_files
  # unit_files
  # model_names

  pfl_text_data <- map(model_names, ~ stringr::str_subset(string = pfl_files, pattern = .x)) %>%
    map(~ map(.x, read_lines))

  pfl_data <- map(model_names, ~ stringr::str_subset(string = pfl_files, pattern = .x)) %>%
    map(~ map(.x, ~ as_tibble(read.table(.x, header = FALSE, sep = "\t", stringsAsFactors = FALSE))))

  prm_data <- map(prm_files, read_lines)

  unit_data <- map(model_names, ~ stringr::str_subset(string = unit_files, pattern = .x)) %>%
    map(~ map(.x, read_lines))

  bedrock_density <- map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^denscrust") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.numeric()
  })

  col_ids <- map2(pfl_text_data, bedrock_density, function(x, y){

    map(x, function(pfl){
      stringr::str_subset(string = pfl, pattern = "Densities") %>%
        str_extract_all(pattern = "\\d+")
    }) %>%
      map(function(bd){ #bd: bedrock density
        a <- unlist(bd, recursive = TRUE) %>%
          as.numeric()
        last_bd <- max(which(a == y), na.rm = TRUE) + 3
        3:(last_bd-1)
      })
  })

  # why is this going to NULL?
  out <- pmap(
    .l = list(pfl = pfl_data, well_loc = models_well_location, cols = col_ids),
    function(pfl, well_loc, cols){

      row_data <- pmap(list(a = pfl, b = well_loc, c = cols), function(a, b, c){
        a[a$V2 == b, -c]
      })

      n_cols <- max(map_dbl(row_data, ncol), na.rm = TRUE)

      map_dfr(row_data, function(x){
        x <- as.numeric(x)
        d <- as.data.frame(t(c(x, rep(NA_real_, n_cols - length(x)))))
        colnames(d) <- c("X", "Y", "bedrock", glue("sediment{1:(n_cols-3)}"))
        d
      }) %>%
        mutate(timestep = row_number()) %>%
        relocate(timestep, .before = X)
      }
  )

  names(out) <- model_names

  out

}
