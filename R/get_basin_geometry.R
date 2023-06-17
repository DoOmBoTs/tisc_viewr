#' get_basin_geometry
#'
#' @param models_basin_data output from wrangle_pfl
#' @param model_paths list of paths to the pfl files in the root directory
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
get_basin_geometry <- function(models_basin_data, model_paths){
  purrr::imap(models_basin_data, function(tidy_pfl, y){
    tidy_pfl %>%
      dplyr::group_by(timestep) %>%
      dplyr::summarize(
        basin_length = dplyr::n(),
        basin_depth = min(elevation)
      ) %>%
      dplyr::mutate(
        model = y,
        class = dplyr::first(stringr::str_subset(
          string = fp$UNIT,
          pattern = y)
        ) %>%
          dirname() %>% 
          dirname() %>%
          basename()
      )
  })
}
