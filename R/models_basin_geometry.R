#' models_basin_geometry
#'
#' @param models_basin_data output from wrangle_pfl
#' @param model_paths paths to the model files
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
models_basin_geometry <- function(models_basin_data, model_paths){
  purrr::imap(models_basin_data, function(tidy_pfl, y){
    tidy_pfl %>%
      dplyr::group_by(timestep) %>%
      dplyr::summarize(
        basin_length = dplyr::n(),
        basin_depth = min(elevation)
      ) %>%
      dplyr::mutate(
        model = y,
        # There is an error in the way I am subsetting
        class =  dplyr::first(stringr::str_subset(
          string = model_paths$pfl,
          pattern = model
        )) %>%
          dirname() %>%
          dirname() %>%
          basename()
      )
  })
}
