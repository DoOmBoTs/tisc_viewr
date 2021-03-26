#' models_well_locaton
#'
#' @param models_basin_data output from wrangle_pfl
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
models_well_locaton <- function(models_basin_data) {
  models_basin_data %>%
    purrr::map(~ stats::filter(.x, as.numeric(timestep) == max(as.numeric(timestep)))) %>%
    purrr::map(~ .x$Y[.x$elevation == min(.x$elevation)])
}
