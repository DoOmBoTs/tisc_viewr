#' models_well_locaton
#'
#' Location along the transect proscribed in the .PRFL file from tisc.
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
    purrr::map(~ dplyr::filter(.x, as.numeric(timestep) == max(as.numeric(timestep)))) %>%
    purrr::map(~ .x$Y[.x$elevation == min(.x$elevation)])
}
