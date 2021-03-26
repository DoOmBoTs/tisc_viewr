#' get_subsidence_data
#'
#' @param decompaction_data output from get_decompaction_data
#' @param file_paths list of the paths to the files within the root directory
#'
#' @return
#' @export
#'
#' @examples
get_subsidence_data <- function(decompaction_data, file_paths){
  # Equations ----
  #
  # z_tec Equation pulled from:
  #
  # BasinVis 1.0: A MATLABs-based program for sedimentary basin subsidence analysis and visualization
  # Eun Young Lee, Johannes Novotny, Michael Wagreich
  #
  # z_tec                    tectonic elevation w/seaLoad (m)
  # s                        decompacted lithoUnit (m)
  # w_d                      water depth (m), wrangle from .pfl (if top sed unit is below 0 (0 - top_sed_elevation) )
  # del_sl                   paleo seaLevel (m), extract from .prm | .slv
  # rho_m                    density mantle (kg/m^3), extract from .prm | .UNIT == densmantle
  # rho_s                    density sediment (kg/m^3), extract from .prm | .UNIT == denssedim-sed_porosity*denswater
  # rho_w                    density water (kg/m^3), extract from .prm == denswater
  #
  # z_tec = s * ((rho_m - rho_s)/(rho_m - rho_w)) + w_d - del_sl * (rho_m/(rho_m-rho_w))
  #
  # gather TISC model inputs ----

  prm_data <- purrr::map(file_paths$PRM, read_lines)

  rho_s <- purrr::map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^denssedim") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  rho_m <- purrr::map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^densmantle") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  # Build tectonic subsidence data.frame ----

  purrr::pmap(list(decompaction_data, rho_m, rho_s), function(x, y, z){

    # x <- decompaction_data[[1]]
    # y <- rho_m[[1]]
    # z <- rho_s[[1]]

    if (length(x) > 0) {

      x %>%
        dplyr::group_by(timestep) %>%
        dplyr::summarize(
          compact_thk = sum(compact_thk, na.rm = TRUE),
          decompact_thk = sum(decompact_thk, na.rm = TRUE),
          water_depth = dplyr::last(water_depth),
          rho_m = y,
          rho_w = 1000,
          rho_s = z,
          sub_total = -dplyr::first(unit_depth),
          sea_level = dplyr::last(sea_level)) %>%
        dplyr::mutate(
          del_sl = sea_level - dplyr::lag(sea_level),
          sub_tec = decompact_thk * ((rho_m - rho_s)/(rho_m - rho_w)) + water_depth - del_sl * (rho_m/(rho_m-rho_w))
        ) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(
          spline_sub_tec = stats::smooth.spline(sub_tec, spar = 0.5)$y,
          # spline_water_depth = if (water_depth != 0) {
          #   smooth.spline(water_depth, spar = 0.5)$y
          # } else {
          #   water_depth
          # }
          spline_water_depth = stats::smooth.spline(water_depth, spar = 0.5)$y
        )
    }
  })

}
