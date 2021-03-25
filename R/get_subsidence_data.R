#' Title
#'
#' @param decompaction_data
#' @param prm_files
#'
#' @return
#' @export
#'
#' @examples
get_subsidence_data <- function(decompaction_data, prm_files){
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

  prm_data <- map(prm_files, read_lines)

  rho_s <- map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^denssedim") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  rho_m <- map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^densmantle") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  # Build tectonic subsidence data.frame ----

  pmap(list(decompaction_data, rho_m, rho_s), function(x, y, z){

    # x <- decompaction_data[[1]]
    # y <- rho_m[[1]]
    # z <- rho_s[[1]]

    if (length(x) > 0) {

      x %>%
        group_by(timestep) %>%
        summarize(
          compact_thk = sum(compact_thk, na.rm = TRUE),
          decompact_thk = sum(decompact_thk, na.rm = TRUE),
          water_depth = last(water_depth),
          rho_m = y,
          rho_w = 1000,
          rho_s = z,
          sub_total = -first(unit_depth),
          sea_level = last(sea_level)) %>%
        mutate(
          del_sl = sea_level - lag(sea_level),
          sub_tec = decompact_thk * ((rho_m - rho_s)/(rho_m - rho_w)) + water_depth - del_sl * (rho_m/(rho_m-rho_w))
        ) %>%
        drop_na() %>%
        mutate(
          spline_sub_tec = smooth.spline(sub_tec, spar = 0.5)$y,
          # spline_water_depth = if (water_depth != 0) {
          #   smooth.spline(water_depth, spar = 0.5)$y
          # } else {
          #   water_depth
          # }
          spline_water_depth = smooth.spline(water_depth, spar = 0.5)$y
        )
    }
  })

}
