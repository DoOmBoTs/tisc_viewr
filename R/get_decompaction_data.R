#' get_decompaction_data
#'
#' @param models_well_tops output from get_well_tops
#' @param model_paths list of the paths to the files within the root directory
#'  @param model_names name of the models used
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples

get_decompaction_data <- function(models_well_tops, model_paths, model_names){
  # Equations ----
  #
  # campact_curve = (phi1 -phi2)/(compact_depth - z_initial)
  # compact_thk = unit_depth - dplyr::lag(unit_depth)
  # porosity = (campact_curve * (z_initial - lag(unit_depth))) + phi1
  # decompact_thk = compact_thk + compact_thk * ((y/100)-(porosity/100))
  # water_depth = sea_level - max(unit_depth)
  #

  # prm_data <- purrr::map(model_paths$PRM, readr::read_lines)
  prm_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = model_paths$PRM, pattern = .x)) %>%
    purrr::map(function(x){ dplyr::first(x) %>% readr::read_lines() })

  phi1 <- purrr::map_dbl(prm_data, function(x){                              # sed_porosity from .PRM expressed as a %
    stringr::str_subset(string = x, pattern = "^sed_porosity") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  }) * 100

  phi2 = phi1 - log(phi1)           # compact_depth from .PRM says dz to decrease by an order 'e' to phi1, is log base e or base 10?

  compact_depth = purrr::map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^compact_depth") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  switch_sea = purrr::map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^switch_sea") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  # Rework this into creating the data.frame with 2 columns(timestep & depth)
  slv_data <- furrr::future_map(model_names, ~ stringr::str_subset(string = model_paths$SLV, pattern = .x)) %>%
    purrr::map(function(x){ dplyr::first(x) %>% readr::read_lines(skip_empty_rows = T) %>%
        tibble::as_tibble() %>%
        tidyr::separate(col = value,
                        into = c("timestep", "sea_level"),
                        sep = "\t",
                        fill = "right"
        ) %>%
        ## remove redundant lines
        tidyr::drop_na() %>%
        ## change type for all columns to `double`
        purrr::map_df(as.numeric)})

  purrr::pmap(list(models_well_tops, phi1, phi2, compact_depth, slv_data, switch_sea), function(x, y, z, a, b, c){

    # variables used to test function internally
    # x <- models_well_tops[[1]]
    # y <- phi1[[1]]
    # z <- phi2[[1]]
    # a <- compact_depth[[1]]
    # b <- slv_data[[1]]
    # c <- switch_sea[[1]]

    if (length(x) > 0) {
      if (c == 0){

        x[,-(2:3)] %>%
          tidyr::pivot_longer(cols = -timestep, names_to = "sediment", values_to = "unit_depth") %>%
          dplyr::ungroup() %>%
          tidyr::drop_na() %>%
          dplyr::group_by(sediment) %>%
          dplyr::mutate(z_initial = dplyr::first(unit_depth)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(timestep) %>%
          dplyr::mutate(campact_curve = (z - y)/(a - z_initial),
                 compact_thk = unit_depth - dplyr::lag(unit_depth),
                 top_porosity = dplyr::if_else(condition =  unit_depth > z_initial,
                                        true = y,
                                        false = (campact_curve * (z_initial - unit_depth)) + y),
                 bottom_porosity = dplyr::if_else(condition =  unit_depth > z_initial,
                                           true = y,
                                           false = (campact_curve * (z_initial - dplyr::lag(unit_depth))) + y),
                 avg_porosity = (top_porosity + bottom_porosity)/2,
                 decompact_thk = compact_thk + compact_thk * ((y/100)-(avg_porosity/100)),
                 sea_level = 0,
                 water_depth =  dplyr::if_else(condition = unit_depth > sea_level,
                                        true = 0,
                                        false = sea_level - max(unit_depth))) %>%
          dplyr::ungroup()

      } else {
        x[,-(2:3)] %>%
          tidyr::pivot_longer(cols = -timestep, names_to = "sediment", values_to = "unit_depth") %>%
          dplyr::ungroup() %>%
          tidyr::drop_na() %>%
          dplyr::group_by(sediment) %>%
          dplyr::mutate(z_initial = dplyr::first(unit_depth)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(timestep) %>%
          dplyr::mutate(campact_curve = (z - y)/(a - z_initial),
                 compact_thk = unit_depth - dplyr::lag(unit_depth),
                 top_porosity = dplyr::if_else(condition =  unit_depth > z_initial,
                                        true = y,
                                        false = (campact_curve * (z_initial - unit_depth)) + y),
                 bottom_porosity = dplyr::if_else(condition =  unit_depth > z_initial,
                                           true = y,
                                           false = (campact_curve * (z_initial - dplyr::lag(unit_depth))) + y),
                 decompact_thk = compact_thk + compact_thk * ((y/100)-(avg_porosity/100))) %>%
          dplyr::left_join(y = b) %>%
          dplyr::mutate(sl1 = sea_level,
                 sl2 = sea_level) %>%
          tidyr::fill(sl1, .direction = "down") %>%
          tidyr::fill(sl2, .direction = "up") %>%
          dplyr::mutate(
            sea_level = ifelse(is.na(sea_level),
                               (sl1+sl2)/2,
                               sea_level),
            # sea_level = stats::smooth.spline(x = sea_level, spar = 0.3)$y,    # smooth.spline doesn't seem to work with 0 values or negative
            water_depth =  dplyr::if_else(condition = unit_depth > sea_level,
                                   true = 0,
                                   false = sea_level - max(unit_depth))) %>%
          dplyr::ungroup() %>%
          dplyr::select(-c(sl1,sl2))
      }
    }
  })


}
