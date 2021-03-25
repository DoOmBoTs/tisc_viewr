#' Title
#'
#' @param models_well_tops
#' @param prm_files
#' @param slv_files
#'
#' @return
#' @export
#'
#' @examples

get_decompaction_data <- function(models_well_tops, prm_files, slv_files){
  # Equations ----
  #
  # campact_curve = (phi1 -phi2)/(compact_depth - z_initial)
  # compact_thk = unit_depth - dplyr::lag(unit_depth)
  # porosity = (campact_curve * (z_initial - lag(unit_depth))) + phi1
  # decompact_thk = compact_thk + compact_thk * ((y/100)-(porosity/100))
  # water_depth = sea_level - max(unit_depth)
  #
  # gather TISC model inputs ----

  prm_data <- map(prm_files, read_lines)
  phi1 <- map_dbl(prm_data, function(x){                              # sed_porosity from .PRM expressed as a %
    stringr::str_subset(string = x, pattern = "^sed_porosity") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  }) * 100

  phi2 = phi1 - log(phi1)           # compact_depth from .PRM says dz to decrease by an order 'e' to phi1, is log base e or base 10?

  compact_depth = map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^compact_depth") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  switch_sea = map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^switch_sea") %>%
      stringr::str_extract(pattern = "\\d+\\.*\\d*") %>%
      as.numeric()
  })

  # Rework this into creating the data.frame with 2 columns(timestep & depth)

    slv_data =  slv_data <- map(slv_files, function(x){

    x %>%
      read_lines(skip_empty_rows = T) %>%
      as_tibble() %>%
      separate(col = value,
               into = c("timestep", "sea_level"),
               sep = "\t",
               fill = "right"
      ) %>%
      ## remove redundant lines
      drop_na() %>%
      ## change type for all columns to `double`
      map_df(as.numeric)

  })


  # Build decompaction data.frame conditional case----

  pmap(list(models_well_tops, phi1, phi2, compact_depth, slv_data, switch_sea), function(x, y, z, a, b, c){

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
          pivot_longer(cols = -timestep, names_to = "sediment", values_to = "unit_depth") %>%
          ungroup() %>%
          drop_na() %>%
          group_by(sediment) %>%
          mutate(z_initial = first(unit_depth)) %>%
          ungroup() %>%
          group_by(timestep) %>%
          mutate(campact_curve = (z - y)/(a - z_initial),
                 compact_thk = unit_depth - dplyr::lag(unit_depth),
                 top_porosity = if_else(condition =  unit_depth > z_initial,
                                        true = y,
                                        false = (campact_curve * (z_initial - unit_depth)) + y),
                 bottom_porosity = if_else(condition =  unit_depth > z_initial,
                                           true = y,
                                           false = (campact_curve * (z_initial - lag(unit_depth))) + y),
                 avg_porosity = (top_porosity + bottom_porosity)/2,
                 decompact_thk = compact_thk + compact_thk * ((y/100)-(avg_porosity/100)),
                 sea_level = 0,
                 water_depth =  if_else(condition = unit_depth > sea_level,
                                        true = 0,
                                        false = sea_level - max(unit_depth))) %>%
          ungroup()

      } else {
        x[,-(2:3)] %>%
          pivot_longer(cols = -timestep, names_to = "sediment", values_to = "unit_depth") %>%
          ungroup() %>%
          drop_na() %>%
          group_by(sediment) %>%
          mutate(z_initial = first(unit_depth)) %>%
          ungroup() %>%
          group_by(timestep) %>%
          mutate(campact_curve = (z - y)/(a - z_initial),
                 compact_thk = unit_depth - dplyr::lag(unit_depth),
                 top_porosity = if_else(condition =  unit_depth > z_initial,
                                        true = y,
                                        false = (campact_curve * (z_initial - unit_depth)) + y),
                 bottom_porosity = if_else(condition =  unit_depth > z_initial,
                                           true = y,
                                           false = (campact_curve * (z_initial - lag(unit_depth))) + y),
                 decompact_thk = compact_thk + compact_thk * ((y/100)-(avg_porosity/100))) %>%
          left_join(y = b) %>%
          mutate(sl1 = sea_level,
                 sl2 = sea_level) %>%
          tidyr::fill(sl1, .direction = "down") %>%
          tidyr::fill(sl2, .direction = "up") %>%
          mutate(
            sea_level = ifelse(is.na(sea_level),
                               (sl1+sl2)/2,
                               sea_level),
            # sea_level = stats::smooth.spline(x = sea_level, spar = 0.3)$y,    # smooth.spline doesn't seem to work with 0 values or negative
            water_depth =  if_else(condition = unit_depth > sea_level,
                                   true = 0,
                                   false = sea_level - max(unit_depth))) %>%
          ungroup() %>%
          select(-c(sl1,sl2))
      }
    }
  })


}
