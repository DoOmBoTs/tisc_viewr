#' compute_basin_symmetry_data
#'
#' @param models_basin_data
#' @param file_paths
#'
#' @return
#' @export
#'
#' @examples
compute_basin_symmetry_data <- function(models_basin_data, file_paths){

  purrr::imap(models_basin_data, function(tidy_pfl, y){

    midpoint_data <- tidy_pfl %>%
      dplyr::group_by(timestep) %>%
      dplyr::summarize(middle = stats::median(Y, na.rm = TRUE))

    tidy_pfl %>%
      dplyr::group_split(timestep) %>%
      purrr::map2_dfr(midpoint_data$middle, function(pfl, middle){

        data.frame(
          left = sum(abs(pfl$elevation[pfl$Y < middle])),
          right = sum(abs(pfl$elevation[pfl$Y > middle]))
        )

      })

    tidy_pfl %>%
      dplyr::group_nest(timestep) %>%
      dplyr::mutate(
        midpoint = purrr::map_dbl(data, ~ stats::median(.x$Y, na.rm = TRUE)),
        left = purrr::map2_dbl(data, midpoint, ~ sum(abs(.x$elevation[.x$Y < .y]))),
        right = purrr::map2_dbl(data, midpoint, ~ sum(abs(.x$elevation[.x$Y > .y]))),
        ratio = right / left,
        difference = right - left,
        model = y,
        class =  dplyr::first(readr::str_subset(string = file_paths$pfl, pattern = model)) %>%
          dirname() %>%
          dirname() %>%
          basename()
      )

  })

}
