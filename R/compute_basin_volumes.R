#' compute_basin_volumes
#'
#' @param Y values from within wrangle_pfl
#' @param elevation elevation at a each point within the basin for calculating the Reimman sum
#' @param middle median of length of basin
#'
#' @return
#' @export
#'
#' @examples
compute_basin_volumes <- function(Y, elevation, middle){

  data.frame(
    left = sum(abs(elevation[Y < middle])),
    right = sum(abs(elevation[Y > middle]))
  )

}
