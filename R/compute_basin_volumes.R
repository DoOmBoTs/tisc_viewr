#' compute_basin_volumes
#'
#' @param Y
#' @param elevation
#' @param middle
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
