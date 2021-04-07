#' Find basin data
#'
#' Write description here...
#'
#' @param Y description...
#' @param elevation_vector description...
#' @param n description...
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
#'
find_basin_data <- function(Y, elevation_vector, n){

  # pass in X and Y from pfl
  # use X or Y based on if .UNIT file contains either vel_x or vel_y

  # would read string function to look through unit file also need to be
  # called on from this function?

  negative_runs <- rle(elevation_vector <= 0)

  tbl <- tibble::tibble(values = negative_runs$values, lengths = negative_runs$lengths) %>%
    dplyr::filter(values, lengths >= n) %>%
    dplyr::slice(dplyr::n())   # keep last if vel_x/vel_y vaule is - or keep first if +

  if(nrow(tbl) == 0) return(tibble::tibble(Y = NA_real_, elevation = NA_real_))

  i <- which(negative_runs$values & (negative_runs$lengths == tbl$lengths))
  cumsums <- cumsum(negative_runs$lengths)
  indices <- (cumsums[i-1]+1):cumsums[i]

  tibble::tibble(
    Y = Y[indices],
    elevation = elevation_vector[indices]
  )

}
