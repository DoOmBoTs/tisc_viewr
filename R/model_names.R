#' model_names
#'
#' @param file_paths List of paths to the pfl files that contain the models being analyzed.
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
model_names <- function(file_paths){
  unique(basename(dirname(file_paths$pfl)))
}
