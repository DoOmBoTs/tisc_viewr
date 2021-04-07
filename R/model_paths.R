#' model_paths
#'
#'  Creates a named list of paths for each file type the user wants to use from TISC
#'
#' @param wanted_files List of the file's the user wants to pass from TISC
#' @param root_path The path to the directory where all the models are stored
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
model_paths <- function(wanted_files, root_path){

  paths <- furrr::future_map(wanted_files, function(x){

    if (x == "pfl") {
      fs::dir_ls(
        path = root_path,
        recurse = TRUE,
        all = TRUE,
        regexp = sprintf("%s$", x)
      ) %>%
        stringr::str_subset(pattern = sprintf("NS\\.%s$", x), negate = TRUE)
    } else {
      fs::dir_ls(
        path = root_path,
        recurse = TRUE,
        all = TRUE,
        regexp = sprintf("%s$", x)
      )
    }

  })

  names(paths) <- wanted_files

  paths
}
