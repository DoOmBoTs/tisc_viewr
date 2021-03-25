#' file_paths
#'
#'  Creates a named list of paths for each file type the user wants to use from TISC
#'
#'  @param file_paths list of file types from TISC the user wants to include
#'  @param root_path the path to the root directory in which TISC models are located
#'
#'  @importFrom dplyr %>%
#'
#'  @examples
#'  @export
file_paths <- function(wanted_files, root_path){

  paths <- purrr::map(wanted_files, function(x){

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
