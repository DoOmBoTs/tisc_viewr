#' file_paths
#'
#'  Creates a named list of paths for each file type the user wants to use from TISC
#'
#'  @param file_paths list of file types from TISC the user wants to include
#'  @param root_path the path to the root directory in which TISC models are located
#'
#'  @importFrom purrr map
#'  @importFrom fs dir_ls
#'  @importFrom stringr str_subset
#'
#'  @examples
#'  paths <-  file_paths(wanted_files = c("pfl", "UNIT", "PRM"), root_path = )
#'
#'  @export


file_paths <- function(wanted_files, root_path){

  paths <- map(wanted_files, function(x){

    if (x == "pfl") {
      dir_ls(
        path = root_path,
        recurse = TRUE,
        all = TRUE,
        regexp = sprintf("%s$", x)
      ) %>%
        str_subset(pattern = sprintf("NS\\.%s$", x), negate = TRUE)
    } else {
      dir_ls(
        path = root_path,
        recurse = TRUE,
        all = TRUE,
        regexp = sprintf("%s$", x)
      )
    }

  })

  paths <- names(paths) <- wanted_files

  paths

}
