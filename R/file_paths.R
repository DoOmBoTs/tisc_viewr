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
  
  pahts <- names(paths) <- wanted_files
  
  paths
  
}
