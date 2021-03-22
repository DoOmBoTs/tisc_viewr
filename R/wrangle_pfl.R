#
# ~~ * ~~ WRANGLE PFL DATA
#

wrangle_pfl <- function(pfl_files, prm_files, unit_files, model_names){ 
  
  # Get data from file paths ----
  
  pfl_text_data <- map(model_names, ~ stringr::str_subset(string = pfl_files, pattern = .x)) %>%
    map(~ map(.x, read_lines))
  
  pfl_data <- map(model_names, ~ stringr::str_subset(string = pfl_files, pattern = .x)) %>%
    map(~ map(.x, ~ as_tibble(read.table(.x, header = FALSE, sep = "\t", stringsAsFactors = FALSE))))
  
  prm_data <- map(prm_files, read_lines)
  
  unit_data <- map(model_names, ~ stringr::str_subset(string = unit_files, pattern = .x)) %>%
    map(~ map(.x, read_lines))
  
  bedrock_density <- map_dbl(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^denscrust") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.numeric()
  })
  
  init_final_time <- map(prm_data, function(x){
    stringr::str_subset(string = x, pattern = "^Timeini|^Timefinal") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.numeric()
  })
  
  fault_time <- map(unit_data, function(x){
    map(x, function(y){
      str_subset(string = y, pattern = "^time|^time_stop") %>%
        str_extract(pattern = "\\d+") %>%
        as.numeric()
    })
  })
  
  
  # Process pfl files ----
  
  col_ids <- map2(pfl_text_data, bedrock_density, function(x, y){
    
    map(x, function(pfl){
      stringr::str_subset(string = pfl, pattern = "Densities") %>%
        str_extract_all(pattern = "\\d+")
    }) %>%
      map(function(bd){
        a <- unlist(bd, recursive = TRUE) %>%
          as.numeric()
        max(which(a == y), na.rm = TRUE) + 3
      })
  }) 
  
  
  # Extract columns of interest from the original pfl files ----
  
  new_pfl_data <- map2(pfl_data, col_ids, function(pfl, col){
    map2(pfl[-1], col[-1],  ~ .x[c(2, .y)])
  }) %>%
    map(function(x){
      reduce(x, left_join, by = "V2") %>%
        setNames(c("Y", paste0("Z", 2:ncol(.)))) 
    })
  
  names(new_pfl_data) <- model_names
  
  
  map(new_pfl_data, function(x){
    x %>%
      pivot_longer(-Y,
                   names_to = "timestep",
                   names_prefix="Z_",
                   values_to = "elevation") %>%
      mutate(timestep = fct_inorder(timestep)) %>%
      group_by(timestep) %>%
      summarize(find_basin_data(Y = Y, elevation_vector = elevation, n = 100)) %>%
      drop_na() %>%
      ungroup()
  })
  
}



