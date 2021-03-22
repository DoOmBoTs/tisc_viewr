#
# ~~ * ~~ UTILITY FUNCTIONS
#

compute_basin_volumes <- function(Y, elevation, middle){
  
  data.frame(
    left = sum(abs(elevation[Y < middle])),
    right = sum(abs(elevation[Y > middle]))
  )          
  
}



compute_basin_symmetry_data <- function(models_basin_data, pfl_files){
  
  imap(models_basin_data, function(tidy_pfl, y){
    
    midpoint_data <- tidy_pfl %>%
      group_by(timestep) %>%
      summarize(middle = median(Y, na.rm = TRUE))
    
    tidy_pfl %>%
      group_split(timestep) %>%
      map2_dfr(midpoint_data$middle, function(pfl, middle){
        
        data.frame(
          left = sum(abs(pfl$elevation[pfl$Y < middle])),
          right = sum(abs(pfl$elevation[pfl$Y > middle]))
        )   
        
      })
    
    tidy_pfl %>%
      group_nest(timestep) %>%
      mutate(
        midpoint = map_dbl(data, ~ median(.x$Y, na.rm = TRUE)),
        left = map2_dbl(data, midpoint, ~ sum(abs(.x$elevation[.x$Y < .y]))),
        right = map2_dbl(data, midpoint, ~ sum(abs(.x$elevation[.x$Y > .y]))),
        ratio = right / left,
        difference = right - left,
        model = y,
        class =  first(str_subset(string = pfl_files, pattern = model)) %>%
          dirname() %>%
          dirname() %>%
          basename() 
      ) 
      
  })
  
}
