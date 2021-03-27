fp <- model_paths(wanted_files = c("pfl", "PRM", "UNIT"),
            root_path = here("sample_data/foreland_basin"))

mn <- model_names(file_paths = fp)

basinData <- wrangle_pfl(file_paths = fp,
                         model_names = mn)

location <- models_well_locaton(models_basin_data = basinData)

tops <- get_well_tops(models_well_location = location,
                      file_paths = fp,
                      model_names = mn)

# class definition in function says it is of a different length than the rest,
# though it doesn't appear to be. I commented out this section temporarily.
basin_geometry <- get_basin_geometry(models_basin_data = basinData,
                                     model_paths = fp)

# returning an empty list and I am not sure why
decompaction <- get_decompaction_data(models_well_tops = tops,
                                      model_paths = fp)



