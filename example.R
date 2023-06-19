devtools::install_github(repo = "https://github.com/DoOmBoTs/tisc_viewr.git")

fp <- TISCViewR::model_paths(wanted_files = c("pfl", "PRM", "UNIT", "SLV"),
            root_path = here::here("sample_data/"))

mn <- TISCViewR::model_names(file_paths = fp)

basinData <- TISCViewR::wrangle_pfl(file_paths = fp,
                         model_names = mn)

# spelling error with location, change in function as well
location <- TISCViewR::models_well_locaton(models_basin_data = basinData)

tops <- TISCViewR::get_well_tops(models_well_location = location,
                      file_paths = fp,
                      model_names = mn)

basinGeometry <- TISCViewR::get_basin_geometry(models_basin_data = basinData,
                                     model_paths = fp)

# if SLV is a passed file type, an empty list is returned
decompaction <- TISCViewR::get_decompaction_data(models_well_tops = tops,
                                      model_paths = fp,
                                      model_names = mn)

# class definition in function says it is of a different length than the rest,
# though it doesn't appear to be. I commented out this section temporarily.
symmetryData <- TISCViewR::compute_basin_symmetry_data(models_basin_data = basinData,
                                            file_paths = fp)

subsidence <- TISCViewR::get_subsidence_data(decompaction_data = decompaction,
                                  file_paths = fp,
                                  model_names = mn)

# faceting is currently set to class but the variable does not exist from upstream errors
# faceting by model works fine
# commented out hline portion. may consider adding conditional logic and variable that lets
# the user define the hline and what to facet by
TISCViewR::plot_basin_depth(models_basin_geometry = basinGeometry,
                 vis_dir = here::here("sample_data"))

# faceting is currently set to class but the variable does not exist from upstream errors
# faceting by model works fine
# may consider adding conditional logic and variable that lets he user define the facet
TISCViewR::plot_basin_symmetry_data(models_basin_symmetry_data = symmetryData,
                         vis_dir = here::here("sample_data"))

# faceting is currently set to class but the variable does not exist from upstream errors
# faceting by model works fine
# may consider adding conditional logic and variable that lets he user define the facet
TISCViewR::plot_basin_length(models_basin_geometry = basinGeometry,
                  vis_dir = here::here("sample_data"))

# need to make minor adjustments upstream in subsidence to prevent water depth from being
# measured above zero
TISCViewR::plot_geohistory_curves(subsidence_data = subsidence,
                       vis_dir = here::here("sample_data"))


TISCViewR::make_basin_plotly(models_basin_data = basinData,
                  vis_dir = here::here("sample_data"))

# I don't see a plot being created where I am telling it to be created
# TISCViewR::make_basin_plots_small_multiples_with_midpoint(models_basin_data = basinData,
                                               # vis_dir = here("sample_data"))

# I don't see a plot being created where I am telling it to be created
# TISCViewR::make_basin_plots_small_multiples(models_basin_data = basinData,
                                 # vis_dir = here::here("sample_data"))

# add a dir_create to this function to put all of the individual plots in folders per model?
TISCViewR::make_basin_plots_individual(models_basin_data = basinData,
                            vis_dir = here::here("sample_data"))
