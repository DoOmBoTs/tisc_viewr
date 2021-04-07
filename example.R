filePaths <- model_paths(wanted_files = c("pfl", "UNIT", "PRM"),
                       root_path = here("sample_data/foreland_basin"))

modelNames <- model_names(file_paths = filePaths)

modelsBasinData <- wrangle_pfl(file_paths = filePaths,
                                 model_names = modelNames)

modelsWellLocation <- models_well_locaton(models_basin_data = modelsBasinData)

modelsBasinGerometry <- models_basin_geometry(models_basin_data = modelsBasinData,
                                              model_paths = filePaths)

models_basin_symmetry_data <- compute_basin_symmetry_data(models_basin_data = ,
                                                          file_paths = )

models_well_tops <- get_well_tops(models_well_location = ,
                                  file_paths = ,
                                  model_names = )

decompaction_data <- get_decompaction_data(models_well_tops = ,
                                           file_paths = )

subsidence_data <- get_subsidence_data(decompaction_data = ,
                                       file_paths = )

basin_plots_facets <- make_basin_plots_small_multiples(models_basin_data = ,
                                                       vis_dir = )

basin_plots_facets_with_midpoint <- make_basin_plots_small_multiples_with_midpoint(models_basin_data = ,
                                                                                   vis_dir = )

basin_length_plots_facets <- plot_basin_length(models_basin_geometry = ,
                                               vis_dir = )

basin_depth_plots_facets <- plot_basin_depth(models_basin_geometry = ,
                                             vis_dir = )

basin_symmetry_plots <- plot_basin_symmetry_data(models_basin_symmetry_data = ,
                                                 vis_dir = )

geohistory_curves <- plot_geohistory_curves(subsidence_data = ,
                                            vis_dir = )

basin_plotly <- make_basin_plotly(models_basin_data = ,
                                  vis_dir = )


