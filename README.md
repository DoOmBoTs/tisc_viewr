# TISCViewR

TISCViewR is an R package built to work cooperatively with [TISC](https://github.com/danigeos/tisc), a landscape evolution and flexural modeling software written in C, or geological/geophysical basin model analysis. TISCViewR is designed to call on both TISC input parameters and outputs files to use for analysis of each TISC model. TISCViewR calculates and outputs figures for basin geometry, basin symmetry, basin length, basin depth, and backstripping (e.g. decompaction, total subsidence, and tectonic subsidence). This allows the user to quickly perform deep analysis on many sets of models.

## Acknowledgements
This research was supported by the National Science Foundation (EAR‐1824557).

To get a glimpse of the visualizations output by TISCViewR see the [visualizations directory](visualizations).

## Getting Started
### Instalation

This package can currently be pulled directly from GitLab or installed to your R environment using the devtools package, as shown below.
```
devtools::install_github(repo = "https://gitlab.com/Bryanrt-geophys/tisc_viewr")
```
### Necessary TISC Edits
To use TISCViewR, a minor edit must be made to how TISC overwrites some of its output files at each timestep. After downloading TISC, navigate to the downloaded directory, open the src directory, and open the tiscio.c file. Find the `int write_file_cross_section()` function in the file (cmd+f/cntrl+f) and insert the following lines at the end of the function just above `return 1;`:

```
/*****************************************************************************************
edits to iterate pfl file's
*****************************************************************************************/
 
     char      command[300];
     sprintf(command, "cp %s.pfl %s_%03d.pfl", projectname, projectname, n_image);
               system(command);
/*****************************************************************************************
edits to iterate pfl file's
*****************************************************************************************/
```
When TISC is ran with this edit, the .pfl file (which contains transect data) will be copied at each timestep and renamed iteratively for the number of timesteps that take place in the model. This will provide the data frames necessary to perform the analysis.

TISCViewR is currently designed to look through a designated directory path that is adaptable to any machine due to the leveraging of R's [here package](https://cran.r-project.org/web/packages/here/index.html). Despite this, minor directory structuring is necessary if the user wants to avoid changing the code (see copy_files.R in the next section). TISCViewR will detect all TISC models within the designated directory path `Tisc_models/<model case name>/<model name>`. TISCViewR will not currently look deeper than two subdirectories for model data. TISCViewR uses the end of the directory path, `<model name>` to name listed data and facete plots appropriately. Keep this in mind when naming the folders that will house your model data. It is good practice to names the directory in a fashion that explicitely states the unique attribute to that model (e.g. `Tisc_models/foreland_basin/75km_EET`, `Tisc_models/foreland_basin/25km_EET`, etc.).  
