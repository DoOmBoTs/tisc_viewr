# TISCViewR

TISCViewR is an R library built to work cooperatively with [TISC](https://github.com/danigeos/tisc), a landscape evolution and flexural modeling software written in C, for basin model analysis. TISCViewR is designed to call on both TISC input parameters and outputs files for analysis of each TISC model. TISCViewR calculates and outputs figures for basin geometry, basin symmetry, basin length, basin depth, and backstripping (e.g. decompaction, total subsidence, and tectonic subsidece). This allows the user to quickly perform deep analysis on many sets of models.

This research was supported by the National Science Foundation (EAR‐1824557).

## Instalation

```
devtools::install_gitlab(repo = "https://gitlab.com/Bryanrt-geophys/tisc_viewr")
```
