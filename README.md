# Inter-epidemic Rift Valley fever in a changing climate

This repository contains code, data, and figures that support:

Eskew, E.A. E. Clancey, D. Singh, S. Situma, L. Nyakarahuka, M. K. Njenga, and S. L. Nuismer. Predicting climate change impacts on inter-epidemic risk of Rift Valley fever across East Africa.

---

### Predictor Variables

Models of inter-epidemic Rift Valley fever (RVF) relied on a suite of spatially-explicit predictor variables. All predictors were ultimately processed to a resolution of 2.5 arcminutes, but here we provide details about the sourcing and native resolution of all predictors:

- Hydrology

    - Lake data from [HydroLAKES](https://www.hydrosheds.org/products/hydrolakes) (shapefile of lakes globally)

    - River data from [HydroRIVERS](https://www.hydrosheds.org/products/hydrorivers) (shapefile of rivers globally)

- Soils

    - Multiple variables from [SoilGrids](https://soilgrids.org/) (250 m resolution [~8 arseconds])

- Topography

    - Elevation data from [SRTM](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1) (1 arcsecond resolution)

    - Slope was calculated using the elevation data described above

- Disease detection

    - Travel time to healthcare data from [Weiss et al. 2020, *Nature Medicine*](https://www.nature.com/articles/s41591-020-1059-1) (30 arcsecond resolution)

- Livestock density

    - Cattle, goat, and sheep density data from [Gridded Livestock of the World version 4](https://dataverse.harvard.edu/dataverse/glw) (5 arcminute resolution)

- Human population density

    - Historical human population data from [WorldPop](https://hub.worldpop.org/) (30 arcsecond resolution)

    - Projected human population data from [Wang et al. 2022, *Scientific Data*](https://www.nature.com/articles/s41597-022-01675-x) (30 arcsecond resolution)

- Precipitation and temperature

    - Historical weather data from [WorldClim](https://www.worldclim.org/data/monthlywth.html) (downscaled CRU-TS-4.06 data at 2.5 arcminute resolution)

    - Historical climate data from [WorldClim](https://www.worldclim.org/data/worldclim21.html)

    - Projected climate data from [WorldClim](https://www.worldclim.org/data/cmip6/cmip6climate.html) (downscaled output from various CMIP6 models at 2.5 arcminute resolution)

--- 

### Project Workflow

To help explain the project scripts, the overall workflow is as follows:

#### Gather predictor variables

    - [`get_SoilGrids_data.R`](/scripts/get_SoilGrids_data.R) programmatically downloads the soil predictor data. All other predictor data were manually downloaded from the online resources described above

#### Process predictor variables

    - [`process_all_predictors.R`](/scripts/process_all_predictors.R) processes all predictor data into rasters of 2.5 arcminute resolution. This script calls the various `process_*_data.R` scripts that each handle a certain type of predictor data. Note that these scripts do need to be called in the order prescribed by `process_all_predictors.R` so that intermediate files are available, as needed

    - [`generate_predictor_flat_files.R`](/scripts/generate_predictor_flat_files.R) takes the 2.5 arcminute raster predictor files and generates flat CSV files describing the predictor data for each grid cell across the study region. Predictor data in this format are necessary for downstream modeling. Note that these flat predictor files are generated for both historical and future climate conditions

#### Prepare data for modeling

    - [`generate_absence_data.R`](/scripts/generate_absence_data.R) generates the background (i.e., pseudo-absence) data for use in inter-epidemic RVF modeling

    - [`extract_outbreak_absence_predictors.R`](/scripts/extract_outbreak_absence_predictors.R) uses the predictor flat files to generate a data frame with predictor data for all observed inter-epidemic RVF outbreak events as well as the background points

#### Modeling of inter-epidemic RVF

    - [`fit_model.R`](/scripts/fit_model.R) fits and saves an XGBoost model of the disease outbreak and background data

#### Model post-processing and validation

#### Plotting