library(tidyverse)
library(terra)
library(sf)
library(assertthat)

source("R/functions.R")

# Script to generate predictor flat files. These will contain predictor values 
# for each grid cell on a 2.5 arcminute grid (dim = 413 rows, 301 columns). To 
# save space and time, different predictor types will be processed and stored
# in the smallest size possible

#==============================================================================


# Generate flat file for static predictors (so only need grid cells)

n.cells <- 413 * 301

d <- data.frame(
  grid_cell = 1:n.cells
)


# Add hydrology data to the data frame

r <- rast("data/rasters/hydrology/processed/dist_to_lake_all_2.5min.tif")
d$dist_to_lake_all <- as.numeric(values(r))
sum(is.na(d$dist_to_lake_all))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_1_2.5min.tif")
d$dist_to_lake_1 <- as.numeric(values(r))
sum(is.na(d$dist_to_lake_1))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_5_2.5min.tif")
d$dist_to_lake_5 <- as.numeric(values(r))
sum(is.na(d$dist_to_lake_5))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_10_2.5min.tif")
d$dist_to_lake_10 <- as.numeric(values(r))
sum(is.na(d$dist_to_lake_10))

r <- rast("data/rasters/hydrology/processed/dist_to_river_10_2.5min.tif")
d$dist_to_river_10 <- as.numeric(values(r))
sum(is.na(d$dist_to_river_10))


# Add soil data to the data frame

r <- rast("data/rasters/soil/processed/soil_2.5min.tif")

d$`bdod_0-5cm_mean` <- as.numeric(values(r$`bdod_0-5cm_mean`))
sum(is.na(d$`bdod_0-5cm_mean`))

d$`cfvo_0-5cm_mean` <- as.numeric(values(r$`cfvo_0-5cm_mean`))
sum(is.na(d$`cfvo_0-5cm_mean`))

d$`clay_0-5cm_mean` <- as.numeric(values(r$`clay_0-5cm_mean`))
sum(is.na(d$`clay_0-5cm_mean`))

d$`nitrogen_0-5cm_mean` <- as.numeric(values(r$`nitrogen_0-5cm_mean`))
sum(is.na(d$`nitrogen_0-5cm_mean`))

d$`phh2o_0-5cm_mean` <- as.numeric(values(r$`phh2o_0-5cm_mean`))
sum(is.na(d$`phh2o_0-5cm_mean`))

d$`sand_0-5cm_mean` <- as.numeric(values(r$`sand_0-5cm_mean`))
sum(is.na(d$`sand_0-5cm_mean`))

d$`silt_0-5cm_mean` <- as.numeric(values(r$`silt_0-5cm_mean`))
sum(is.na(d$`silt_0-5cm_mean`))

d$`soc_0-5cm_mean` <- as.numeric(values(r$`soc_0-5cm_mean`))
sum(is.na(d$`soc_0-5cm_mean`))


# Add elevation and slope data to the data frame

r <- rast("data/rasters/elevation/processed/elevation_2.5min.tif")
d$elevation <- as.numeric(values(r))
sum(is.na(d$elevation))

r <- rast("data/rasters/elevation/processed/slope_2.5min.tif")
d$slope <- as.numeric(values(r))
sum(is.na(d$slope))


# Add travel time to healthcare data to the data frame

r <- rast("data/rasters/healthcare/processed/healthcare_2.5min.tif")
d$travel_time_to_healthcare <- as.numeric(values(r))
sum(is.na(d$travel_time_to_healthcare))


# Add animal density data to the data frame

# Load in cattle density file
r <- rast("data/rasters/livestock/processed/Ct_2015_Aw_density_2.5min.tif")
d$cattle_density <- as.numeric(values(r))
sum(is.na(d$cattle_density))

# Load in goat density file
r <- rast("data/rasters/livestock/processed/Gt_2015_Aw_density_2.5min.tif")
d$goat_density <- as.numeric(values(r))
sum(is.na(d$goat_density))

# Load in sheep density file
r <- rast("data/rasters/livestock/processed/Sh_2015_Aw_density_2.5min.tif")
d$sheep_density <- as.numeric(values(r))
sum(is.na(d$sheep_density))


# Write static predictor data to disk

write_csv(d, "data/predictor_flat_files/static_predictors.csv")

#==============================================================================


# Generate flat file for yearly predictors (so need grid cells and years)

n.cells <- 413 * 301
years <- 2008:2022
n.years <- length(years)

d <- data.frame(
  year = rep(years, each = n.cells),
  grid_cell = rep(1:n.cells, times = n.years)
)


# Add human population data to the data frame

# Load in human population raster files
files <- list.files(
  path = "data/rasters/human_population/processed",
  full.names = TRUE
)

files

r <- rast(files)

d$human_pop <- NA

for(year in years) {
  
  # Subset to the correct raster layer
  # Use SSP245 data for years after 2020
  if(year <= 2020) {layer <- r[[paste0("pd_", year)]]}
  if(year > 2020) {layer <- r[[paste0("SSP245_", year)]]}
  
  # Extract values
  d[d$year == year, "human_pop"] <- as.numeric(values(layer))
}

sum(is.na(d$human_pop))


# Add land cover data to the data frame

# Load in land cover raster files
files <- list.files(
  path = "data/rasters/land_cover/processed",
  full.names = TRUE
)

files

r <- rast(files)

lc.variables <- c(
  "c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr", "primf", 
  "primn", "range", "secdf", "secdn", "secma", "secmb", "urban"
)

df <- data.frame(matrix(nrow = nrow(d), ncol = length(lc.variables)))
colnames(df) <- lc.variables
d <- cbind(d, df)

for(year in years) {
  
  # Subset to the correct raster layer for the focal year
  # Use SSP245 data for years after 2015
  if(year <= 2015) {r.sub <- tidyterra::select(r, matches(paste0("^", year)))}
  if(year > 2015) {r.sub <- tidyterra::select(r, matches(paste0("SSP245_", year)))}
  
  assert_that(length(names(r.sub)) == length(lc.variables))

  for(var in lc.variables) {
    
    print(c(year, var))
    
    # Subset to the correct raster layer for the focal variable
    layer <- tidyterra::select(r.sub, matches(var))
    
    # Extract values
    d[d$year == year, var] <- as.numeric(values(layer))
  }
}

sum(is.na(d$c3ann))


# Write yearly predictor data to disk

write_csv(d, "data/predictor_flat_files/yearly_predictors_historical.csv")

#==============================================================================


# Generate flat file for monthly predictors (so need grid cells, years, and
# months)

n.cells <- 413 * 301
years <- 2007:2022
n.years <- length(years)
n.months <- 12

d <- data.frame(
  year = rep(years, each = n.cells * n.months),
  month = rep(rep(month.name, each = n.cells), times = n.years),
  grid_cell = rep(1:n.cells, times = n.years * n.months)
  
)


# Add NDVI data to the data frame

# Load in NDVI raster files
# files <- list.files(
#   path = "data/rasters/NDVI/processed",
#   pattern = paste(years, collapse = "|"),
#   full.names = TRUE
# )
# 
# files
# 
# r <- rast(files)
# 
# d$monthly_NDVI <- as.numeric(values(r))
# 
# for(year in unique(d$year)) {
#   
#   for(month in month.name) {
#     
#     print(c(year, month))
#   
#     # Subset to the correct raster layer
#     if(year <= 2023) {layer <- r[[paste0("Monthly_NDVI_", year, "_", month.table[month])]]}
#     if(year > 2023) {layer <- r[[paste0("projected_", year, "_", month.table[month])]]}
#     
#     # Extract values
#     d[d$year == year & d$month == month, "monthly_NDVI"] <- as.numeric(values(layer))
#   }
# }


# Add precipitation data to the data frame

# Load in precipitation raster files
files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = paste(years, collapse = "|"),
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_precip <- NA

for(year in 2007:2021) {
  
  for(month in month.name) {
    
    print(c(year, month))
    
    # Subset to the correct raster layer
    layer <- r[[paste0("wc2.1_2.5m_prec_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_precip"] <- as.numeric(values(layer))
  }
}


# Add temperature data to the data frame

# Load in temperature raster files
files <- list.files(
  path = "data/rasters/temperature/processed",
  pattern = paste(years, collapse = "|"),
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_tmax <- NA
d$monthly_tmin <- NA

for(year in 2007:2021) {
  
  for(month in month.name) {
    
    print(c(year, month))
    
    # Subset to the correct raster layer for tmax
    layer <- r[[paste0("wc2.1_2.5m_tmax_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_tmax"] <- as.numeric(values(layer))
    
    # Subset to the correct raster layer for tmin
    layer <- r[[paste0("wc2.1_2.5m_tmin_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_tmin"] <- as.numeric(values(layer))
  }
}


# Calculate lagged values for monthly variables

d <- d %>%
  # convert month to a factor variable for sorting
  mutate(month = factor(month, levels = month.name)) %>%
  # arrange by grid cell, year, and month to get a time series of each
  # cell's values
  arrange(grid_cell, year, month) %>%
  group_by(grid_cell) %>%
  # perform lagged variable calculations on the per-cell level
  mutate(
    monthly_tmax_lag_1 = lag(monthly_tmax, n = 1),
    monthly_tmax_lag_2 = lag(monthly_tmax, n = 2),
    monthly_tmax_lag_3 = lag(monthly_tmax, n = 3),
    
    monthly_tmin_lag_1 = lag(monthly_tmin, n = 1),
    monthly_tmin_lag_2 = lag(monthly_tmin, n = 2),
    monthly_tmin_lag_3 = lag(monthly_tmin, n = 3),
    
    monthly_precip_lag_1 = lag(monthly_precip, n = 1),
    monthly_precip_lag_2 = lag(monthly_precip, n = 2),
    monthly_precip_lag_3 = lag(monthly_precip, n = 3),
    
    cum_precip_3_months_prior = 
      monthly_precip_lag_1 + monthly_precip_lag_2 + monthly_precip_lag_3
  )


# Write monthly predictor data to disk

write_csv(d, "data/predictor_flat_files/monthly_predictors_historical.csv")
