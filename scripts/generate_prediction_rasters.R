library(tidyverse)
library(tidymodels)
library(terra)

source("R/functions.R")

# Given a saved XGBoost model object, generate predictions for focal months
# and years

#==============================================================================


# Import saved XGBoost model object

xgb.RVF.final <- readRDS("saved_objects/xgb.RVF.final.rds")
full.training.fit <- extract_workflow(xgb.RVF.final)

#==============================================================================


# Generate prediction rasters for all historical years of interest

# Import predictor files

static.predictors <- read_csv("data/predictor_flat_files/static_predictors.csv")
yearly.predictors <- read_csv("data/predictor_flat_files/yearly_predictors_historical.csv")
monthly.predictors <- read_csv("data/predictor_flat_files/monthly_predictors_historical_weather.csv")

# Loop through years

for(year in c(2008:2021)) {
  
  # Import a raster to serve as a template
  r <- terra::rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")
  
  # Generate a data frame to hold predictions for this year
  d <- data.frame(
    grid_cell = rep(1:(dim(r)[1] * dim(r)[2]), times = 12),
    year = rep(year, (dim(r)[1] * dim(r)[2]) * 12),
    month = rep(month.name, each = (dim(r)[1] * dim(r)[2]))
  )
  
  # Fill the data frame with predictor data for this year
  d <- d %>%
    left_join(., static.predictors, by = "grid_cell") %>%
    left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
    left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))
  
  obs <- nrow(d)
  prop.missing <- sapply(1:ncol(d), function(x) sum(is.na(d[,x]))/obs)
  assertthat::assert_that(max(prop.missing) < 0.16)
  
  d <- d %>%
    mutate(
      longitude = rep(NA, nrow(.)),
      latitude = rep(NA, nrow(.)),
      RVF_presence = rep(NA, nrow(.)),
      Hu_Cs = rep(NA, nrow(.)),
      month_numeric = rep(NA, nrow(.)),
      year_group = rep(NA, nrow(.)),
      testing_data = rep(NA, nrow(.))
    )
  
  # Generate predictions using the fit model
  d <- cbind(d, predict(full.training.fit, d, type = "prob"))
  
  # Generate raster templates for each month of this year
  r <- rep(r, 12)
  names(r) <- paste0(
    year, "_", 
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  )
  varnames(r) <- "RVF_suitability"
  
  # Fill each month's raster with predicted values and save
  for(i in 1:12) {
    
    values(r[[i]]) <- d[d$month == month.name[i], ".pred_1"]
    writeRaster(r[[i]], paste0("data/prediction_rasters/", names(r)[i], ".tif"), overwrite = TRUE)
  }
}

#==============================================================================


# Generate prediction rasters for historical climate

# Import predictor files

static.predictors <- read_csv("data/predictor_flat_files/static_predictors.csv")
yearly.predictors <- read_csv("data/predictor_flat_files/yearly_predictors_historical.csv")
monthly.predictors <- read_csv("data/predictor_flat_files/monthly_predictors_historical_climate.csv")

# Import a raster to serve as a template
r <- terra::rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Generate a data frame to hold predictions for this year
# (we'll use historical yearly predictors from the year 2000 to pair with our
# 1970-2000 historical climate data)
d <- data.frame(
  grid_cell = rep(1:(dim(r)[1] * dim(r)[2]), times = 12),
  year = rep(2000, (dim(r)[1] * dim(r)[2]) * 12),
  month = rep(month.name, each = (dim(r)[1] * dim(r)[2]))
)

# Fill the data frame with predictor data for this year
d <- d %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "month"))

obs <- nrow(d)
prop.missing <- sapply(1:ncol(d), function(x) sum(is.na(d[,x]))/obs)
assertthat::assert_that(max(prop.missing) < 0.16)

d <- d %>%
  mutate(
    longitude = rep(NA, nrow(.)),
    latitude = rep(NA, nrow(.)),
    RVF_presence = rep(NA, nrow(.)),
    Hu_Cs = rep(NA, nrow(.)),
    month_numeric = rep(NA, nrow(.)),
    year_group = rep(NA, nrow(.)),
    testing_data = rep(NA, nrow(.))
  )

# Generate predictions using the fit model
d <- cbind(d, predict(full.training.fit, d, type = "prob"))

# Generate raster templates for each month of this year
r <- rep(r, 12)
names(r) <- paste0(
  "historical_climate_1985_", 
  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
)
varnames(r) <- "RVF_suitability"

# Fill each month's raster with predicted values and save
for(i in 1:12) {
  
  values(r[[i]]) <- d[d$month == month.name[i], ".pred_1"]
  writeRaster(r[[i]], paste0("data/prediction_rasters/", names(r)[i], ".tif"), overwrite = TRUE)
}

#==============================================================================


# Generate prediction rasters for future scenarios

gcms <- c(
  "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", 
  "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
)
scenarios <- c("SSP126", "SSP245", "SSP370")

for(g in gcms) {
  
  print(g)
  
  for(s in scenarios) {
    
    print(s)

    # Loop through years
    
    for(year in c(2030, 2050, 2070)) {
      
      # Import a raster to serve as a template
      r <- terra::rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")
      
      # Generate a data frame to hold predictions for this year
      d <- data.frame(
        grid_cell = rep(1:(dim(r)[1] * dim(r)[2]), times = 12),
        year = rep(year, (dim(r)[1] * dim(r)[2]) * 12),
        month = rep(month.name, each = (dim(r)[1] * dim(r)[2]))
      )
      
      # Fill the data frame with predictor data for this year
      d <- d %>%
        left_join(
          ., 
          static.predictors, 
          by = "grid_cell"
        ) %>%
        left_join(
          ., 
          read_csv(paste0("data/predictor_flat_files/yearly_predictors_", s, ".csv")), 
          by = c("grid_cell", "year")
        ) %>%
        left_join(
          ., 
          read_csv(paste0("data/predictor_flat_files/monthly_predictors_", g, "_", s, ".csv")), 
          by = c("grid_cell", "year", "month")
        )
      
      obs <- nrow(d)
      prop.missing <- sapply(1:ncol(d), function(x) sum(is.na(d[,x]))/obs)
      assertthat::assert_that(max(prop.missing) < 0.16)
      
      d <- d %>%
        mutate(
          longitude = rep(NA, nrow(.)),
          latitude = rep(NA, nrow(.)),
          RVF_presence = rep(NA, nrow(.)),
          Hu_Cs = rep(NA, nrow(.)),
          month_numeric = rep(NA, nrow(.)),
          year_group = rep(NA, nrow(.)),
          testing_data = rep(NA, nrow(.))
        )
      
      # Generate predictions using the fit model
      d <- cbind(d, predict(full.training.fit, d, type = "prob"))
      
      # Generate raster templates for each month of this year
      r <- rep(r, 12)
      names(r) <- paste0(
        g, "_", s, "_", year, "_", 
        c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
      )
      varnames(r) <- "RVF_suitability"
      
      # Fill each month's raster with predicted values and save
      for(i in 1:12) {
        
        values(r[[i]]) <- d[d$month == month.name[i], ".pred_1"]
        writeRaster(r[[i]], paste0("data/prediction_rasters/", names(r)[i], ".tif"), overwrite = TRUE)
      }
    }
  }
}
