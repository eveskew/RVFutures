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


# Generate prediction rasters for all years of interest

# Import predictor files

static.predictors <- read_csv("data/predictor_flat_files/static_predictors.csv")
yearly.predictors <- read_csv("data/predictor_flat_files/yearly_predictors_historical.csv")
monthly.predictors <- read_csv("data/predictor_flat_files/monthly_predictors_historical.csv")

# Loop through years

for(year in c(2008:2022)) {
  
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
