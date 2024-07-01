library(tidyverse)
library(terra)
library(tidyterra)

source("R/functions.R")

#==============================================================================


# Load in the travel time to healthcare raster data and resample to match other
# processed raster data

# Import the raster
r <- rast("data/rasters/healthcare/2020_motorized_travel_time_to_healthcare.geotiff")

# Reproject the raster to EPSG 4326, if needed
if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}

# Load another processed predictor raster for resampling
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Resample to the same resolution and extent
r.resample <- resample(r, x, "bilinear")

# Save the resampled raster file
if(!dir.exists("data/rasters/healthcare/processed")) {
  dir.create("data/rasters/healthcare/processed")
}

writeRaster(
  r.resample, 
  paste0(
    "data/rasters/healthcare/processed/", 
    "healthcare_2.5min.tif"
  ),
  overwrite = TRUE
)

#==============================================================================


# Load in the resampled travel time to healthcare raster data and 
# East Africa map
r <- rast("data/rasters/healthcare/processed/healthcare_2.5min.tif")
east.africa <- load_country_map()


# Plot and save the travel time to healthcare raster data
p <- ggplot() +
  geom_spatraster(data = r) +
  geom_sf(data = east.africa, fill = NA, color = "black") +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/travel_time_to_healthcare.jpg",
  width = 1000,
  height = 1000,
  units = "px"
)
