library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# Load in the raw African elevation raster data and resample to match other
# processed raster data

files <- list.files(
  path = "data/rasters/elevation/SRTM",
  pattern = ".tif$",
  full.names = TRUE
)

# Merge the raster files into a VRT
vrt <- vrt(files)

# Reproject the raster to EPSG 4326, if needed
if(crs(vrt, describe = TRUE)$code != "4326" | is.na(crs(vrt, describe = TRUE)$code)) {vrt <- project(x = vrt, y = "epsg:4326")}

# Create a slope raster from the elevation VRT
slope <- terrain(vrt, v = "slope")

# Load another processed predictor raster for resampling
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Resample to the same resolution and extent
vrt.resample <- resample(vrt, x, "bilinear")
slope.resample <- resample(slope, x, "bilinear")

# Save the merged, resampled raster file
if(!dir.exists("data/rasters/elevation/processed")) {
  dir.create("data/rasters/elevation/processed")
}

writeRaster(
  vrt.resample, 
  paste0("data/rasters/elevation/processed/elevation_2.5min.tif"),
  overwrite = TRUE
)

writeRaster(
  slope.resample, 
  paste0("data/rasters/elevation/processed/slope_2.5min.tif"),
  overwrite = TRUE
)

#==============================================================================


# Load in the resampled elevation and slope data and East Africa map
e <- rast("data/rasters/elevation/processed/elevation_2.5min.tif")
s <- rast("data/rasters/elevation/processed/slope_2.5min.tif")
east.africa <- load_country_map()


# Plot and save the elevation raster data
p <- ggplot() +
  geom_spatraster(data = e) +
  geom_sf(data = east.africa, fill = NA, color = "black") +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/elevation.jpg",
  width = 1000,
  height = 1000,
  units = "px"
)

# Plot and save the slope raster data
p <- ggplot() +
  geom_spatraster(data = s) +
  geom_sf(data = east.africa, fill = NA, color = "black") +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  theme_void()

ggsave(
  p,
  filename = "outputs/predictor_layers/slope.jpg",
  width = 1000,
  height = 1000,
  units = "px"
)
 