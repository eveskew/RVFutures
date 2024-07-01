library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")
unlink("data/rasters/livestock/processed", recursive = TRUE)

#==============================================================================


# For cattle, goats, and sheep, load in the raster data and crop to the 
# relevant country extents. Also need to resample the data to 2.5 minute
# resolution. This involves converting the count data to density maps then 
# doing the resampling.

east.africa <- load_country_map()
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

if(!dir.exists("data/rasters/livestock/processed")) {
  dir.create("data/rasters/livestock/processed")
}

pattern <- c(
  "Ct_2015_Da", "Ct_2015_Aw",
  "Gt_2015_Da", "Gt_2015_Aw",
  "Sh_2015_Da", "Sh_2015_Aw"
)

for(p in pattern) {
  
  file <- list.files(
    path = "data/rasters/livestock",
    pattern = paste0(p, ".tif"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  r <- rast(file)
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  rr <- resample_count_raster(
    count_raster = r,
    crop_extent = east.africa,
    raster_for_resampling = x
  )
  
  writeRaster(
    rr, 
    paste0("data/rasters/livestock/processed/", p, "_density_2.5min.tif"),
    overwrite = TRUE
  )
}

#==============================================================================


# Load in the cropped livestock raster data (Da layers), effectively getting a 
# raster stack with each species' data as a layer
files <- list.files(
  path = "data/rasters/livestock/processed",
  pattern = "Da",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 3)

# Plot and save the livestock raster data
p <- ggplot() +
  geom_spatraster(data = r) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, nrow = 1) +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/livestock_Da.jpg",
  width = 3000,
  height = 1500,
  units = "px"
)


# Load in the cropped livestock raster data (Aw layer), effectively getting a 
# raster stack with each species' data as a layer
files <- list.files(
  path = "data/rasters/livestock/processed",
  pattern = "Aw",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 3)

# Plot and save the livestock raster data
p <- ggplot() +
  geom_spatraster(data = r) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, nrow = 1) +
  theme_void()

ggsave(
  p,
  filename = "outputs/predictor_layers/livestock_Aw.jpg",
  width = 3000,
  height = 1500,
  units = "px"
)
