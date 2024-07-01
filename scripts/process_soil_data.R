library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# Load in the raw soil raster data and resample to match other
# processed raster data

files <- list.files(
  path = "data/rasters/soil/SoilGrids",
  full.names = TRUE
)

# Import the rasters
r <- rast(files)

# Reproject the raster to EPSG 4326, if needed
if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}

# Load another processed predictor raster for resampling
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Resample to the same resolution and extent
r.r <- resample(r, x, "bilinear")

# Rescale all soil variables appropriately
# Information provided here: https://www.isric.org/explore/soilgrids/faq-soilgrids
r.r[[which(str_detect(names(r.r), "bdod|nitrogen"))]] <- 
  r.r[[which(str_detect(names(r.r), "bdod|nitrogen"))]] / 100
r.r[[which(str_detect(names(r.r), "cfvo|clay|phh2o|sand|silt|soc"))]] <- 
  r.r[[which(str_detect(names(r.r), "cfvo|clay|phh2o|sand|silt|soc"))]] / 10

# Save the resampled, rescaled raster file
if(!dir.exists("data/rasters/soil/processed")) {
  dir.create("data/rasters/soil/processed")
}

writeRaster(
  r.r, 
  paste0("data/rasters/soil/processed/soil_2.5min.tif"),
  overwrite = TRUE
)

#==============================================================================


# Load in the resampled soil data
r <- rast("data/rasters/soil/processed/soil_2.5min.tif")
east.africa <- load_country_map()


# Loop through the soil variables and plot

variable <- c("bdod", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

for(v in variable) {

  r.sub <- r %>%
    select(matches(v))
  
  p <- ggplot() +
    geom_spatraster(data = r.sub) +
    geom_sf(data = east.africa, fill = NA, color = "black") +
    scale_fill_distiller(palette = "Spectral", na.value = "white") +
    facet_wrap(~lyr, ncol = 3) +
    theme_void()
  
  ggsave(
    p,
    filename = paste0("outputs/predictor_layers/soil_", v, ".jpg"),
    width = 4000,
    height = 2000,
    units = "px"
  )
}
