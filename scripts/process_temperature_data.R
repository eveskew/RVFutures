library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# For all files, load in the temperature raster data and crop to the
# relevant country extents

east.africa <- load_country_map()

files <- list.files(
  path = "data/rasters/temperature/WorldClim"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/temperature/WorldClim/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names(crop) <- str_replace(i, "\\.tif", "")
  
  # Save the cropped raster
  if(!dir.exists("data/rasters/temperature/processed")) {
    dir.create("data/rasters/temperature/processed")
  }
  
  writeRaster(
    crop, 
    paste0("data/rasters/temperature/processed/", i),
    overwrite = TRUE
  )
}

#==============================================================================


# Load in the cropped temperature raster data, effectively getting a 
# raster stack with each month's data as a layer
variables <- c("tmax", "tmin")

for(var in variables) {
  
  files <- list.files(
    path = "data/rasters/temperature/processed",
    pattern = "wc.*200[8-9]|wc.*201[0-9]|wc.*202[0-3]",
    full.names = TRUE
  )
  files <- files[str_detect(files, var)]
  
  r <- rast(files)
  assert_that(dim(r)[3] == 14 * 12)
  
  # Plot and save the temperature raster data
  p <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_distiller(palette = "Spectral", na.value = "gray", limits = c(-20, 60)) +
    facet_wrap(~lyr, ncol = 12) +
    theme_void()
  
  if(!dir.exists("outputs/predictor_layers")) {
    dir.create("outputs/predictor_layers")
  }
  
  ggsave(
    p,
    filename = paste0("outputs/predictor_layers/", var, ".jpg"),
    width = 8000,
    height = 14000,
    units = "px"
  )
}

#==============================================================================


# Generate future projections of temperature on a by-month basis using the
# existing data and a linear projection method
months <- c(
  "01", "02", "03", "04", "05", "06", 
  "07", "08", "09", "10", "11", "12"
)

first.data.year <- 2000

variables <- c("tmax", "tmin")

for(var in variables) {
  
  files <- list.files(
    path = "data/rasters/temperature/processed",
    pattern = var,
    full.names = TRUE
  )
  
  r <- rast(files)
  assert_that(dim(r)[3] == 22 * 12)
  
  for(month in months) {
    
    # Subset the raster stack to only one particular month across time
    r.sub <- r %>%
      select(matches(paste0("-", month)))
    
    # Regress across the years
    x <- regress(r.sub, 1:nlyr(r.sub), na.rm = TRUE)
    
    # Generate predictor layers for 2024-2050 and save them
    for(year in 2024:2050) {
      
      writeRaster(
        generate_raster_projection(
          regression_raster = x,
          first_year_of_data = first.data.year,
          projection_year = year,
          lower_clamp = -20,
          upper_clamp = 80,
          layer_name = paste0("linear_projection_", var, "_", year, "-", month)
        ), 
        paste0("data/rasters/temperature/processed/linear_projection_2.5min_", var, "_", year, "-", month, ".tif"),
        overwrite = TRUE
      )
    }
  }
}

#==============================================================================


# Load in the cropped temperature raster data, effectively getting a 
# raster stack with each month's data as a layer
variables <- c("tmax", "tmin")

for(var in variables) {
  
  obs.files <- list.files(
    path = "data/rasters/temperature/processed",
    pattern = paste0("wc2.1_2.5m_", var),
    full.names = TRUE
  )
  proj.files <- list.files(
    path = "data/rasters/temperature/processed/",
    pattern = paste0("linear_projection_2.5min_", var),
    full.names = TRUE
  )
  files <- c(obs.files, proj.files)
  
  r <- rast(files)
  assert_that(dim(r)[3] == 49 * 12)
  
  r.sub <- r %>%
    select(matches("2000|2010|2020|2030|2040|2050"))
  
  # Plot and save the temperature raster data
  p <- ggplot() +
    geom_spatraster(data = r.sub) +
    scale_fill_distiller(palette = "Spectral", na.value = "gray", limits = c(-20, 60)) +
    facet_wrap(~lyr, ncol = 12) +
    theme_void()
  
  ggsave(
    p,
    filename = paste0("outputs/predictor_layers/", var, "_projected.jpg"),
    width = 6000,
    height = 6000,
    units = "px"
  )
}

#==============================================================================


# Load in the GCM temperature raster data and crop to the relevant 
# country extents

files <- list.files(
  path = "data/rasters/temperature/GCMs"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/temperature/GCMs/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names <- i %>%
    str_replace("\\.tif", "") %>%
    str_replace("2021-2040", "2030") %>%
    str_replace("2041-2060", "2050")
  names <- paste0(names, "-", months)
  names(crop) <- names
  
  # Save the cropped rasters
  for(name in names) {
    
    writeRaster(
      crop[[name]], 
      paste0("data/rasters/temperature/processed/", name, ".tif"),
      overwrite = TRUE
    )
  }
}
