library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

# Using pre-generated raster files of future and historical predictions, 
# make figures of change over time

#==============================================================================


# Establish key data objects

east.africa <- load_country_map()

gcms <- c(
  "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", 
  "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
)

ssps <- c("ssp126", "ssp245", "ssp370")
ssps.upper <- toupper(ssps)

years <- c("2030", "2050", "2070")

#==============================================================================


# Generate delta plots showing differences between model predictions for 
# historical climates and all future climate scenarios

all.files <- list.files(
  path = "data/prediction_rasters",
  pattern = ".tif",
  full.names = TRUE
)

# Generate a 12-stack raster of monthly predictions for historical climate
files <- all.files[str_detect(all.files, "historical")]
assert_that(length(files) == 12)
hist.pred <- mask(rast(files), east.africa)

for(gcm in gcms) {
  
  print(gcm)
  
  for(ssp in ssps.upper) {
    
    print(ssp)
    
    for(year in years) {
      
      print(year)
      
      # Generate a 12-stack delta raster for this particular scenario
      files <- all.files[str_detect(all.files, paste(gcm, ssp, year, sep = "_"))]
      assert_that(length(files) == 12)
      delta <- mask(rast(files), east.africa) - hist.pred
      names(delta) <- month.name
      
      print(paste0("Minimum delta: ", min(minmax(delta))))
      print(paste0("Maximum delta: ", max(minmax(delta))))
      
      p <- ggplot() +
        geom_spatraster(data = delta) +
        geom_sf(data = east.africa, fill = NA) +
        scale_fill_gradient2(
          low = "darkblue", mid = "white", high = "darkred",
          na.value = "lightgray",
          limits = c(-0.45, 0.45)
        ) +
        ggtitle(paste(gcm, ssp, year, sep = ", ")) +
        theme_void() +
        facet_wrap(~lyr)
      
      ggsave(
        filename = paste0(
          "outputs/deltas/predictions_", 
          paste(gcm, ssp, year, sep = "_"),
          ".jpg"
        ),
        plot = p,
        height = 10,
        width = 10
      )
    }
  }
}

#==============================================================================


# Generate delta plots showing differences between model predictions for 
# historical climates and ensembled future climate scenarios

for(ssp in ssps.upper) {
  
  print(ssp)
  
  for(year in years) {
    
    print(year)
    
    files <- all.files[str_detect(all.files, paste(ssp, year, sep = "_"))]
    assert_that(length(files) == 12*11)
    
    ensemble <- tapp(
      mask(rast(files), east.africa), 
      index = rep(1:12, times = 11),
      fun = "mean"
    )
    
    delta <- ensemble - hist.pred
    names(delta) <- month.name
    
    print(paste0("Minimum delta: ", min(minmax(delta))))
    print(paste0("Maximum delta: ", max(minmax(delta))))
    
    p <- ggplot() +
      geom_spatraster(data = delta) +
      geom_sf(data = east.africa, fill = NA) +
      scale_fill_gradient2(
        low = "darkblue", mid = "white", high = "darkred",
        na.value = "lightgray",
        limits = c(-0.45, 0.45)
      ) +
      ggtitle(paste("GCM Ensemble", ssp, year, sep = ", ")) +
      theme_void() +
      facet_wrap(~lyr)
    
    ggsave(
      filename = paste0(
        "outputs/deltas/predictions_GCM_ensemble_", 
        paste(ssp, year, sep = "_"),
        ".jpg"
      ),
      plot = p,
      height = 10,
      width = 10
    )
  }
}

#==============================================================================


# Generate delta plots showing differences between precipitation for 
# historical climates and ensemble future climate scenarios

all.files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = ".tif",
  full.names = TRUE
)

# Generate a 12-stack raster of monthly precipitation for historical climate
files <- all.files[str_detect(all.files, "1970")]
assert_that(length(files) == 12)
hist.clim <- mask(rast(files), east.africa)

for(ssp in ssps) {
  
  print(ssp)
  
  for(year in years) {
    
    print(year)
    
    files <- all.files[str_detect(all.files, paste(ssp, year, sep = "_"))]
    assert_that(length(files) == 12*11)
    
    ensemble <- tapp(
      mask(rast(files), east.africa), 
      index = rep(1:12, times = 11),
      fun = "mean"
    )
    
    delta <- ensemble - hist.clim
    names(delta) <- month.name
    
    print(paste0("Minimum delta: ", min(minmax(delta))))
    print(paste0("Maximum delta: ", max(minmax(delta))))
    
    p <- ggplot() +
      geom_spatraster(data = delta) +
      geom_sf(data = east.africa, fill = NA) +
      scale_fill_gradient2(
        low = "darkred", mid = "white", high = "darkblue",
        na.value = "lightgray",
        limits = c(-120, 120)
      ) +
      ggtitle(paste("GCM Ensemble", ssp, year, sep = ", ")) +
      theme_void() +
      facet_wrap(~lyr)
    
    ggsave(
      filename = paste0(
        "outputs/deltas/precipitation_GCM_ensemble_", 
        paste(ssp, year, sep = "_"),
        ".jpg"
      ),
      plot = p,
      height = 10,
      width = 10
    )
  }
}

#==============================================================================


# Generate delta plots showing differences between tmax for 
# historical climates and ensemble future climate scenarios

all.files <- list.files(
  path = "data/rasters/temperature/processed",
  pattern = "tmax",
  full.names = TRUE
)

# Generate a 12-stack raster of monthly tmax for historical climate
files <- all.files[str_detect(all.files, "1970")]
assert_that(length(files) == 12)
hist.clim <- mask(rast(files), east.africa)

for(ssp in ssps) {
  
  print(ssp)
  
  for(year in years) {
    
    print(year)
    
    files <- all.files[str_detect(all.files, paste(ssp, year, sep = "_"))]
    assert_that(length(files) == 12*11)
    
    ensemble <- tapp(
      mask(rast(files), east.africa), 
      index = rep(1:12, times = 11),
      fun = "mean"
    )
    
    delta <- ensemble - hist.clim
    names(delta) <- month.name
    
    print(paste0("Minimum delta: ", min(minmax(delta))))
    print(paste0("Maximum delta: ", max(minmax(delta))))
    
    p <- ggplot() +
      geom_spatraster(data = delta) +
      geom_sf(data = east.africa, fill = NA) +
      scale_fill_gradient(
        low = "white", high = "darkred",
        na.value = "lightgray",
        limits = c(0, 4)
      ) +
      ggtitle(paste("GCM Ensemble", ssp, year, sep = ", ")) +
      theme_void() +
      facet_wrap(~lyr)
    
    ggsave(
      filename = paste0(
        "outputs/deltas/tmax_GCM_ensemble_", 
        paste(ssp, year, sep = "_"),
        ".jpg"
      ),
      plot = p,
      height = 10,
      width = 10
    )
  }
}
