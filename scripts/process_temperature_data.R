library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# Load in the temperature historical weather data and crop to the
# relevant country extents

east.africa <- load_country_map()

files <- list.files(
  path = "data/rasters/temperature/WorldClim_historical_weather"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/temperature/WorldClim_historical_weather/", i))
  
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


# Load in the temperature historical climate data and crop to the relevant 
# country extents

files <- list.files(
  path = "data/rasters/temperature/WorldClim_historical_climate"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/temperature/WorldClim_historical_climate/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names(crop) <- i %>% 
    str_replace("\\.tif", "") %>%
    str_replace("tmin_", "tmin_1970-2000-") %>%
    str_replace("tmax_", "tmax_1970-2000-")
  
  writeRaster(
    crop, 
    paste0("data/rasters/temperature/processed/", names(crop), ".tif"),
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
    pattern = "wc.*_200[8-9]|wc.*_201[0-9]|wc.*_202[0-3]",
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
    pattern = paste0("wc2.1_2.5m_", var, "_2"),
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
    pattern = paste0("wc2.1_2.5m_", var, "_2"),
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
    width = 8000,
    height = 6000,
    units = "px"
  )
}

#==============================================================================


# Load in the temperature future climate data and crop to the relevant 
# country extents

files <- list.files(
  path = "data/rasters/temperature/WorldClim_future_climate"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/temperature/WorldClim_future_climate/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names <- i %>%
    str_replace("\\.tif", "") %>%
    str_replace("2021-2040", "2030") %>%
    str_replace("2041-2060", "2050") %>%
    str_replace("2061-2080", "2070")
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

#==============================================================================


# Plot mean temperatures over time for different SSP scenarios

# Import historical observations and future projections
files <- list.files(
  path = "data/rasters/temperature/processed",
  full.names = TRUE
)
files <- files[!str_detect(files, "linear|1970-2000")]

r <- rast(files)

# Generate data frame to track changes in precipitation variables over time
d <- data.frame(
  variable = str_extract(files, "(?<=2.5m_)[a-z]{4}(?=_)"),
  year = as.numeric(str_extract(files, "[0-9]{4}")),
  month = rep(months, times = 242),
  type = ifelse(
    is.na(str_extract(files, "ssp[0-9]{3}")), 
    "Historical",
    str_extract(files, "ssp[0-9]{3}")
  ),
  gcm = str_extract(files, "(?<=tm[a-z]{2}_)[^,]+(?=_ssp)"),
  median_value = NA,
  mean_value = NA
)

# Fill in the data frame
values <- values(r)
d$median_value <- apply(values, 2, median, na.rm = TRUE)
d$mean_value <- apply(values, 2, mean, na.rm = TRUE)

# Plot mean tmin and tmax values over time
variables <- c("tmax", "tmin")
gcms <- d %>%
  distinct(gcm) %>%
  filter(!is.na(gcm)) %>%
  pull(gcm)

for(var in variables) {
  
  for(g in gcms) {
    
    p <- d %>%
      filter(variable == var) %>%
      filter(gcm == g | is.na(gcm)) %>%
      ggplot(aes(x = year, y = mean_value, group = type, color = type)) +
      geom_point() +
      geom_line(linewidth = 0.2) +
      geom_vline(xintercept = 2021, linetype = 2) +
      xlab("") +
      ylab("Mean across study region") +
      ylim(14, 34) +
      ggtitle(g) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.title = element_blank()
      ) +
      facet_wrap(~month)
    
    ggsave(
      p,
      filename = paste0("outputs/predictor_layers/", var, "_", g, ".jpg"),
      width = 3000,
      height = 2000,
      units = "px"
    ) 
  }
}
