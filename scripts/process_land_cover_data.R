library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# For all years, load in MODIS land cover raster data and crop to 
# relevant country extents

east.africa <- load_country_map()

# years <- as.character(2003:2022)
# 
# # Loop through all years
# for(i in years) {
#   
#   # Pull raster file name for that year
#   file <- list.files(
#     path = "data/rasters/land_cover/MODIS", 
#     pattern = paste0("A", i),
#     full.names = TRUE
#   )
#   print(file)
#   
#   # Import the raster
#   r <- rast(file)
#   
#   # Reproject the raster to EPSG 4326, if needed
#   if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
#   
#   # Crop the raster
#   crop <- terra::crop(r, east.africa)
#   
#   # Save the cropped raster
#   if(!dir.exists("data/rasters/land_cover/processed")) {
#     dir.create("data/rasters/land_cover/processed")
#   }
#   writeRaster(
#     crop, 
#     paste0("data/rasters/land_cover/processed/", i, ".tif"),
#     overwrite = TRUE
#   )
# }

#==============================================================================


# For the years 2000-2015, load in LUH2 land cover raster data and crop to 
# relevant country extents

r <- rast("data/rasters/land_cover/LUH2/states.nc")
ext(r) <- c(-180, 180, -90, 90)
crs(r) <- "+proj=longlat +datum=WGS84"

# Pull data from 2000-2015 (the file contains 14 variables and 1116 years, those
# from from 850-2015)
indexes <- as.character(1151:1166)
pattern <- paste0(indexes, collapse = "|")

r.sub <- select(r, matches(pattern))

# Reproject the raster to EPSG 4326, if needed
if(crs(r.sub, describe = TRUE)$code != "4326" | is.na(crs(r.sub, describe = TRUE)$code)) {r.sub <- project(x = r.sub, y = "epsg:4326")}

# Crop the raster to minimize its size and set NA values
r.crop <- crop(r.sub, east.africa)
r.crop[r.crop > 1000000] <- NA

# Disaggregate the raster to 2.5 arcminutes and crop again to ensure proper
# final dimensions
# Original resolution is 15 arcminutes (0.25 degrees)
r.crop <- disagg(r.crop, fact = 6)
r.crop <- crop(r.crop, east.africa)

base.names <- unique(str_remove(names(r.crop), "_[1-9]..."))

# Loop through years and generate raster files with all 14 variables from each
# year
years <- as.character(2000:2015)

for(i in 1:length(years)) {
  
  temp <- r.crop %>%
    select(matches(indexes[i]))
  
  names(temp) <- paste0(years[i], "_", base.names)
  
  if(!dir.exists("data/rasters/land_cover/processed")) {
    dir.create("data/rasters/land_cover/processed")
  }
    
  writeRaster(
    temp, 
    paste0("data/rasters/land_cover/processed/", years[i], "_2.5min.tif"),
    overwrite = TRUE
  )
}

file.remove(
  list.files(
    path = "data/rasters/land_cover/processed",
    pattern = "json",
    full.names = TRUE
  )
)

#==============================================================================


# For the years 2016-2070, load in LUH2 land cover raster data and crop to 
# relevant country extents for different SSP scenarios

scenarios <- c("ssp126", "ssp245", "ssp585")

for(s in scenarios) {
  
  # Import the scenario's file
  file <- list.files(
    "data/rasters/land_cover/LUH2",
    pattern = s,
    full.names = TRUE
  )
  
  r <- rast(file)
  ext(r) <- c(-180, 180, -90, 90)
  crs(r) <- "+proj=longlat +datum=WGS84"
  
  # Pull data from 2016-2070 (the file contains 14 variables and 86 years, those
  # from from 2015-2100)
  indexes <- as.character(2:56)
  pattern1 <- paste0("_", indexes, "$")
  pattern2 <- paste0(pattern1, collapse = "|")
  
  r.sub <- select(r, matches(pattern2))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r.sub, describe = TRUE)$code != "4326" | is.na(crs(r.sub, describe = TRUE)$code)) {r.sub <- project(x = r.sub, y = "epsg:4326")}
  
  # Crop the raster to minimize its size and set NA values
  r.crop <- crop(r.sub, east.africa)
  r.crop[r.crop > 1000000] <- NA
  
  # Disaggregate the raster to 2.5 arcminutes and crop again to ensure proper
  # final dimensions
  # Original resolution is 15 arcminutes (0.25 degrees)
  r.crop <- disagg(r.crop, fact = 6)
  r.crop <- crop(r.crop, east.africa)
  
  # Loop through years and generate raster files with all 14 variables from each
  # year
  years <- as.character(2016:2070)
  
  for(i in 1:length(years)) {
    
    temp <- r.crop %>%
      select(matches(pattern1[i]))
    
    names(temp) <- paste0(toupper(s), "_", years[i], "_", base.names)
    
    writeRaster(
      temp, 
      paste0("data/rasters/land_cover/processed/", toupper(s), "_", years[i], "_2.5min.tif"),
      overwrite = TRUE
    )
  }
}

file.remove(
  list.files(
    path = "data/rasters/land_cover/processed",
    pattern = "json",
    full.names = TRUE
  )
)

#==============================================================================


# For all years of the cropped data, load in land cover raster data and 
# generate a new raster detailing the most likely land cover class for each
# pixel
  
years <- as.character(2000:2024)

# Loop through all years
for(i in years) {
  
  if(i < 2016) {
    # Pull raster file name for that year
    file <- list.files(
      path = "data/rasters/land_cover/processed", 
      pattern = i,
      full.names = TRUE
    )
  } else {
    # Pull SSP245 raster file name for that year
    file <- list.files(
      path = "data/rasters/land_cover/processed", 
      pattern = paste0("SSP245_", i),
      full.names = TRUE
    )
  }
  
  # Import the raster, excluding "secma" and "secmb" variables since these
  # are not fractional land cover variables
  r <- rast(file) %>%
    select(!matches("secma|secmb"))
  
  # Create a raster with the best land cover class for each pixel
  b <- which.max(r)
  names(b) <- i
  
  # Save the cropped raster
  if(!dir.exists("data/rasters/land_cover/best_class")) {
    dir.create("data/rasters/land_cover/best_class")
  }
  
  writeRaster(
    b, 
    paste0("data/rasters/land_cover/best_class/", i, ".tif"),
    overwrite = TRUE
  )
} 
  
#==============================================================================


# Load in the best land cover class data for each year, effectively getting 
# a raster stack with each year's data as a layer
files <- list.files(
  path = "data/rasters/land_cover/best_class",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 25)

# Plot and save the land cover raster data
p <- ggplot() +
  geom_spatraster(data = as.factor(r), maxcell = 5000) +
  facet_wrap(~lyr) +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/land_cover_best_class.jpg",
  width = 5000,
  height = 5000,
  units = "px"
)

 #==============================================================================


# Generate plots for all land cover categories for the years 2000-2024

historical.files <- list.files(
  path = "data/rasters/land_cover/processed",
  pattern = "^2",
  full.names = TRUE
)
ssp245.files <- list.files(
  path = "data/rasters/land_cover/processed",
  pattern = "SSP245",
  full.names = TRUE
)
files <- c(historical.files, ssp245.files)[1:25] 

r <- rast(files)
assert_that(dim(r)[3] == 14 * 25)

# Plot and save the land cover raster data
for(var in base.names) {
  
  r.sub <- r %>%
    select(matches(var))
  
  assert_that(dim(r.sub)[3] == 25)
  
  p <- ggplot() +
    geom_spatraster(data = r.sub, maxcell = 5000) +
    facet_wrap(~lyr) +
    theme_void()
  
  ggsave(
    p,
    filename = paste0("outputs/predictor_layers/land_cover_", var, ".jpg"),
    width = 5000,
    height = 5000,
    units = "px"
  )
}

#==============================================================================


# Generate plots of median and mean values for different land use variables 
# in different SSP scenarios over time

files <- list.files(
  path = "data/rasters/land_cover/processed", 
  full.names = TRUE
)

# Generate data frame to track changes in land cover variables over time
d <- data.frame(
  variable = rep(base.names, each = length(files)),
  year = rep(as.numeric(str_extract(files, "[0-9]{4}")), times = length(base.names)),
  type = rep(
    ifelse(
      is.na(str_extract(files, "SSP[0-9]{3}")), 
      "Historical",
      str_extract(files, "SSP[0-9]{3}")
    ),
    times = length(base.names)
  ),
  median_value = NA,
  mean_value = NA
)

r <- rast(files)
assert_that(dim(r)[3] == 14 * length(files))

# Loop through all variables to fill in the data frame
for(var in base.names) {
  
  r.sub <- r %>%
    select(matches(var))
  
  assert_that(dim(r.sub)[3] == length(files))
  
  values <- values(r.sub)
  d[d$variable == var, "median_value"] <- apply(values, 2, median, na.rm = TRUE)
  d[d$variable == var, "mean_value"] <- apply(values, 2, mean, na.rm = TRUE)
}

# Plot median values over time
p <- d %>%
  filter(!(variable %in% c("secma", "secmb"))) %>%
  ggplot(aes(x = year, y = median_value, group = type, color = type)) +
  geom_line() +
  geom_vline(xintercept = 2015, linetype = 2) +
  xlab("") +
  ylab("Median value across study region") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_wrap(~variable, scales = "free_y")

ggsave(
  p,
  filename = paste0("outputs/predictor_layers/land_cover_all_scenarios_median.jpg"),
  width = 3000,
  height = 2000,
  units = "px"
)

# Plot mean values over time
p <- d %>%
  filter(!(variable %in% c("secma", "secmb"))) %>%
  ggplot(aes(x = year, y = mean_value, group = type, color = type)) +
  geom_line() +
  geom_vline(xintercept = 2015, linetype = 2) +
  xlab("") +
  ylab("Mean value across study region") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_wrap(~variable, scales = "free_y")

ggsave(
  p,
  filename = paste0("outputs/predictor_layers/land_cover_all_scenarios_mean.jpg"),
  width = 3000,
  height = 2000,
  units = "px"
)
