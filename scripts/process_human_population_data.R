library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# For all years, load in human population density raster data from 
# Kenya, Uganda, and Tanzania, then merge and save the merged raster file

years <- as.character(2001:2020)
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Loop through all years
for(i in years) {
  
  # Pull raster file names for that year
  files <- list.files(
    path = "data/rasters/human_population/WorldPop", 
    pattern = i,
    full.names = TRUE
  )
  
  # Combine the rasters
  s <- sprc(files)
  s <- merge(s)
  names(s) <- paste0("pd_", i)
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(s, describe = TRUE)$code != "4326" | is.na(crs(s, describe = TRUE)$code)) {s <- project(x = s, y = "epsg:4326")}
  
  # Resample the raster to 2.5 minutes and check that the total population
  # counts are preserved
  s.resample <- resample(s, x, "bilinear")
  
  s.tot.count <- global(
    s * cellSize(s, unit = "km"), 
    "sum", na.rm = TRUE
  )
  s.r.tot.count <- global(
    s.resample * cellSize(s.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (s.tot.count/s.r.tot.count) & 1.01 > (s.tot.count/s.r.tot.count))
  
  # Save the merged raster file
  if(!dir.exists("data/rasters/human_population/processed")) {
    dir.create("data/rasters/human_population/processed")
  }
  
  writeRaster(
    s.resample, 
    paste0("data/rasters/human_population/processed/pd_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
}

#==============================================================================


# Load in the merged raster data, effectively getting a raster stack with
# each year's data as a layer
files <- list.files(
  path = "data/rasters/human_population/processed/",
  pattern = "pd",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 20)

# Plot and save the human population raster data
p <- ggplot() +
  geom_spatraster(data = r, maxcell = 10000) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr) +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/human_population.jpg",
  width = 4000,
  height = 4000,
  units = "px"
)

#==============================================================================


# Modify Wang et al. projected population data under the SSPs
# https://www.nature.com/articles/s41597-022-01675-x

years <- as.character(c(2025, 2030, 2035, 2040, 2045, 2050))
east.africa <- load_country_map()
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Loop through all years
for(i in years) {
  
  # Pull raster file names for that year
  file.ssp2 <- list.files(
    path = "data/rasters/human_population/SPP2", 
    pattern = i,
    full.names = TRUE
  )
  file.ssp5 <- list.files(
    path = "data/rasters/human_population/SPP5", 
    pattern = i,
    full.names = TRUE
  )
  
  # Import the rasters
  r.ssp2 <- terra::rast(file.ssp2)
  r.ssp5 <- terra::rast(file.ssp5)
  
  # Reproject the rasters to EPSG 4326, if needed
  if(crs(r.ssp2, describe = TRUE)$code != "4326" | is.na(crs(r.ssp2, describe = TRUE)$code)) {r.ssp2 <- project(x = r.ssp2, y = "epsg:4326")}
  if(crs(r.ssp5, describe = TRUE)$code != "4326" | is.na(crs(r.ssp5, describe = TRUE)$code)) {r.ssp5 <- project(x = r.ssp5, y = "epsg:4326")}
  
  # Crop, resample, and mask the rasters and check that the 
  # total population counts are preserved
  r.ssp2.crop <- crop(r.ssp2, east.africa)
  r.ssp2.resample <- resample(r.ssp2.crop, x, "bilinear")
  r.ssp2.mask <- mask(r.ssp2.resample, r$pd_2001)
  
  r.tot.count <- global(
    r.ssp2.crop * cellSize(r.ssp2.crop, unit = "km"), 
    "sum", na.rm = TRUE
  )
  r.r.tot.count <- global(
    r.ssp2.resample * cellSize(r.ssp2.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  r.ssp5.crop <- crop(r.ssp5, east.africa)
  r.ssp5.resample <- resample(r.ssp5.crop, x, "bilinear")
  r.ssp5.mask <- mask(r.ssp5.resample, r$pd_2001)
  
  r.tot.count <- global(
    r.ssp5.crop * cellSize(r.ssp5.crop, unit = "km"), 
    "sum", na.rm = TRUE
  )
  r.r.tot.count <- global(
    r.ssp5.resample * cellSize(r.ssp5.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  # Save the merged raster files
  if(!dir.exists("data/rasters/human_population/processed")) {
    dir.create("data/rasters/human_population/processed")
  }
  
  writeRaster(
    r.ssp2.mask, 
    paste0("data/rasters/human_population/processed/SSP2_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
  writeRaster(
    r.ssp5.mask, 
    paste0("data/rasters/human_population/processed/SSP5_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
}

#==============================================================================


# Generate linear projections of future human population density

first.data.year <- 2001

# Regress across the years
x <- regress(r, 1:nlyr(r), na.rm = TRUE)

# Generate predictor layers for 2021-2050 and save them
for(year in 2021:2050) {
  
  writeRaster(
    generate_raster_projection(
      regression_raster = x,
      first_year_of_data = first.data.year,
      projection_year = year,
      lower_clamp = 0,
      layer_name = paste0("linear_projection_", year)
    ), 
    paste0("data/rasters/human_population/processed/linear_projection_2.5min_", year, ".tif"),
    overwrite = TRUE
  )
}
  
#==============================================================================


# Load in the merged raster data, effectively getting a raster stack with
# each year's data as a layer
files <- list.files(
  path = "data/rasters/human_population/processed",
  full.names = TRUE
)

r <- rast(files)

# Display various future projections of human population density
r.sub <- r %>% 
  select(matches("2025|2030|2035|2040|2045|2050"))

# Plot and save the human population raster data
p <- ggplot() +
  geom_spatraster(data = r.sub, maxcell = 10000) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, nrow = 3) +
  theme_void()

ggsave(
  p,
  filename = "outputs/predictor_layers/human_population_projected.jpg",
  width = 5000,
  height = 3000,
  units = "px"
)
 