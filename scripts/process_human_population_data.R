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
  
  assert_that(length(files) == 12)
  
  # Combine the rasters
  s <- sprc(files)
  s <- merge(s)
  names(s) <- paste0("pd_", i)
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(s, describe = TRUE)$code != "4326" | is.na(crs(s, describe = TRUE)$code)) {s <- project(x = s, y = "epsg:4326")}
  
  # Resample the raster to 2.5 arcminutes and check that the total population
  # counts are preserved
  s.crop <- crop(s, x)
  s.resample <- resample(s.crop, x, "bilinear")
  
  s.tot.count <- global(
    s.crop * cellSize(s.crop, unit = "km"), 
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
  geom_spatraster(data = r, maxcell = 5000) +
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


# Modify Wang et al. projected population data
# https://www.nature.com/articles/s41597-022-01675-x

years <- as.character(
  c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070)
)
east.africa <- load_country_map()
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Loop through all years
for(i in years) {
  
  # Pull raster file names for that year
  file.ssp1 <- list.files(
    path = "data/rasters/human_population/SPP1", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  file.ssp2 <- list.files(
    path = "data/rasters/human_population/SPP2", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  file.ssp5 <- list.files(
    path = "data/rasters/human_population/SPP5", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  
  # Import the rasters
  r.ssp1 <- terra::rast(file.ssp1)
  r.ssp2 <- terra::rast(file.ssp2)
  r.ssp5 <- terra::rast(file.ssp5)
  
  # Reproject the rasters to EPSG 4326, if needed
  if(crs(r.ssp1, describe = TRUE)$code != "4326" | is.na(crs(r.ssp1, describe = TRUE)$code)) {r.ssp1 <- project(x = r.ssp1, y = "epsg:4326")}
  if(crs(r.ssp2, describe = TRUE)$code != "4326" | is.na(crs(r.ssp2, describe = TRUE)$code)) {r.ssp2 <- project(x = r.ssp2, y = "epsg:4326")}
  if(crs(r.ssp5, describe = TRUE)$code != "4326" | is.na(crs(r.ssp5, describe = TRUE)$code)) {r.ssp5 <- project(x = r.ssp5, y = "epsg:4326")}
  
  # Crop and resample from count to density raster, 
  # checking that the total population counts are preserved
  r.ssp1.crop <- crop(r.ssp1, east.africa)
  r.ssp1.resample <- resample_count_raster(r.ssp1.crop, east.africa, x)
  
  r.tot.count <- global(r.ssp1.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp1.resample * cellSize(r.ssp1.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  r.ssp2.crop <- crop(r.ssp2, east.africa)
  r.ssp2.resample <- resample_count_raster(r.ssp2.crop, east.africa, x)
  
  r.tot.count <- global(r.ssp2.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp2.resample * cellSize(r.ssp2.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  r.ssp5.crop <- crop(r.ssp5, east.africa)
  r.ssp5.resample <- resample_count_raster(r.ssp5.crop, east.africa, x)
  
  r.tot.count <- global(r.ssp5.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp5.resample * cellSize(r.ssp5.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  # Save the merged raster files
  writeRaster(
    r.ssp1.resample, 
    paste0("data/rasters/human_population/processed/SSP1_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
  writeRaster(
    r.ssp2.resample, 
    paste0("data/rasters/human_population/processed/SSP2_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
  writeRaster(
    r.ssp5.resample, 
    paste0("data/rasters/human_population/processed/SSP5_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
}

#==============================================================================


# Modify Boke-Olen et al. projected population data
# https://www.nature.com/articles/sdata2016130

years <- as.character(2021:2050)
east.africa <- load_country_map()
x <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")
threshold = 0.015

# Loop through all years
for(i in years) {
  
  # Pull raster file names for that year
  file.ssp126 <- list.files(
    path = "data/rasters/human_population/african-future-population_SSP1-RCP2.6_2000-01-01_2100-12-31_1year_original", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  file.ssp245 <- list.files(
    path = "data/rasters/human_population/african-future-population_SSP2-RCP4.5_2000-01-01_2100-12-31_1year_original", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  file.ssp585 <- list.files(
    path = "data/rasters/human_population/african-future-population_SSP5-RCP8.5_2000-01-01_2100-12-31_1year_original", 
    pattern = paste0(i, ".tif$"),
    full.names = TRUE
  )
  
  # Import the rasters
  r.ssp126 <- terra::rast(file.ssp126)
  r.ssp245 <- terra::rast(file.ssp245)
  r.ssp585 <- terra::rast(file.ssp585)
  
  # Reproject the rasters to EPSG 4326, if needed
  if(crs(r.ssp126, describe = TRUE)$code != "4326" | is.na(crs(r.ssp126, describe = TRUE)$code)) {r.ssp126 <- project(x = r.ssp126, y = "epsg:4326")}
  if(crs(r.ssp245, describe = TRUE)$code != "4326" | is.na(crs(r.ssp245, describe = TRUE)$code)) {r.ssp245 <- project(x = r.ssp245, y = "epsg:4326")}
  if(crs(r.ssp585, describe = TRUE)$code != "4326" | is.na(crs(r.ssp585, describe = TRUE)$code)) {r.ssp585 <- project(x = r.ssp585, y = "epsg:4326")}
  
  # Crop and resample from count to density raster, 
  # checking that the total population counts are preserved
  r.ssp126.crop <- crop(r.ssp126, east.africa)
  r.ssp126.resample <- resample_count_raster(r.ssp126.crop, east.africa, x, threshold = threshold)
  names(r.ssp126.resample) <- paste0("SSP126_", i)
  
  r.tot.count <- global(r.ssp126.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp126.resample * cellSize(r.ssp126.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that((1 - threshold) < (r.tot.count/r.r.tot.count) & (1 + threshold) > (r.tot.count/r.r.tot.count))
  
  r.ssp245.crop <- crop(r.ssp245, east.africa)
  r.ssp245.resample <- resample_count_raster(r.ssp245.crop, east.africa, x, threshold = threshold)
  names(r.ssp245.resample) <- paste0("SSP245_", i)
  
  r.tot.count <- global(r.ssp245.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp245.resample * cellSize(r.ssp245.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  r.ssp585.crop <- crop(r.ssp585, east.africa)
  r.ssp585.resample <- resample_count_raster(r.ssp585.crop, east.africa, x, threshold = threshold)
  names(r.ssp585.resample) <- paste0("SSP585_", i)
  
  r.tot.count <- global(r.ssp585.crop, "sum", na.rm = TRUE)
  r.r.tot.count <- global(
    r.ssp585.resample * cellSize(r.ssp585.resample, unit = "km"), 
    "sum", na.rm = TRUE
  )
  assert_that(0.99 < (r.tot.count/r.r.tot.count) & 1.01 > (r.tot.count/r.r.tot.count))
  
  # Save the merged raster files
  writeRaster(
    r.ssp126.resample, 
    paste0("data/rasters/human_population/processed/SSP126_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
  writeRaster(
    r.ssp245.resample, 
    paste0("data/rasters/human_population/processed/SSP245_2.5min_", i, ".tif"),
    overwrite = TRUE
  )
  writeRaster(
    r.ssp585.resample, 
    paste0("data/rasters/human_population/processed/SSP585_2.5min_", i, ".tif"),
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
  select(matches("2025|2030|2035|2040|2045|2050")) %>%
  select(matches("linear|SSP1_|SSP2_|SSP5_"))

# Plot and save the human population raster data
p <- ggplot() +
  geom_spatraster(data = r.sub, maxcell = 5000) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, nrow = 4) +
  theme_void()

ggsave(
  p,
  filename = "outputs/predictor_layers/human_population_projected.jpg",
  width = 5000,
  height = 4000,
  units = "px"
)
 