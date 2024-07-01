library(tidyverse)
library(terra)
library(sf)
library(tidyterra)

sf_use_s2(FALSE)

source("R/functions.R")

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda

east.africa <- load_country_map()


# Import lakes data, ensure proper CRS (4326), crop to the country extents, 
# and save with various lake sizes included

lakes.all <- st_read("data/rasters/hydrology/HydroLAKES_polys_v10_shp") %>%
  st_transform(4326) %>%
  st_crop(east.africa)

ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.all, fill = "blue") +
  theme_minimal()

saveRDS(lakes.all, file = "data/rasters/hydrology/saved_objects/lakes_east_africa_all.rds")


lakes.1 <- filter(lakes.all, Lake_area >= 1)

ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.1, fill = "blue") +
  theme_minimal()

saveRDS(lakes.1, file = "data/rasters/hydrology/saved_objects/lakes_east_africa_1.rds")
  

lakes.5 <- filter(lakes.all, Lake_area >= 5)

ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.5, fill = "blue") +
  theme_minimal()

saveRDS(lakes.5, file = "data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds")


lakes.10 <- filter(lakes.all, Lake_area >= 10)

ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.10, fill = "blue") +
  theme_minimal()
  
saveRDS(lakes.10, file = "data/rasters/hydrology/saved_objects/lakes_east_africa_10.rds")
  

# Import river data, ensure proper CRS (4326), crop to the country extents, 
# and save

rivers <- st_read("data/rasters/hydrology/HydroRIVERS_v10_af_shp") %>%
  st_transform(4326) %>%
  st_crop(east.africa)

ggplot() +
  geom_sf(data = east.africa) +
  geom_sf(data = rivers, color = "blue") +
  theme_minimal()

saveRDS(rivers, file = "data/rasters/hydrology/saved_objects/rivers_east_africa.rds")

# Filter to only "ORD_FLOW" <= 5, which corresponds to rivers with >= 10 m3/s
# flow, and mask out large lake regions

rivers.10 <- filter(rivers, ORD_FLOW <= 5)
lakes.to.mask <- lakes.all %>% 
  filter(Lake_area >= 100) %>%
  st_union()
rivers.10.mask <- st_difference(rivers.10, lakes.to.mask)
                               
ggplot() +
  geom_sf(data = east.africa) +
  geom_sf(data = rivers.10, color = "purple") +
  geom_sf(data = rivers.10.mask, color = "blue") +
  theme_minimal()

saveRDS(rivers.10.mask, file = "data/rasters/hydrology/saved_objects/rivers_east_africa_10.rds")

#==============================================================================


# For the hydrology datasets, make rasters that represent the distance from 
# grid cell centroids to the various hydrology features

# Load in precipitation raster as a reference
r <- terra::rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Make the East Africa map into a grid matching the extent and resolution of 
# the raster
east.africa.grid <- st_make_grid(
  east.africa, 
  n = c(dim(r)[2], dim(r)[1]),
  cellsize = res(r)
)

# Make an sf object of grid cell centroid points
longitude <- seq(
  from = xmin(r) + res(r)[1]/2, 
  to = xmax(r) - res(r)[1]/2, 
  by = res(r)[1]
)
assertthat::assert_that(length(longitude) == dim(r)[2])
latitude <- seq(
  from = ymin(r) + res(r)[2]/2, 
  to = ymax(r) - res(r)[2]/2, 
  by = res(r)[2]
)
assertthat::assert_that(length(latitude) == dim(r)[1])
centroid.points <- expand.grid(
  x = longitude,
  y = latitude
) %>%
  # need to sort this way so that the grid cells appear in order from
  # north-to-south, west-to-east, the same way the cells are sorted in raster files
  arrange(desc(y), x) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(east.africa))


# Generate rasters for the various lake layers

values(r) <- extract_hydrology_distance(centroid.points, lakes.all)
varnames(r) <- "dist_to_lake_all"
names(r) <- "dist_to_lake_all"

plot(r)
ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.all, fill = "blue") +
  theme_minimal()

# Save the raster file
if(!dir.exists("data/rasters/hydrology/processed")) {
  dir.create("data/rasters/hydrology/processed")
}

writeRaster(
  r, 
  paste0("data/rasters/hydrology/processed/dist_to_lake_all_2.5min.tif"),
  overwrite = TRUE
)


values(r) <- extract_hydrology_distance(centroid.points, lakes.1)
varnames(r) <- "dist_to_lake_1"
names(r) <- "dist_to_lake_1"

plot(r)
ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.1, fill = "blue") +
  theme_minimal()

# Save the raster file
writeRaster(
  r, 
  paste0("data/rasters/hydrology/processed/dist_to_lake_1_2.5min.tif"),
  overwrite = TRUE
)


values(r) <- extract_hydrology_distance(centroid.points, lakes.5)
varnames(r) <- "dist_to_lake_5"
names(r) <- "dist_to_lake_5"

plot(r)
ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.5, fill = "blue") +
  theme_minimal()

# Save the raster file
writeRaster(
  r, 
  paste0("data/rasters/hydrology/processed/dist_to_lake_5_2.5min.tif"),
  overwrite = TRUE
)


values(r) <- extract_hydrology_distance(centroid.points, lakes.10)
varnames(r) <- "dist_to_lake_10"
names(r) <- "dist_to_lake_10"

plot(r)
ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = lakes.10, fill = "blue") +
  theme_minimal()

# Save the raster file
writeRaster(
  r, 
  paste0("data/rasters/hydrology/processed/dist_to_lake_10_2.5min.tif"),
  overwrite = TRUE
)


# Generate raster for the river layer

values(r) <- extract_hydrology_distance(centroid.points, rivers.10.mask)
varnames(r) <- "dist_to_river_10"
names(r) <- "dist_to_river_10"

plot(r)
ggplot() + 
  geom_sf(data = east.africa) + 
  geom_sf(data = rivers.10.mask, color = "blue") +
  theme_minimal()

# Save the raster file
writeRaster(
  r, 
  paste0("data/rasters/hydrology/processed/dist_to_river_10_2.5min.tif"),
  overwrite = TRUE
)

#==============================================================================


# Load in the cropped hydrology rasters
files <- list.files(
  path = "data/rasters/hydrology/processed",
  full.names = TRUE
)

r <- rast(files)
assertthat::assert_that(dim(r)[3] == 5)


# Plot and save the hydrology raster data
p <- ggplot() +
  geom_spatraster(data = r) +
  scale_fill_distiller(palette = "Blues", na.value = "white") +
  facet_wrap(~lyr, ncol = 2) +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/hydrology.jpg",
  width = 2000,
  height = 3000,
  units = "px"
)
