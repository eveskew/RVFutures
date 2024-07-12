library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)

sf_use_s2(FALSE)

source("R/functions.R")


# Generate "absence" data (RVF outbreak background/pseudo-absence data) for use 
# in modeling exercises

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda
east.africa <- load_country_map()

# Create a masked version of the background outlines that erases out large
# lakes so pseudo-absences don't end up showing up over water where we don't 
# have predictor data
lakes.10 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_10.rds")
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
east.africa.lake.erase <- st_erase(east.africa, lakes.10)

# Import observed RVF outbreak data
d <- read_csv("data/outbreak_data/EC_RVF_clean_April27.csv")
d.sf <- st_as_sf(d, coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

#==============================================================================


# Generate pseudo-absence points randomly from within the East African
# country extent

# More, specifically generate random pseudo-absence points for each year
# in the observed data to completely evaluate the available
# environmental space


# Import a reference raster and build a grid for the study region
r <- rast("data/rasters/elevation/processed/elevation_2.5min.tif")

# Make a 10 arcminute grid across the study region
east.africa.grid <- st_make_grid(
  east.africa, 
  cellsize = 10/60
)

n.points.per.yrmon <- 150
points <- data.frame()

set.seed(8)

for(year in 2008:2022) {
  
  # Calculate which cells in the grid were positive for actual RVF outbreaks
  # in the focal year
  outbreak.cells <- st_intersects(
    filter(d.sf, OB_Yr == year),
    east.africa.grid
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  print(outbreak.cells)
  
  # Generate a map of East Africa with gaps for the positive grid cells to
  # ensure that pseudo-absence data are not drawn from locations with actual
  # RVF outbreaks
  ifelse(
    is.null(outbreak.cells),
    sampling.area.for.year <- east.africa.lake.erase,
    sampling.area.for.year <- st_erase(east.africa.lake.erase, east.africa.grid[outbreak.cells])
  )
  
  # Generate a data frame of random pseudo-absence points across all months
  # of the focal year
  temp <- st_sample(
    sampling.area.for.year, 
    size = n.points.per.yrmon * 12
  ) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    ) %>%
    mutate(
      OB_Yr = rep(year, times = n.points.per.yrmon * 12),
      OB_Mo = rep(month.name, each = n.points.per.yrmon) 
    )
  
  points <- bind_rows(points, temp)
}

random.points <- points

#==============================================================================


# Generate points randomly for each outbreak location using "doughnut" approach


# Create a layer for sampling that represents regions neither too close nor 
# too far from outbreak locations

inner.buffer <- d.sf %>%
  # strip to just the geometry of the outbreak points
  st_geometry() %>%
  # buffer around them
  st_buffer(dist = 0.5) %>%
  # combine all 100 buffers into one layer
  st_combine() %>%
  # make it behave nicely
  st_make_valid()

outer.buffer <- d.sf %>%
  # strip to just the geometry of the outbreak points
  st_geometry() %>%
  # buffer around them
  st_buffer(dist = 1.5) %>%
  # combine all 100 buffers into one layer
  st_combine() %>%
  # make it behave nicely
  st_make_valid()

east.africa.doughnut <- east.africa.lake.erase %>%
  st_intersection(outer.buffer) %>%
  st_difference(inner.buffer)

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = east.africa.doughnut, fill = "grey", color = "grey") +
  geom_sf(data = d.sf) +
  theme_minimal()


n.points.per.obs <- 10

set.seed(8)

points <- data.frame()

for(i in 1:nrow(d.sf)) {
  
  temp <- st_sample(
    east.africa.doughnut, 
    size = n.points.per.obs
  ) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    ) %>%
    mutate(
      OB_Yr = rep(d$OB_Yr[i], times = n.points.per.obs),
      OB_Mo = rep(d$OB_Mo[i], times = n.points.per.obs) 
    )
  
  points <- bind_rows(points, temp)
}

doughnut.points <- points

#==============================================================================


# Use an approach that is population-weighted for humans


# Load in human population density raster files
files <- list.files(
  path = "data/rasters/human_population/processed",
  pattern = "pd",
  full.names = TRUE
)

files

r <- terra::rast(files)

# Calculate total population size represented in the raster and plot
cs <- cellSize(r, unit = "km")
global(r * cs, "sum", na.rm = T)
plot(r)

# Aggregate the raster to 10 arcminute grid cells and report prior statistics
r.agg <- aggregate(r, fact = 4, fun = "mean")
cs.agg <- cellSize(r.agg, unit = "km")
global(r.agg * cs.agg, "sum", na.rm = T)
plot(r.agg)


# Make the East Africa map into a grid matching the extent and resolution of 
# the aggregated raster
east.africa.grid <- st_make_grid(
  east.africa, 
  n = c(dim(r.agg)[2], dim(r.agg)[1]),
  cellsize = res(r.agg)
)

ggplot() + 
  geom_sf(data = east.africa.lake.erase, fill = "grey") +
  geom_sf(data = east.africa.grid, fill = NA) +
  theme_void()


# Generate pseudo-absence points in proportion to the human population density
# raster

n.points.per.yrmon <- 150
human.points <- data.frame()

for(year in 2008:2022) {
  
  # Pull the correct population raster from the aggregated raster stack
  year.mod <- ifelse(year > 2020, 2020, year)
  r.temp <- r.agg[[str_detect(names(r.agg), as.character(year.mod))]]
  
  # Modify the aggregated human population density raster just to reduce the
  # magnitude of the values and therefore generate a reasonable number of 
  # pseudo-absence points
  r.temp <- r.temp/15
  
  # Generate a vector of cells for this year with RVF outbreak cases
  outbreak.cells <- st_intersects(
    filter(d.sf, OB_Yr == year),
    east.africa.grid
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  print(outbreak.cells)
  
  # Generate a vector of human population density values
  # (corresponding to the value of each raster grid cell) 
  human.pop.vec <- r.temp %>%
    # need to flip this raster vertically since "st_sample()" will start
    # sampling from the bottom left cell and move upwards rather than starting
    # from the upper left
    flip(direction = "vertical") %>%
    # extract values
    values() %>%
    # convert population density values to integers
    as.integer() %>%
    # convert NA values to zeroes
    ifelse(is.na(.), 0, .)
  
  # Set outbreak cell values to 0 so they will not be sampled for 
  # pseudo-absences
  human.pop.vec[outbreak.cells] <- 0
  
  # How many pseudo-absences will be generated?
  print(sum(human.pop.vec))
  
  assertthat::assert_that(length(east.africa.grid) == length(human.pop.vec))
  
  # Generate pseudo-absences, only targeting grid cells with positive human 
  # population densities to save computation time
  temp <- st_sample(
    east.africa.grid[human.pop.vec > 0],
    size = human.pop.vec[human.pop.vec > 0]
  )
  
  temp <- temp %>%
    st_intersection(., east.africa.lake.erase) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    # only pull a subset of points
    sample_n(size = n.points.per.yrmon * 12) %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    ) %>%
    mutate(
      OB_Yr = rep(year, times = n.points.per.yrmon * 12),
      OB_Mo = rep(month.name, each = n.points.per.yrmon)
    )
  
  human.points <- rbind(human.points, temp)  
}

#==============================================================================


# Use an approach that is inversely related to travel time to healthcare


# Load in travel to healthcare raster
r <- terra::rast("data/rasters/healthcare/processed/healthcare_2.5min.tif")
plot(r)

# Aggregate the raster to 10 arcminute grid cells
r.agg <- aggregate(r, fact = 4, fun = "mean")
plot(r.agg)


# Make the East Africa map into a grid matching the extent and resolution of 
# the aggregated raster
east.africa.grid <- st_make_grid(
  east.africa, 
  n = c(dim(r.agg)[2], dim(r.agg)[1]),
  cellsize = res(r.agg)
)

ggplot() + 
  geom_sf(data = east.africa.lake.erase, fill = "grey") +
  geom_sf(data = east.africa.grid, fill = NA) +
  theme_void()


# Generate pseudo-absence points in inverse proportion to the 
# healthcare travel time raster

n.points.per.yrmon <- 150
travel.points <- data.frame()

for(year in 2008:2022) {
  
  # Modify the aggregated healthcare travel time raster to sample in the inverse
  # and to increase the magnitude of the values, therefore generating a 
  # reasonable number of pseudo-absence points
  r.agg.mod <- (1/r.agg) * 100
  
  # Generate a vector of cells for this year with RVF outbreak cases
  outbreak.cells <- st_intersects(
    filter(d.sf, OB_Yr == year),
    east.africa.grid
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  print(outbreak.cells)
  
  # Generate a vector of inverse travel time values
  # (corresponding to the value of each raster grid cell) 
  travel.vec <- r.agg.mod %>%
    # need to flip this raster vertically since "st_sample()" will start
    # sampling from the bottom left cell and move upwards rather than starting
    # from the upper left
    flip(direction = "vertical") %>%
    # extract values
    values() %>%
    # convert population density values to integers
    as.integer() %>%
    # convert NA values to zeroes
    ifelse(is.na(.), 0, .)
  
  # Set outbreak cell values to 0 so they will not be sampled for 
  # pseudo-absences
  travel.vec[outbreak.cells] <- 0
  
  # How many pseudo-absences will be generated?
  print(sum(travel.vec))
  
  assertthat::assert_that(length(east.africa.grid) == length(travel.vec))
  
  # Generate pseudo-absences, only targeting grid cells with positive travel 
  # values to save computation time
  temp <- st_sample(
    east.africa.grid[travel.vec > 0],
    size = travel.vec[travel.vec > 0]
  )
  
  temp <- temp %>%
    st_intersection(., east.africa.lake.erase) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    # only pull a subset of points
    sample_n(size = n.points.per.yrmon * 12) %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    ) %>%
    mutate(
      OB_Yr = rep(year, times = n.points.per.yrmon * 12),
      OB_Mo = rep(month.name, each = n.points.per.yrmon)
    )
  
  travel.points <- rbind(travel.points, temp)  
}

#==============================================================================


# Append pseudo-absences to outbreak data and visualize


d.random <- d %>%
  bind_rows(random.points) %>%
  mutate(RVF_presence = ifelse(is.na(CASES), 0, 1)) 

d.random.sf <- d.random %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))
  
ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.10, fill = "lightblue") +
  geom_sf(data = d.random.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = d.random.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  facet_wrap(~OB_Yr)


d.doughnut <- d %>%
  bind_rows(doughnut.points) %>%
  mutate(RVF_presence = ifelse(is.na(CASES), 0, 1)) 

d.doughnut.sf <- d.doughnut %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.10, fill = "lightblue") +
  geom_sf(data = d.doughnut.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = d.doughnut.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  facet_wrap(~OB_Yr)


d.popweighted <- d %>%
  bind_rows(human.points) %>%
  mutate(RVF_presence = ifelse(is.na(CASES), 0, 1)) 

d.popweighted.sf <- d.popweighted %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa.lake.erase, fill = "white") +
  geom_sf(data = lakes.10, fill = "lightblue") +
  geom_sf(data = d.popweighted.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = d.popweighted.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  facet_wrap(~OB_Yr)


d.travel <- d %>%
  bind_rows(travel.points) %>%
  mutate(RVF_presence = ifelse(is.na(CASES), 0, 1)) 

d.travel.sf <- d.travel %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.10, fill = "lightblue") +
  geom_sf(data = d.travel.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = d.travel.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  facet_wrap(~OB_Yr)


# Save data

write_csv(d.random, file = "data/outbreak_data/data_random_pseudoabsences.csv")
write_csv(d.popweighted, file = "data/outbreak_data/data_popweighted_pseudoabsences.csv")
write_csv(d.travel, file = "data/outbreak_data/data_travel_pseudoabsences.csv")
