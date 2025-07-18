library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(assertthat)

sf_use_s2(FALSE)

source("R/functions.R")


# Generate "absence" data (RVF outbreak background/pseudo-absence data) for use 
# in modeling

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda
east.africa <- load_country_map()

# Create a masked version of the background outlines that erases out 
# medium/large lakes so pseudo-absences don't end up showing up over water 
# where we don't have predictor data
lakes.5 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds")
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
east.africa.lake.erase <- st_erase(east.africa, lakes.5)

# Import observed RVF outbreak data
d <- read_csv("data/outbreak_data/outbreak_data_randomly_filled.csv")
d.sf <- st_as_sf(d, coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

n.points.per.yrmon <- 150
# Define the outbreak buffer radius in degrees
outbreak.buffer.radius <- 0.2

#==============================================================================


# Generate pseudo-absence points randomly from within the East African
# country extent

# More specifically, generate random pseudo-absence points for each month
# in the observed data to completely evaluate the available
# environmental space


random.points <- data.frame()

set.seed(8)

for(year in 2008:2022) {
  
  # Generate a buffer representing RVF outbreak locations in the focal year
  outbreak.buffer <- d.sf %>%
    filter(outbreak_year == year) %>%
    st_buffer(dist = outbreak.buffer.radius * 2) %>%
    st_union()
  
  # Generate a data frame of random pseudo-absence points for the focal year, 
  # drawing 5x as many points as ultimately needed to 
  # account for later filtering based on known outbreak locations
  temp <- st_sample(
    east.africa.lake.erase,
    size = n.points.per.yrmon * 12 * 5
  ) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    )
  
  # Filter points to eliminate any within the outbreak buffer zone,
  # assuming there are outbreak points in that year
  if (length(outbreak.buffer) > 0) {
    
    temp <- st_difference(
      st_as_sf(temp, coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa)),
      outbreak.buffer
    ) %>%
      mutate(
        GPS_x = st_coordinates(.)[,1],
        GPS_y = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry()
  }
  
  assert_that(nrow(temp) >= n.points.per.yrmon * 12)
  
  # Subsample down to only the required number of points for a year
  temp <- slice_sample(temp, n = n.points.per.yrmon * 12) %>%
    mutate(
      outbreak_year = rep(year, times = n.points.per.yrmon * 12),
      outbreak_month = rep(month.name, each = n.points.per.yrmon)
    )
  
  random.points <- bind_rows(random.points, temp)
}

#==============================================================================


# Generate points randomly for each outbreak location using "doughnut" approach


# Create a layer for sampling that represents regions neither too close nor 
# too far from outbreak locations

inner.buffer <- d.sf %>%
  # strip to just the geometry of the outbreak points
  st_geometry() %>%
  # buffer around them
  st_buffer(dist = 0.5) %>%
  # combine all buffers into one layer
  st_combine() %>%
  # make it behave nicely
  st_make_valid()

outer.buffer <- d.sf %>%
  # strip to just the geometry of the outbreak points
  st_geometry() %>%
  # buffer around them
  st_buffer(dist = 1.5) %>%
  # combine all buffers into one layer
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

doughnut.points <- data.frame()

set.seed(8)

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
      outbreak_year = rep(d$outbreak_year[i], times = n.points.per.obs),
      outbreak_month = rep(d$outbreak_month[i], times = n.points.per.obs) 
    )
  
  doughnut.points <- bind_rows(doughnut.points, temp)
}

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
global(r * cs, "sum", na.rm = TRUE)
plot(r)

# Make the East Africa map into a grid matching the extent and resolution of 
# the raster
east.africa.grid <- st_make_grid(
  east.africa, 
  n = c(dim(r)[2], dim(r)[1]),
  cellsize = res(r)
)

ggplot() + 
  geom_sf(data = east.africa.lake.erase, fill = "grey") +
  geom_sf(data = east.africa.grid, fill = NA) +
  theme_void()


# Generate pseudo-absence points in proportion to the human population density
# raster

human.points <- data.frame()

set.seed(8)

for(year in 2008:2022) {
  
  # Generate a buffer representing RVF outbreak locations in the focal year
  outbreak.buffer <- d.sf %>%
    filter(outbreak_year == year) %>%
    st_buffer(dist = outbreak.buffer.radius * 2) %>%
    st_union()
  
  # Pull the correct population raster from the raster stack
  year.mod <- ifelse(year > 2020, 2020, year)
  r.temp <- r[[str_detect(names(r), as.character(year.mod))]]
  
  # Generate a vector of human population density values
  # (corresponding to the value of each raster grid cell) 
  human.pop.vec <- r.temp %>%
    # need to flip this raster vertically since "st_sample()" will start
    # sampling from the bottom left cell and move upwards rather than starting
    # from the upper left
    flip(direction = "vertical") %>%
    # extract values
    values() %>%
    # convert NA values to zeroes
    ifelse(is.na(.), 0, .)
  
  # With what probability should each grid cell be sampled?
  probs <- human.pop.vec / sum(human.pop.vec)
  # Sample grid cells in proportion to their probability
  samples <- sample(1:length(human.pop.vec), size = 10000, prob = probs, replace = TRUE)
  # Generate a vector giving the point count needed from each grid cell
  counts <- table(factor(samples, levels = 1:length(human.pop.vec)))
  
  assert_that(length(east.africa.grid) == length(counts))
  
  # Generate pseudo-absences, only targeting grid cells with positive counts 
  # to save computation time
  temp <- st_sample(
    east.africa.grid[counts > 0],
    size = counts[counts > 0]
  ) %>%
    st_intersection(., east.africa.lake.erase) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    )
  
  # Filter points to eliminate any within the outbreak buffer zone,
  # assuming there are outbreak points in that year
  if (length(outbreak.buffer) > 0) {
    
    temp <- st_difference(
      st_as_sf(temp, coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa)),
      outbreak.buffer
    ) %>%
      mutate(
        GPS_x = st_coordinates(.)[,1],
        GPS_y = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry()
  }
  
  assert_that(nrow(temp) >= n.points.per.yrmon * 12)
  
  # Subsample down to only the required number of points for a year
  temp <- slice_sample(temp, n = n.points.per.yrmon * 12) %>%
    mutate(
      outbreak_year = rep(year, times = n.points.per.yrmon * 12),
      outbreak_month = rep(month.name, each = n.points.per.yrmon)
    )
  
  human.points <- bind_rows(human.points, temp)
}

#==============================================================================


# Use an approach that is inversely related to travel time to healthcare


# Load in travel to healthcare raster
r <- terra::rast("data/rasters/healthcare/processed/healthcare_2.5min.tif")
plot(r)

# Make the East Africa map into a grid matching the extent and resolution of 
# the aggregated raster
east.africa.grid <- st_make_grid(
  east.africa, 
  n = c(dim(r)[2], dim(r)[1]),
  cellsize = res(r)
)

ggplot() + 
  geom_sf(data = east.africa.lake.erase, fill = "grey") +
  geom_sf(data = east.africa.grid, fill = NA) +
  theme_void()


# Generate pseudo-absence points in inverse proportion to the 
# healthcare travel time raster

travel.points <- data.frame()

set.seed(8)

for(year in 2008:2022) {
  
  # Generate a buffer representing RVF outbreak locations in the focal year
  outbreak.buffer <- d.sf %>%
    filter(outbreak_year == year) %>%
    st_buffer(dist = outbreak.buffer.radius * 2) %>%
    st_union()
  
  # Modify the healthcare travel time raster to sample in the inverse
  r.mod <- 1 / r
  
  # Generate a vector of inverse travel time values
  # (corresponding to the value of each raster grid cell) 
  travel.vec <- r.mod %>%
    # need to flip this raster vertically since "st_sample()" will start
    # sampling from the bottom left cell and move upwards rather than starting
    # from the upper left
    flip(direction = "vertical") %>%
    # extract values
    values() %>%
    # convert NA values to zeroes
    ifelse(is.na(.), 0, .)
  
  # With what probability should each grid cell be sampled?
  probs <- travel.vec / sum(travel.vec)
  # Sample grid cells in proportion to their probability
  samples <- sample(1:length(travel.vec), size = 10000, prob = probs, replace = TRUE)
  # Generate a vector giving the point count needed from each grid cell
  counts <- table(factor(samples, levels = 1:length(travel.vec)))
  
  assert_that(length(east.africa.grid) == length(counts))
  
  # Generate pseudo-absences, only targeting grid cells with positive travel 
  # values to save computation time
  temp <- st_sample(
    east.africa.grid[counts > 0],
    size = counts[counts > 0]
  ) %>%
    st_intersection(., east.africa.lake.erase) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(
      GPS_x = X,
      GPS_y = Y
    )
  
  # Filter points to eliminate any within the outbreak buffer zone,
  # assuming there are outbreak points in that year
  if (length(outbreak.buffer) > 0) {
    
    temp <- st_difference(
      st_as_sf(temp, coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa)),
      outbreak.buffer
    ) %>%
      mutate(
        GPS_x = st_coordinates(.)[,1],
        GPS_y = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry()
  }
  
  assert_that(nrow(temp) >= n.points.per.yrmon * 12)
  
  # Subsample down to only the required number of points for a year
  temp <- slice_sample(temp, n = n.points.per.yrmon * 12) %>%
    mutate(
      outbreak_year = rep(year, times = n.points.per.yrmon * 12),
      outbreak_month = rep(month.name, each = n.points.per.yrmon)
    )
  
  travel.points <- rbind(travel.points, temp)  
}

#==============================================================================


# Append pseudo-absences to outbreak data and visualize


d.random <- d %>%
  mutate(RVF_presence = rep(1, nrow(.))) %>%
  bind_rows(random.points) %>%
  mutate(RVF_presence = ifelse(is.na(RVF_presence), 0, RVF_presence)) 

d.random.sf <- d.random %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))
  
ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = d.random.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = filter(d.random.sf, outbreak_year == 2018), aes(color = as.factor(RVF_presence))) +
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
  facet_wrap(~outbreak_year)


d.doughnut <- d %>%
  mutate(RVF_presence = rep(1, nrow(.))) %>%
  bind_rows(doughnut.points) %>%
  mutate(RVF_presence = ifelse(is.na(RVF_presence), 0, RVF_presence)) 

d.doughnut.sf <- d.doughnut %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = d.doughnut.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = filter(d.doughnut.sf, outbreak_year == 2018), aes(color = as.factor(RVF_presence))) +
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
  facet_wrap(~outbreak_year)


d.popweighted <- d %>%
  mutate(RVF_presence = rep(1, nrow(.))) %>%
  bind_rows(human.points) %>%
  mutate(RVF_presence = ifelse(is.na(RVF_presence), 0, RVF_presence)) 

d.popweighted.sf <- d.popweighted %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa.lake.erase, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = d.popweighted.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa.lake.erase, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = filter(d.popweighted.sf, outbreak_year == 2018), aes(color = as.factor(RVF_presence))) +
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
  facet_wrap(~outbreak_year)


d.travel <- d %>%
  mutate(RVF_presence = rep(1, nrow(.))) %>%
  bind_rows(travel.points) %>%
  mutate(RVF_presence = ifelse(is.na(RVF_presence), 0, RVF_presence)) 

d.travel.sf <- d.travel %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = d.travel.sf, aes(color = as.factor(RVF_presence))) +
  scale_color_manual(values = c(alpha("gray", 0.2), "darkred")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "lightblue") +
  geom_sf(data = filter(d.travel.sf, outbreak_year == 2018), aes(color = as.factor(RVF_presence))) +
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
  facet_wrap(~outbreak_year)

#==============================================================================


# Save data

write_csv(d.random, file = "data/outbreak_data/outbreak_data_w_random_pseudoabsences.csv")
write_csv(d.popweighted, file = "data/outbreak_data/outbreak_data_w_popweighted_pseudoabsences.csv")
write_csv(d.travel, file = "data/outbreak_data/outbreak_data_w_travel_pseudoabsences.csv")
