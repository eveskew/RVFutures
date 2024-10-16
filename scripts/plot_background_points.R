library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyterra)
library(cowplot)

sf_use_s2(FALSE)

source("R/functions.R")

#==============================================================================


# Generate a figure showing background point locations used in modeling

# Import background outlines of Kenya, Tanzania, and Uganda and 
# large lakes layer
east.africa <- load_country_map()
lakes.10 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_10.rds")

# Import the population-weighted background data
d.popweighted <- read_csv(
  file = "data/outbreak_data/data_popweighted_pseudoabsences.csv"
)

d.popweighted.sf <- d.popweighted %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

# Load in human population density raster files
files <- list.files(
  path = "data/rasters/human_population/processed",
  pattern = "pd",
  full.names = TRUE
)

# Subset to 2008-2020 (the period of observed data)
files <- files[str_detect(files, paste(as.character(2008:2020), collapse = "|"))]

# Import these rasters and take the mean
r <- terra::rast(files)
r.mean <- mean(r, na.rm = FALSE)

#==============================================================================


# Plot the background point data

# Map showing the background points on a white background
a <- d.popweighted.sf %>%
  filter(RVF_presence == 0) %>%
  ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.10, fill = "lightblue") +
  geom_sf(color = alpha("gray", 0.1), size = 0.5) +
  theme_void() +
  theme(
    legend.position = "none"
  )

# Map showing the mean population density values (2008-2020 average)
b <- ggplot() +
  geom_spatraster(data = r.mean) +
  geom_sf(data = east.africa, fill = NA) +
  geom_sf(data = lakes.10, fill = "lightblue") +
  scale_fill_viridis_c(trans = "log10", na.value = "white") +
  theme_void() +
  theme(
    legend.title = element_blank()
  )

plot_grid(
  a, b,
  labels = "auto"
)

ggsave("outputs/figures/background_points.jpg",
       width = 2000, height = 1000, units = "px")
