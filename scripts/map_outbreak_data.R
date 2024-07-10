library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyterra)

sf_use_s2(FALSE)

source("R/functions.R")

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda

east.africa <- load_country_map()

ggplot() + 
  geom_sf(data = east.africa)

#==============================================================================


# Import hydrology data 
lakes <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds")
rivers <- readRDS("data/rasters/hydrology/saved_objects/rivers_east_africa.rds")

# Generate a new variable in the data frame to be used to plot the width
# of rivers of different flow orders. The "m" variable is a multiplier to
# make this easy to fiddle with, keeping the relative size among river orders
# the same
m <- 0.7
rivers <- rivers %>%
  mutate(
    width = case_when(
      ORD_FLOW == 3 ~ 1 * m,
      ORD_FLOW == 4 ~ 0.8 * m,
      ORD_FLOW == 5 ~ 0.6 * m,
      ORD_FLOW == 6 ~ 0.4 * m,
      ORD_FLOW == 7 ~ 0.2 * m,
      ORD_FLOW == 8 ~ 0.2 * m,
      ORD_FLOW == 9 ~ 0.1 * m,
      ORD_FLOW == 10 ~ 0.1 * m,
      TRUE ~ 0
    )
  )

# Generate alpha values corresponding to different river orders. A value of 0
# will effectively make rivers of that order invisible
alpha.values <- c(
  "3" = 0.85, 
  "4" = 0.6, 
  "5" = 0.4, 
  "6" = 0.3,
  "7" = 0.2, 
  "8" = 0.05, 
  "9" = 0, 
  "10" = 0
)

ggplot() +
  geom_sf(
    data = east.africa, 
    fill = alpha("darkseagreen", 0.2)
  ) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    color = alpha("cornflowerblue", 0.9)
  ) +
  geom_sf(
    data = rivers,
    aes(alpha = factor(ORD_FLOW)),
    color = "cornflowerblue",
    linewidth = rivers$width
  ) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  theme(legend.position = "none")

#==============================================================================


# Get all elevation raster file names
files <- list.files(
  path = "data/rasters/elevation/SRTM",
  full.names = TRUE
)

# Create VRT and cropped VRT layers
vrt <- vrt(files)
crs(vrt) <- st_crs(east.africa)$proj4string
vrt.crop <- terra::crop(vrt, east.africa, mask = TRUE)

plot(vrt)
plot(vrt.crop)

# Calculate slope, aspect, and hillshade using the elevation data
sl.radians <- terrain(vrt.crop, v = "slope", unit = "radians")
saveRDS(sl.radians, file = "data/rasters/elevation/saved_objects/sl_radians.rds")
sl.radians <- readRDS("data/rasters/elevation/saved_objects/sl_radians.rds")

asp.radians <- terrain(vrt.crop, v = "aspect", unit = "radians")
saveRDS(asp.radians, file = "data/rasters/elevation/saved_objects/asp_radians.rds")
asp.radians <- readRDS("data/rasters/elevation/saved_objects/asp_radians.rds")

hill.single <- shade(
  sl.radians, asp.radians,
  angle = 45,
  direction = 315,
  normalize = TRUE
)
saveRDS(hill.single, file = "data/rasters/elevation/saved_objects/hill_single.rds")
hill.single <- readRDS("data/rasters/elevation/saved_objects/hill_single.rds")
plot(hill.single, col = grey(1:100/100))

hill.multi <- purrr::map(
  c(270, 15, 60, 330), function(dir) {
    shade(
      sl.radians, asp.radians,
      angle = 45,
      direction = dir,
      normalize = TRUE
    )
  }
)
hill.multi <- hill.multi %>%
  rast() %>%
  sum()
saveRDS(hill.multi, file = "data/rasters/elevation/saved_objects/hill_multi.rds")
hill.multi <- readRDS("data/rasters/elevation/saved_objects/hill_multi.rds")
plot(hill.multi, col = grey(1:100/100))

#==============================================================================


# Import RVF outbreak data

d <- read_csv("data/outbreak_data/EC_RVF_clean_April27.csv") %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

#==============================================================================


# Plot RVF outbreak data

cuts <- c(100, 250, 500, 1000, 2000, 4000, 6000, 9000)
x <- 2000


# Plot a map with the elevation and hydrology layers
ggplot() +
  geom_sf(
    data = east.africa,
    fill = "white",
    linewidth = NA
  ) +
  geom_spatraster(
    data = vrt.crop,
    alpha = 0.6
  ) +
  scale_fill_hypso_tint_c(breaks = cuts) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    color = alpha("cornflowerblue", 0.9)
  ) +
  geom_sf(
    data = rivers,
    aes(alpha = factor(ORD_FLOW)),
    color = "cornflowerblue",
    linewidth = rivers$width
  ) +
  geom_sf(
    data = east.africa,
    fill = NA,
    linewidth = 1
  ) +
  geom_sf(data = d, aes(size = CASES), color = alpha("darkred", 0.5)) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  theme(legend.position = "none")

ggsave("outputs/outbreak_maps/RVF_outbreaks_elevation_hydro_map.jpg", 
       width = x, height = x*1.4, units = "px")


# Plot a map with the hillshade, elevation, and hydrology layers
ggplot() +
  geom_sf(
    data = east.africa,
    fill = "white"
  ) +
  geom_spatraster(
    data = hill.multi,
    show.legend = FALSE
  ) +
  scale_fill_distiller(palette = "Greys", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_spatraster(
    data = vrt.crop,
    alpha = 0.6
  ) +
  scale_fill_hypso_tint_c(breaks = cuts) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    color = alpha("cornflowerblue", 0.9)
  ) +
  geom_sf(
    data = rivers,
    aes(alpha = factor(ORD_FLOW)),
    color = "cornflowerblue",
    linewidth = rivers$width
  ) +
  geom_sf(
    data = east.africa,
    fill = NA,
    linewidth = 1
  ) +
  geom_sf(data = d, aes(size = CASES), color = alpha("darkred", 0.5)) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  theme(legend.position = "none")

ggsave("outputs/outbreak_maps/RVF_outbreaks_hillshade_elevation_hydro_map.jpg", 
       width = x, height = x*1.4, units = "px")


ggplot() +
  geom_sf(data = east.africa, fill = alpha("darkseagreen", 0.2)) +
  geom_sf(data = d, aes(size = CASES), color = alpha("darkred", 0.5)) +
  facet_wrap(~OB_Yr, nrow = 3) +
  theme_minimal()
