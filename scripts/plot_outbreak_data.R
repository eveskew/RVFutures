library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyterra)
library(ggspatial)

sf_use_s2(FALSE)

source("R/functions.R")

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda

east.africa <- load_country_map()
east.africa.adm <- load_country_map_adm()
east.africa.box <- st_as_sfc(st_bbox(east.africa))

ggplot() + 
  geom_sf(data = east.africa)
ggplot() + 
  geom_sf(data = east.africa.adm)

#==============================================================================


# Import hydrology data 
lakes <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds")
rivers <- readRDS("data/rasters/hydrology/saved_objects/rivers_east_africa.rds")
oceans <- rnaturalearth::ne_download(
  category = "physical",
  type = "ocean",
  scale = "medium"
) %>%
  st_transform(4326)
oceans <- st_crop(oceans, east.africa)

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
    data = east.africa.box,
    fill = "floralwhite",
    linewidth = NA
  ) +
  geom_sf(
    data = east.africa,
    fill = alpha("darkseagreen", 0.2)
  ) +
  geom_sf(
    data = oceans,
    fill = "cornflowerblue",
    linewidth = NA
  ) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    linewidth = NA
  ) +
  geom_sf(
    data = rivers,
    aes(alpha = factor(ORD_FLOW)),
    color = "cornflowerblue",
    linewidth = rivers$width
  ) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  theme(
    legend.position = "none"
  )

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

asp.radians <- terrain(vrt.crop, v = "aspect", unit = "radians")

hill.single <- shade(
  sl.radians, asp.radians,
  angle = 45,
  direction = 315,
  normalize = TRUE
)

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

plot(hill.multi, col = grey(1:100/100))

#==============================================================================


# Import RVF outbreak data

d <- read_csv("data/outbreak_data/outbreak_data_centroid_filled.csv") %>%
  st_as_sf(coords = c("GPS_x", "GPS_y"), crs = st_crs(east.africa))

#==============================================================================


# Plot RVF outbreak data

width <- 7
x.dim <- st_bbox(east.africa)[3] - st_bbox(east.africa)[1]
y.dim <- st_bbox(east.africa)[4] - st_bbox(east.africa)[2]
yx.ratio <- round(y.dim/x.dim, digits = 1)

labels <- data.frame(
  x = c(37, 34.8, 33.2),
  y = c(1.4, -6.4, 2.7),
  label = c("Kenya", "Tanzania", "Uganda")
)


# Plot a map with the elevation and hydrology layers
ggplot() +
  geom_sf(
    data = east.africa.box,
    fill = "floralwhite",
    linewidth = NA
  ) +
  geom_spatraster(
    data = vrt.crop,
    alpha = 0.6
  ) +
  scale_fill_hypso_tint_c(limits = c(0, 6000)) +
  geom_sf(
    data = oceans,
    fill = "cornflowerblue",
    linewidth = NA
  ) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    linewidth = NA
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
  geom_label(
    data = labels, 
    aes(x = x, y = y, label = label),
    fill = alpha("ghostwhite", 0.7), 
    size = 6
  ) +
  geom_sf(data = d, size = 4, color = alpha("darkred", 0.5)) +
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_cex = 1,
    pad_x = unit(0.5, "inch"),
    pad_y = unit(0.6, "inch")
  ) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    height = unit(0.5, "inch"),
    width = unit(0.4, "inch"),
    pad_x = unit(0.98, "inch"), 
    pad_y = unit(0.9, "inch"),
    style = north_arrow_orienteering(
      fill = c("black", "black")
    )
  ) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  guides(
    size = "none", 
    alpha = "none",
    fill = guide_colorbar(title = "Elevation\n(metres)")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.885, 0.32),
    legend.key.height = unit(0.5, "inch"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

filename <- "outputs/figures/RVF_outbreaks_elevation_hydro_map.jpg"
ggsave(filename, width = width, height = width * yx.ratio, units = "in")
knitr::plot_crop(filename)


# Plot a map with the hillshade, elevation, and hydrology layers
ggplot() +
  geom_sf(
    data = east.africa.box,
    fill = "floralwhite",
    linewidth = NA
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
  scale_fill_hypso_tint_c(limits = c(0, 6000)) +
  geom_sf(
    data = oceans,
    fill = "cornflowerblue",
    linewidth = NA
  ) +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    linewidth = NA
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
  geom_sf(data = d, size = 4, color = alpha("darkred", 0.5)) +
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_cex = 1,
    pad_x = unit(0.5, "inch"),
    pad_y = unit(0.6, "inch")
  ) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    height = unit(0.5, "inch"),
    width = unit(0.4, "inch"),
    pad_x = unit(0.98, "inch"), 
    pad_y = unit(0.9, "inch"),
    style = north_arrow_orienteering(
      fill = c("black", "black")
    )
  ) +
  scale_alpha_manual(values = alpha.values) +
  theme_void() +
  guides(
    size = "none", 
    alpha = "none",
    fill = guide_colorbar(title = "Elevation\n(meters)")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.885, 0.32),
    legend.key.height = unit(0.5, "inch"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

filename <- "outputs/figures/RVF_outbreaks_hillshade_elevation_hydro_map.jpg"
ggsave(filename, width = width, height = width * yx.ratio, units = "in")
knitr::plot_crop(filename)


ggplot() +
  geom_sf(data = east.africa, fill = alpha("darkseagreen", 0.2)) +
  geom_sf(data = d, size = 2, color = alpha("darkred", 0.5)) +
  facet_wrap(~outbreak_year, nrow = 3) +
  theme_minimal()


# Plot a map with admin level 1 outlines
ggplot() +
  geom_sf(
    data = lakes,
    fill = "cornflowerblue",
    linewidth = NA
  ) +
  geom_sf(
    data = east.africa.adm,
    fill = NA,
    linewidth = 0.5
  ) +
  geom_sf(data = d, size = 4, color = alpha("darkred", 0.5)) +
  theme_void() +
  theme(legend.position = "none")

filename <- "outputs/figures/RVF_outbreaks_admin1_map.jpg"
ggsave(filename, width = width, height = width * yx.ratio, units = "in")
knitr::plot_crop(filename)
