library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyterra)

source("R/functions.R")

# Generate GIFs of prediction maps

#==============================================================================


# Import key objects needed for plotting

# Import background outlines of Kenya, Tanzania, and Uganda and 
# large lakes layer
east.africa <- load_country_map()
lakes.10 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_10.rds")

# Import retrodictions from 2008-2021
files <- list.files(
  path = "data/prediction_rasters/",
  pattern = paste(2008:2021, collapse = "|"),
  full.names = TRUE
)

r <- terra::rast(files)
r.mask <- mask(r, east.africa)

# TSS cutoff
tss.cutoff <- readRDS("data/misc/tss.cutoff.rds")

# Define plotting limits
low <- -6
high <- 0

list.val <- list(
  "-6" = -6, 
  "-5" = -5,
  "-4" = -4,
  "-3" = -3,
  "-2" = -2,
  "0" = 0
)

#==============================================================================


# Generate images that will serve as the basis for the GIF

# Loop through all raster layers and plot each individually

for(i in 1:nlyr(r.mask)) {
  
  r.sub <- r.mask[[i]]
  
  name <- names(r.sub)
  year <- str_split(name, "_")[[1]][1]
  month <- month.name[as.numeric(str_split(name, "_")[[1]][2])]
  
  ggplot() +
    geom_spatraster(data = log10(r.sub)) +
    geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
    geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
    scale_fill_gradient2(
      low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
      midpoint = log10(tss.cutoff), na.value = NA,
      breaks = unlist(list.val), limits = c(low, high)
    ) +
    ggtitle(paste(year, month)) +
    theme_void() +
    theme(
      title = element_text(size = 16),
      legend.position = "none"
    )
  
  ggsave(
    filename = paste0("outputs/for_gif/", name, ".jpg"),
    height = 7, width = 5
  )
}

#==============================================================================


# Plot GIF using the generated image files

# List file names and read in
imgs <- list.files(
  "outputs/for_gif", 
  full.names = TRUE
)
img.list <- lapply(imgs, image_read)

# Join the images together
img.joined <- image_join(img.list)

# Animate at 2 frames per second
img.animated <- image_animate(img.joined, fps = 2)

# Save to disk
image_write(
  image = img.animated,
  path = "outputs/gif/test.gif"
)
