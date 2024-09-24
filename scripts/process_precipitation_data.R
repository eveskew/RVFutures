library(tidyverse)
library(terra)
library(tidyterra)
library(assertthat)

source("R/functions.R")

#==============================================================================


# Load in the precipitation historical weather data and crop to the relevant 
# country extents

east.africa <- load_country_map()

files <- list.files(
  path = "data/rasters/precipitation/WorldClim_historical_weather"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/precipitation/WorldClim_historical_weather/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names(crop) <- str_replace(i, "\\.tif", "")
  
  # Save the cropped raster
  if(!dir.exists("data/rasters/precipitation/processed")) {
    dir.create("data/rasters/precipitation/processed")
  }
  
  writeRaster(
    crop, 
    paste0("data/rasters/precipitation/processed/", i),
    overwrite = TRUE
  )
}

#==============================================================================


# Load in the precipitation historical climate data and crop to the relevant 
# country extents

files <- list.files(
  path = "data/rasters/precipitation/WorldClim_historical_climate",
  pattern = ".tif$"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/precipitation/WorldClim_historical_climate/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names(crop) <- i %>% 
    str_replace("\\.tif", "") %>%
    str_replace("prec_", "prec_1970-2000-")
  
  writeRaster(
    crop, 
    paste0("data/rasters/precipitation/processed/", names(crop), ".tif"),
    overwrite = TRUE
  )
}

#==============================================================================


# Load in the cropped precipitation raster data, making sure to only grab the
# cropped raw data files. Effectively, we'll get a raster stack with each 
# month's data as a layer
files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = "wc.*_200[8-9]|wc.*_201[0-9]|wc.*_202[0-3]",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 14 * 12)


# Plot and save the precipitation raster data
p <- ggplot() +
  geom_spatraster(data = r) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, ncol = 12) +
  theme_void()

if(!dir.exists("outputs/predictor_layers")) {
  dir.create("outputs/predictor_layers")
}

ggsave(
  p,
  filename = "outputs/predictor_layers/precipitation.jpg",
  width = 8000,
  height = 14000,
  units = "px"
)

#==============================================================================


# Generate future projections of precipitation on a by-month basis using the
# existing weather data and a linear projection method
files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = "wc.*_200[0-9]|wc.*_201[0-9]|wc.*_202[0-3]",
  full.names = TRUE
)

r <- rast(files)
assert_that(dim(r)[3] == 22 * 12)

months <- c(
  "01", "02", "03", "04", "05", "06", 
  "07", "08", "09", "10", "11", "12"
)

first.data.year <- 2000

for(month in months) {
  
  # Subset the raster stack to only one particular month across time
  r.sub <- r %>%
    select(matches(paste0("-", month)))
  
  # Regress across the years
  x <- regress(r.sub, 1:nlyr(r.sub), na.rm = TRUE)
  
  # Generate raster layers for 2024-2050 and save them
  for(year in 2024:2050) {
    
    writeRaster(
      generate_raster_projection(
        regression_raster = x,
        first_year_of_data = first.data.year,
        projection_year = year,
        lower_clamp = 0,
        layer_name = paste0("linear_projection_", year, "-", month)
      ), 
      paste0("data/rasters/precipitation/processed/linear_projection_2.5min_", year, "-", month, ".tif"),
      overwrite = TRUE
    )
  }
}

#==============================================================================


# Load in the cropped precipitation raster data, effectively getting a 
# raster stack with each month's data as a layer
obs.files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = "wc.*_200[0-9]|wc.*_201[0-9]|wc.*_202[0-3]",
  full.names = TRUE
)
proj.files <- list.files(
  path = "data/rasters/precipitation/processed/",
  pattern = "linear_projection",
  full.names = TRUE
)
files <- c(obs.files, proj.files)

r <- rast(files)
assert_that(dim(r)[3] == 49 * 12)

r.sub <- r %>%
  select(matches("2000|2010|2020|2030|2040|2050"))

# Plot and save the precipitation raster data
p <- ggplot() +
  geom_spatraster(data = r.sub) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, ncol = 12) +
  theme_void()

ggsave(
  p,
  filename = "outputs/predictor_layers/precipitation_projected.jpg",
  width = 8000,
  height = 6000,
  units = "px"
)

#==============================================================================


# Check linearly projected data

# Subset to only January data (arbitrary)
r.sub <- r %>%
  select(matches("-01"))

facet.names <- c(
  `wc2.1_2.5m_prec_2010-01` = "January 2010",
  `wc2.1_2.5m_prec_2020-01` = "January 2020",
  `linear_projection_2025-01` = "January 2025",
  `linear_projection_2030-01` = "January 2030",
  `linear_projection_2035-01` = "January 2035",
  `linear_projection_2040-01` = "January 2040",
  `linear_projection_2045-01` = "January 2045",
  `linear_projection_2050-01` = "January 2050"
)

ggplot() +
  geom_spatraster(data = select(r.sub, matches("2010|2020|2025|2030|2035|2040|2045|2050"))) +
  scale_fill_distiller(palette = "Spectral", na.value = "white") +
  facet_wrap(~lyr, ncol = 5, labeller = as_labeller(facet.names)) +
  theme_void() +
  theme(
    text = element_text(size = 20)
  )

# ggsave("precipitation_projection_example.jpg", units = "px", width = 3000, height = 1000)

# Make the data tidy
tidy <- r.sub %>%
  # extract data for every cell
  terra::extract(1:(dim(r.sub)[1]*dim(r.sub)[2])) %>% 
  # pivot the data longer
  tidyr::pivot_longer(everything(), names_to = "layer", values_to = "rainfall") %>%
  # name the pixels and assign year variable
  mutate(
    pixel = rep(1:(dim(r.sub)[1]*dim(r.sub)[2]), each = 49),
    year = str_extract(layer, "_[1-9]...-") %>%
      str_replace("_", "") %>%
      str_replace("-", "") %>%
      as.numeric()
  )

# Grab just one pixel's data
test.dat <- tidy %>%
  filter(pixel == 70000)

# Plot observed data, the trend line, and the projections made for that pixel
ggplot() +
  geom_point(data = test.dat[1:22, ], aes(x = year, y = rainfall)) +
  geom_smooth(data = test.dat[1:22, ], aes(x = year, y = rainfall), method = "lm", se = FALSE, col = "black") +
  geom_point(data = test.dat[23:49, ], aes(x = year, y = rainfall), col = "red") +
  geom_line(data = test.dat[23:49, ], aes(x = year, y = rainfall), col = "red", lty = 2) +
  ggtitle("January precipitation through time (raster pixel 70000)") +
  ylab("Precipitation (mm)") +
  theme_minimal()

# ggsave("precipitation_model_example.jpg", units = "px", width = 2000, height = 1000)

# Fit a linear model on the observed data from this pixel to confirm the projected
# values
m <- lm(rainfall ~ year, data = test.dat[1:22, ])
predict(m, newdata = data.frame(year = 2024:2050))
test.dat[23:49, ]

#==============================================================================


# Load in the precipitation future climate data and crop to the relevant 
# country extents

files <- list.files(
  path = "data/rasters/precipitation/WorldClim_future_climate"
)

# Loop through all files
for(i in files) {
  
  # Import the raster
  r <- rast(paste0("data/rasters/precipitation/WorldClim_future_climate/", i))
  
  # Reproject the raster to EPSG 4326, if needed
  if(crs(r, describe = TRUE)$code != "4326" | is.na(crs(r, describe = TRUE)$code)) {r <- project(x = r, y = "epsg:4326")}
  
  # Crop the raster
  crop <- terra::crop(r, east.africa)
  
  # Rename the raster
  names <- i %>%
    str_replace("\\.tif", "") %>%
    str_replace("2021-2040", "2030") %>%
    str_replace("2041-2060", "2050") %>%
    str_replace("2061-2080", "2070")
  names <- paste0(names, "-", months)
  names(crop) <- names
  
  # Save the cropped rasters
  for(name in names) {
    
    writeRaster(
      crop[[name]], 
      paste0("data/rasters/precipitation/processed/", name, ".tif"),
      overwrite = TRUE
    )
  }
}

#==============================================================================


# Plot total precipitation over time for different SSP scenarios

# Import historical observations and future projections
files <- list.files(
  path = "data/rasters/precipitation/processed",
  full.names = TRUE
)
files <- files[!str_detect(files, "linear|1970-2000")]

r <- rast(files)

# Generate data frame to track changes in precipitation variables over time
d <- data.frame(
  year = as.numeric(str_extract(files, "[0-9]{4}")),
  month = rep(months, times = 121),
  type = ifelse(
    is.na(str_extract(files, "ssp[0-9]{3}")), 
    "Historical",
    str_extract(files, "ssp[0-9]{3}")
  ),
  gcm = str_extract(files, "(?<=prec_)[^,]+(?=_ssp)"),
  median_value = NA,
  mean_value = NA,
  total_value = NA
) %>%
  mutate(
    type = str_replace(type, "ssp", "SSP"),
  )

# Fill in the data frame
values <- values(r)
d$median_value <- apply(values, 2, median, na.rm = TRUE)
d$mean_value <- apply(values, 2, mean, na.rm = TRUE)
d$total_value <- apply(values, 2, sum, na.rm = TRUE)

# Plot total precipitation values over time
gcms <- d %>%
  distinct(gcm) %>%
  filter(!is.na(gcm)) %>%
  pull(gcm)

for(g in gcms) {
  
  p <- d %>%
    filter(gcm == g | is.na(gcm)) %>%
    ggplot(aes(x = year, y = mean_value, group = type, color = type)) +
    geom_point() +
    geom_line(linewidth = 0.2) +
    geom_vline(xintercept = 2021, linetype = 2) +
    xlab("") +
    ylab("Mean precipitation across study region") +
    ylim(0, 300) +
    ggtitle(g) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    ) +
    facet_wrap(~month)
  
  ggsave(
    p,
    filename = paste0("outputs/predictor_layers/precipitation_", g, ".jpg"),
    width = 3000,
    height = 2000,
    units = "px"
  ) 
}
