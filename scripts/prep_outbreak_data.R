library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeoboundaries)
library(assertthat)

source("R/functions.R")


# Prepare raw RVF outbreak data for use in modeling

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda
east.africa <- load_country_map()

# Import lakes layer
lakes.5 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds")

#==============================================================================


# Import and check outbreak data

# Import raw RVF outbreak data
d <- read_csv("data/outbreak_data/outbreak_data_raw.csv") %>%
  select(-exclude, -exclude_reason)

# Check to make sure that ADM1 and ADM2 names used are valid

# Collect all valid ADM1 names across countries
adm1.options <- geoboundaries(country = c("Kenya", "Tanzania", "Uganda"), "adm1") %>%
  sf::st_transform(., 4326) %>%
  pull(shapeName) %>%
  str_replace(., " Region", "")

# Collect ADM1 values in the outbreak dataset
adm1.values <- str_split(d$ADM1[!is.na(d$ADM1)], ", ") %>% 
  simplify() %>% 
  str_replace(., " Region| County", "")

assert_that(sum(adm1.values %in% adm1.options) == length(adm1.values))

# Collect all valid ADM2 names across countries
adm2.options1 <- geoboundaries(country = c("Kenya", "Tanzania"), "adm2") %>%
    sf::st_transform(., 4326)
adm2.options2 <- geoboundaries(country = "Uganda", "adm3") %>%
    sf::st_transform(., 4326)
adm2.options <- c(adm2.options1$shapeName, adm2.options2$shapeName)

# Collect ADM2 values in the outbreak dataset
adm2.values <- str_split(d$ADM2[!is.na(d$ADM2)], ", ") %>% 
  simplify() %>% 
  str_replace(., " District| Subcounty", "")

assert_that(sum(adm2.values %in% adm2.options) == length(adm2.values))

# Check repeated use of the same GPS coordinates
d %>%
  group_by(GPS_x, GPS_y) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
# There are two valid instances where GPS coordinates were repeated for
# sampling at the same location at different times

# Make sure that outbreak year and month agree with the given 
# outbreak start date
assert_that(sum(year(d$start_date) == d$outbreak_year) == nrow(d))
assert_that(sum(month.name[month(d$start_date)] == d$outbreak_month) == nrow(d))

# Make sure outbreak type is categorized correctly
table(d$human_cases, d$livestock_cases, d$outbreak_type)
filter(d, human_cases == 1 & livestock_cases == 0 & is.na(n_human_cases))
# There is one livestock-only outbreak with an unknown outbreak size
filter(d, human_cases == 0 & livestock_cases == 1 & is.na(n_livestock_cases))
# There are four human and livestock outbreaks where the human case count
# is unclear
filter(d, human_cases == 1 & livestock_cases == 1 & is.na(n_human_cases))
filter(d, human_cases == 1 & livestock_cases == 1 & is.na(n_livestock_cases))

# Make sure that human and livestock case counts add up to the recorded 
# total case count
assert_that(
  sum(
    rowSums(select(d, n_human_cases, n_livestock_cases), na.rm = TRUE) == 
  ifelse(is.na(d$n_total_cases), 0, d$n_total_cases)
  ) == nrow(d)
)

#==============================================================================


# Prep a version of the outbreak data where missing coordinates are filled with
# centroids of the known ADM locations

d.centroid <- d %>%
  pull_centroids_from_adm() %>%
  sf::st_as_sf(
    coords = c("GPS_x", "GPS_y"),
    crs = sf::st_crs(4326)
  )

assert_that(nrow(d.centroid) == nrow(d))

# Prep a version of the outbreak data where all data are duplicated and 
# coordinates for missing GPS locations are filled randomly from the known
# ADM locations

set.seed(8)

d.expanded <- d %>%
  # replicate all outbreak data points 10 times
  bind_rows(replicate(n = 9, ., simplify = FALSE)) %>%
  pull_random_points_from_adm() %>%
  sf::st_as_sf(
    coords = c("GPS_x", "GPS_y"),
    crs = sf::st_crs(4326)
  )

assert_that(nrow(d.expanded) == nrow(d) * 10)

#==============================================================================


# Visualize the new outbreak datasets

d.centroid %>%
  ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "skyblue") +
  geom_sf(col = alpha("darkred", 0.4)) 

d.expanded %>%
  ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_sf(data = lakes.5, fill = "skyblue") +
  geom_sf(col = alpha("darkred", 0.4)) 

#==============================================================================


# Save the new outbreak datasets

d.centroid %>%
  mutate(
    GPS_x = st_coordinates(.)[,1],
    GPS_y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  write_csv(., file = "data/outbreak_data/outbreak_data_centroid_filled.csv")

d.expanded %>%
  mutate(
    GPS_x = st_coordinates(.)[,1],
    GPS_y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  write_csv(., file = "data/outbreak_data/outbreak_data_randomly_filled.csv")
