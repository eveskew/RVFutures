library(tidyverse)
library(sf)
library(terra)

source("R/functions.R")

#==============================================================================


# Import background outlines of Kenya, Tanzania, and Uganda

east.africa <- load_country_map()


# Import raster of the correct dimensions for reference

r <- rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")


# Import predictor files

static.predictors <- read_csv("data/predictor_flat_files/static_predictors.csv")
yearly.predictors <- read_csv("data/predictor_flat_files/yearly_predictors_historical.csv")
monthly.predictors <- read_csv("data/predictor_flat_files/monthly_predictors_historical.csv")


# Import outbreak/absence data and add on predictors

d.random <- read_csv("data/outbreak_data/data_random_pseudoabsences.csv") %>%
  rename(
    year = OB_Yr,
    month = OB_Mo,
    x = GPS_x,
    y = GPS_y
  )

d.random$grid_cell <- cellFromXY(
  r,
  d.random %>%
    select(x, y) %>%
    as.matrix()
)

d.random <- d.random %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))


d.popweighted <- read_csv("data/outbreak_data/data_popweighted_pseudoabsences.csv") %>%
  rename(
    year = OB_Yr,
    month = OB_Mo,
    x = GPS_x,
    y = GPS_y
  )

d.popweighted$grid_cell <- cellFromXY(
  r,
  d.popweighted %>%
    select(x, y) %>%
    as.matrix()
)

d.popweighted <- d.popweighted %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))

#==============================================================================


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = elevation))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = elevation))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = slope))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = slope))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = travel_time_to_healthcare))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = travel_time_to_healthcare))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = human_pop))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = human_pop))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = dist_to_lake_all))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = dist_to_lake_all))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = dist_to_river_10))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = dist_to_river_10))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = cattle_density))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = cattle_density))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = goat_density))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = goat_density))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = sheep_density))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = sheep_density))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = monthly_precip))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = monthly_precip))


ggplot() +
  geom_jitter(data = d.random, aes(x = RVF_presence, y = monthly_temp))

ggplot() +
  geom_jitter(data = d.popweighted, aes(x = RVF_presence, y = monthly_temp))

#==============================================================================


# Save data frames with extracted predictors

d.random %>%
  mutate(
    month_numeric = match(month, month.name),
    longitude = x,
    latitude = y
  ) %>%
  write_csv(file = "data/outbreak_data/outbreak_random_predictors.csv")

d.popweighted %>%
  mutate(
    month_numeric = match(month, month.name),
    longitude = x,
    latitude = y
  ) %>%
  write_csv(file = "data/outbreak_data/outbreak_popweighted_predictors.csv")
