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
monthly.predictors <- read_csv("data/predictor_flat_files/monthly_predictors_historical_weather.csv")


# Import outbreak/absence data and add on predictors

d.random <- read_csv("data/outbreak_data/outbreak_data_w_random_pseudoabsences.csv") %>%
  rename(
    year = outbreak_year,
    month = outbreak_month
  )

d.random$grid_cell <- cellFromXY(
  r,
  d.random %>%
    select(GPS_x, GPS_y) %>%
    as.matrix()
)

d.random <- d.random %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))


d.popweighted <- read_csv("data/outbreak_data/outbreak_data_w_popweighted_pseudoabsences.csv") %>%
  rename(
    year = outbreak_year,
    month = outbreak_month
  )

d.popweighted$grid_cell <- cellFromXY(
  r,
  d.popweighted %>%
    select(GPS_x, GPS_y) %>%
    as.matrix()
)

d.popweighted <- d.popweighted %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))


d.travel <- read_csv("data/outbreak_data/outbreak_data_w_travel_pseudoabsences.csv") %>%
  rename(
    year = outbreak_year,
    month = outbreak_month
  )

d.travel$grid_cell <- cellFromXY(
  r,
  d.travel %>%
    select(GPS_x, GPS_y) %>%
    as.matrix()
)

d.travel <- d.travel %>%
  left_join(., static.predictors, by = "grid_cell") %>%
  left_join(., yearly.predictors, by = c("grid_cell", "year")) %>%
  left_join(., monthly.predictors, by = c("grid_cell", "year", "month"))

#==============================================================================


d.all <- bind_rows(
  mutate(d.random, group = rep("random", nrow(d.random))),
  mutate(d.popweighted, group = rep("popweighted", nrow(d.popweighted))),
  mutate(d.travel, group = rep("travel", nrow(d.travel)))
)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = elevation), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = slope), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = travel_time_to_healthcare), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = human_pop), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = dist_to_lake_all), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = dist_to_river_10), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = cattle_density), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = goat_density), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = sheep_density), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = monthly_precip), color = alpha("black", 0.2)) +
  facet_wrap(~group)


ggplot() +
  geom_jitter(data = d.all, aes(x = RVF_presence, y = monthly_tmax), color = alpha("black", 0.2)) +
  facet_wrap(~group)

#==============================================================================


# Save data frames with extracted predictors

cols.to.exclude <- c(14:19, 23)

d.random %>%
  select(!all_of(cols.to.exclude)) %>%
  mutate(month_numeric = match(month, month.name)) %>%
  rename(
    longitude = GPS_x,
    latitude = GPS_y
  ) %>%
  write_csv(file = "data/outbreak_data/outbreak_data_w_random_pseudoabsences_predictors.csv")

d.popweighted %>%
  select(!all_of(cols.to.exclude)) %>%
  mutate(month_numeric = match(month, month.name)) %>%
  rename(
    longitude = GPS_x,
    latitude = GPS_y
  ) %>%
  write_csv(file = "data/outbreak_data/outbreak_data_w_popweighted_pseudoabsences_predictors.csv")

d.travel %>%
  select(!all_of(cols.to.exclude)) %>%
  mutate(month_numeric = match(month, month.name)) %>%
  rename(
    longitude = GPS_x,
    latitude = GPS_y
  ) %>%
  write_csv(file = "data/outbreak_data/outbreak_data_w_travel_pseudoabsences_predictors.csv")
