library(tidyverse)
library(terra)
library(tidyterra)

source("R/functions.R")

# Calculate human population at risk of RVF in the study region 
# (Kenya, Tanzania, and Uganda) under future climate scenarios

#==============================================================================


# Import key data

# Map layer
east.africa <- load_country_map()

# TSS cutoff
tss.cutoff <- readRDS("data/misc/tss.cutoff.rds")

# Prediction rasters
files <- list.files(
  path = "data/prediction_rasters",
  full.names = TRUE
)

# Masked rasters
r.mask <- mask(rast(files), east.africa)

# Thresholded, masked rasters
r.threshold <- r.mask >= tss.cutoff

#==============================================================================


# Select masked predictions for all future climate scenarios 
r.mask.future <- r.mask %>%
  select(matches("SSP"))
assertthat::assert_that(dim(r.mask.future)[3] == 2484)

# Add on masked predictions for historical climate
r.mask.sub <- c(
  r.mask %>%
    select(matches("historical_climate")) %>%
    select(!matches("sensitivity")),
  r.mask.future
)
assertthat::assert_that(dim(r.mask.sub)[3] == 2496)


# Select thresholded predictions for all future climate scenarios 
r.threshold.future <- r.threshold %>%
  select(matches("SSP"))
assertthat::assert_that(dim(r.threshold.future)[3] == 2484)

# Add on thresholded predictions for historical climate
r.threshold.sub <- c(
  r.threshold %>%
    select(matches("historical_climate")) %>%
    select(!matches("sensitivity")),
  r.threshold.future
)
assertthat::assert_that(dim(r.threshold.sub)[3] == 2496)

#==============================================================================


# Average across months in the masked raster stack to get rasters that 
# indicate the yearly mean risk for each scenario
collapse.months.mask <- tapp(
  r.mask.sub,
  fun = "mean",
  index = rep(1:(dim(r.mask.sub)[3]/12), each = 12)
)
names(collapse.months.mask) <- 
  unique(str_replace(names(r.mask.sub), "_[0-9]+$", ""))
assertthat::assert_that(dim(collapse.months.mask)[3] == 208)


# Sum across months to get rasters that indicate the number of months a 
# year that a grid cell is suitable for RVF
collapse.months.threshold <- tapp(
  r.threshold.sub,
  fun = "sum",
  index = rep(1:(dim(r.threshold.sub)[3]/12), each = 12)
)
names(collapse.months.threshold) <- 
  unique(str_replace(names(r.threshold.sub), "_[0-9]+$", ""))
assertthat::assert_that(dim(collapse.months.threshold)[3] == 208)

# Apply a threshold to these layers to identify the grid cells that we will
# call "exposed" to RVF
months.per.year.threshold <- 6
annual.risk.areas <- collapse.months.threshold > months.per.year.threshold

#==============================================================================


# Import human population density data, making sure to mask to the study region
files <- list.files(
  path = "data/rasters/human_population/processed/",
  full.names = TRUE
)

r.human.pop <- mask(rast(files), east.africa)

# Create a table to hold all info
d <- data.frame(
  projection_layer = names(annual.risk.areas)
) %>%
  mutate(
    human_pop_layer = case_when(
      str_detect(projection_layer, "historical_climate_1985") ~ "pd_2000",
      str_detect(projection_layer, "sensitivity_historical_humanpop") ~ "pd_2000",
      str_detect(projection_layer, "SSP126_2030") ~ "SSP1_2030",
      str_detect(projection_layer, "SSP245_2030") ~ "SSP2_2030",
      str_detect(projection_layer, "SSP370_2030") ~ "SSP3_2030",
      str_detect(projection_layer, "SSP126_2050") ~ "SSP1_2050",
      str_detect(projection_layer, "SSP245_2050") ~ "SSP2_2050",
      str_detect(projection_layer, "SSP370_2050") ~ "SSP3_2050",
      str_detect(projection_layer, "SSP126_2070") ~ "SSP1_2070",
      str_detect(projection_layer, "SSP245_2070") ~ "SSP2_2070",
      str_detect(projection_layer, "SSP370_2070") ~ "SSP3_2070"
    ),
    scenario = case_when(
      str_detect(projection_layer, "SSP126") ~ "SSP126",
      str_detect(projection_layer, "SSP245") ~ "SSP245",
      str_detect(projection_layer, "SSP370") ~ "SSP370",
      TRUE ~ "historical"
    ),
    sensitivity = ifelse(str_detect(projection_layer, "sensitivity"), 1, 0),
    year = str_extract(projection_layer, "_[0-9]+$") %>%
      str_replace("_", "") %>%
      as.numeric()
  )

# Calculate the average RVF risk across the landscape
d$mean_RVF_relative_likelihood <- global(
  collapse.months.mask, "mean", na.rm = TRUE
) %>%
  unlist()

# Calculate the total human population size across all scenarios
r.human.pop.scenarios <- r.human.pop[[d$human_pop_layer]]
d$total_pop_size <- global(
  r.human.pop.scenarios * cellSize(r.human.pop.scenarios, unit = "km"), 
  "sum", na.rm = TRUE
) %>%
  unlist()

# Combine annual risk layers with human population density layers to get
# rasters showing the human population density for only at-risk areas
assertthat::assert_that(dim(annual.risk.areas)[3] == dim(r.human.pop.scenarios)[3])
r.pop.density.at.risk <- annual.risk.areas * r.human.pop.scenarios

# Calculate the total human population size at risk across all scenarios
d$pop_size_at_risk <- global(
  r.pop.density.at.risk * cellSize(r.pop.density.at.risk, unit = "km"),
  "sum", na.rm = TRUE
) %>%
  unlist()

# Calculate proportion of the study region's population at risk
d$prop_at_risk <- d$pop_size_at_risk/d$total_pop_size


# Save data
write_csv(d, file = "data/misc/human_pop_at_risk.csv")

#==============================================================================


# Plotting and summary tables

d %>%
  filter(sensitivity == 0 | projection_layer == "historical_climate_1985") %>%
  ggplot(aes(x = year, y = mean_RVF_relative_likelihood, color = scenario)) +
  geom_point() +
  xlim(1980, 2080) +
  theme_minimal()

d %>%
  filter(sensitivity == 0 | projection_layer == "historical_climate_1985") %>%
  ggplot(aes(x = year, y = prop_at_risk, color = scenario)) +
  geom_point() +
  xlim(1980, 2080) +
  theme_minimal()

d %>%
  filter(sensitivity == 0 | projection_layer == "historical_climate_1985") %>%
  ggplot(aes(x = year, y = pop_size_at_risk, color = scenario)) +
  geom_point() +
  xlim(1980, 2080) +
  theme_minimal()


d %>%
  filter(sensitivity == 0) %>%
  group_by(year, scenario) %>%
  summarize(
    total_pop_size = mean(total_pop_size),
    mean_pop_size_at_risk = mean(pop_size_at_risk),
    min_pop_size_at_risk = min(pop_size_at_risk),
    max_pop_size_at_risk = max(pop_size_at_risk),
    mean_prop_at_risk = mean(prop_at_risk),
    min_prop_at_risk = min(prop_at_risk),
    max_prop_at_risk = max(prop_at_risk)
  )
