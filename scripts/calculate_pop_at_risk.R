library(tidyverse)
library(terra)
library(tidyterra)

source("R/functions.R")

# Calculate human population at risk of RVF in the study region (Kenya, Uganda,
# and Tanzania) under future climate scenarios

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


# Select thresholded predictions for all future climate scenarios 
r.threshold.future <- r.threshold %>%
  select(matches("SSP"))
assertthat::assert_that(dim(r.threshold.future)[3] == 11 * 12 * 3 *3)

# Add on thresholded predictions for historical climate
r.threshold.sub <- c(
  select(r.threshold, matches("historical_climate")),
  r.threshold.future
)
assertthat::assert_that(dim(r.threshold.sub)[3] == 12 + (11 * 12 * 3 * 3))

# Sum across months to get rasters that indicate the number of months a 
# year that a grid cell is suitable for RVF
collapse.months <- tapp(
  r.threshold.sub,
  fun = "sum",
  index = rep(1:(dim(r.threshold.sub)[3]/12), each = 12)
)
names(collapse.months) <- 
  unique(str_replace(names(r.threshold.sub), "_[0-9]+$", ""))

# Apply a threshold to these layers to identify the grid cells that we will
# call "exposed" to RVF
months.per.year.threshold <- 6
annual.risk.areas <- collapse.months > months.per.year.threshold

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
      str_detect(projection_layer, "historical") ~ "historical",
      str_detect(projection_layer, "SSP126") ~ "SSP126",
      str_detect(projection_layer, "SSP245") ~ "SSP245",
      str_detect(projection_layer, "SSP370") ~ "SSP370",
    ),
    year = str_extract(projection_layer, "_[0-9]+$") %>%
      str_replace("_", "") %>%
      as.numeric()
  )

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

#==============================================================================


# Plotting and summary tables

d %>%
  ggplot(aes(x = year, y = pop_size_at_risk, color = scenario)) +
  geom_point() +
  xlim(1980, 2080) +
  theme_minimal()

d %>%
  ggplot(aes(x = year, y = prop_at_risk, color = scenario)) +
  geom_point() +
  xlim(1980, 2080) +
  theme_minimal()

d %>%
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
