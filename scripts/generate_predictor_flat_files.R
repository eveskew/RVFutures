library(tidyverse)
library(terra)
library(sf)
library(assertthat)

source("R/functions.R")

# Script to generate predictor flat files. These will contain predictor values 
# for each grid cell on a 2.5 arcminute grid (dim = 413 rows, 301 columns). To 
# save space and time, different predictor types will be processed and stored
# in the smallest size possible

#==============================================================================


# Generate flat file for static predictors (so only need grid cells)

n.cells <- 413 * 301

d <- data.frame(
  grid_cell = 1:n.cells
)


# Add hydrology data to the data frame

r <- rast("data/rasters/hydrology/processed/dist_to_lake_all_2.5min.tif")
d$dist_to_lake_all <- as.numeric(values(r))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_1_2.5min.tif")
d$dist_to_lake_1 <- as.numeric(values(r))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_5_2.5min.tif")
d$dist_to_lake_5 <- as.numeric(values(r))

r <- rast("data/rasters/hydrology/processed/dist_to_lake_10_2.5min.tif")
d$dist_to_lake_10 <- as.numeric(values(r))

r <- rast("data/rasters/hydrology/processed/dist_to_river_10_2.5min.tif")
d$dist_to_river_10 <- as.numeric(values(r))


# Add soil data to the data frame

r <- rast("data/rasters/soil/processed/soil_2.5min.tif")

d$`bdod_0-5cm_mean` <- as.numeric(values(r$`bdod_0-5cm_mean`))

d$`cec_0-5cm_mean` <- as.numeric(values(r$`cec_0-5cm_mean`))

d$`cfvo_0-5cm_mean` <- as.numeric(values(r$`cfvo_0-5cm_mean`))

d$`clay_0-5cm_mean` <- as.numeric(values(r$`clay_0-5cm_mean`))

d$`nitrogen_0-5cm_mean` <- as.numeric(values(r$`nitrogen_0-5cm_mean`))

d$`phh2o_0-5cm_mean` <- as.numeric(values(r$`phh2o_0-5cm_mean`))

d$`sand_0-5cm_mean` <- as.numeric(values(r$`sand_0-5cm_mean`))

d$`silt_0-5cm_mean` <- as.numeric(values(r$`silt_0-5cm_mean`))

d$`soc_0-5cm_mean` <- as.numeric(values(r$`soc_0-5cm_mean`))


# Add elevation and slope data to the data frame

r <- rast("data/rasters/elevation/processed/elevation_2.5min.tif")
d$elevation <- as.numeric(values(r))

r <- rast("data/rasters/elevation/processed/slope_2.5min.tif")
d$slope <- as.numeric(values(r))


# Add travel time to healthcare data to the data frame

r <- rast("data/rasters/healthcare/processed/healthcare_2.5min.tif")
d$travel_time_to_healthcare <- as.numeric(values(r))


# Add animal density data to the data frame

# Load in cattle density file
r <- rast("data/rasters/livestock/processed/Ct_2015_Aw_density_2.5min.tif")
d$cattle_density <- as.numeric(values(r))

# Load in goat density file
r <- rast("data/rasters/livestock/processed/Gt_2015_Aw_density_2.5min.tif")
d$goat_density <- as.numeric(values(r))

# Load in sheep density file
r <- rast("data/rasters/livestock/processed/Sh_2015_Aw_density_2.5min.tif")
d$sheep_density <- as.numeric(values(r))


# Write static predictor data to disk

write_csv(d, "data/predictor_flat_files/static_predictors.csv")

filename <- "data/predictor_reports/static_predictors.csv"

generate_predictor_report(
  dataframe = d,
  type = "static",
  filename = filename
)

max(read_csv(filename)$prop_missing)

#==============================================================================


# Generate flat file for yearly predictors (so need grid cells and years)

n.cells <- 413 * 301
years <- 2000:2022
n.years <- length(years)

d <- data.frame(
  year = rep(years, each = n.cells),
  grid_cell = rep(1:n.cells, times = n.years)
)


# Add human population data to the data frame

# Load in human population raster files
files <- list.files(
  path = "data/rasters/human_population/processed",
  full.names = TRUE
)

files

r <- rast(files)

d$human_pop <- NA

for(year in years) {
  
  # Subset to the correct raster layer
  # Use 2020 data for years after 2020
  if(year <= 2020) {layer <- r[[paste0("pd_", year)]]}
  if(year > 2020) {layer <- r$pd_2020}
  
  # Extract values
  d[d$year == year, "human_pop"] <- as.numeric(values(layer))
}


# Add land cover data to the data frame

# Load in land cover raster files
files <- list.files(
  path = "data/rasters/land_cover/processed",
  full.names = TRUE
)

files

r <- rast(files)

lc.variables <- c(
  "c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr", "primf", 
  "primn", "range", "secdf", "secdn", "secma", "secmb", "urban"
)

df <- data.frame(matrix(nrow = nrow(d), ncol = length(lc.variables)))
colnames(df) <- lc.variables
d <- cbind(d, df)

for(year in years) {
  
  # Subset to the correct raster layer for the focal year
  # Use SSP245 data for years after 2015
  if(year <= 2015) {r.sub <- tidyterra::select(r, matches(paste0("^", year)))}
  if(year > 2015) {r.sub <- tidyterra::select(r, matches(paste0("SSP245_", year)))}
  
  assert_that(length(names(r.sub)) == length(lc.variables))

  for(var in lc.variables) {
    
    print(c(year, var))
    
    # Subset to the correct raster layer for the focal variable
    layer <- tidyterra::select(r.sub, matches(var))
    
    # Extract values
    d[d$year == year, var] <- as.numeric(values(layer))
  }
}


# Write yearly predictor data to disk

write_csv(d, "data/predictor_flat_files/yearly_predictors_historical.csv")

filename <- "data/predictor_reports/yearly_predictors_historical.csv"

generate_predictor_report(
  dataframe = d,
  type = "yearly",
  filename = filename
)

max(read_csv(filename)$prop_missing)

#==============================================================================


# Generate flat files for future yearly predictors

n.cells <- 413 * 301
years <- c(2030, 2050, 2070)
n.years <- length(years)

scenarios <- c("SSP126", "SSP245", "SSP370")

for(s in scenarios) {
  
  d <- data.frame(
    year = rep(years, each = n.cells),
    grid_cell = rep(1:n.cells, times = n.years)
  )
  
  
  # Add human population data to the data frame
  
  # Load in human population raster files
  files <- list.files(
    path = "data/rasters/human_population/processed",
    pattern = paste0(substring(s, 1, 4), "_2.5min"),
    full.names = TRUE
  )
  
  files
  
  r <- rast(files)
  
  d$human_pop <- NA
  
  for(year in years) {
    
    # Subset to the correct raster layer
    layer <- r[[paste0(substring(s, 1, 4), "_", year)]]
    
    # Extract values
    d[d$year == year, "human_pop"] <- as.numeric(values(layer))
  }
  
  
  # Add land cover data to the data frame
  
  # Load in land cover raster files
  files <- list.files(
    path = "data/rasters/land_cover/processed",
    pattern = s,
    full.names = TRUE
  )
  
  files
  
  r <- rast(files)
  
  lc.variables <- c(
    "c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr", "primf", 
    "primn", "range", "secdf", "secdn", "secma", "secmb", "urban"
  )
  
  df <- data.frame(matrix(nrow = nrow(d), ncol = length(lc.variables)))
  colnames(df) <- lc.variables
  d <- cbind(d, df)
  
  for(year in years) {
    
    # Subset to the correct raster layer for the focal year
    r.sub <- tidyterra::select(r, matches(paste0(year)))
    
    assert_that(length(names(r.sub)) == length(lc.variables))
    
    for(var in lc.variables) {
      
      print(c(year, var))
      
      # Subset to the correct raster layer for the focal variable
      layer <- tidyterra::select(r.sub, matches(var))
      
      # Extract values
      d[d$year == year, var] <- as.numeric(values(layer))
    }
  }
  
  
  # Write yearly predictor data to disk
  
  write_csv(d, paste0("data/predictor_flat_files/yearly_predictors_", s, ".csv"))
  
  filename <- paste0("data/predictor_reports/yearly_predictors_", s, ".csv")
  
  generate_predictor_report(
    dataframe = d,
    type = "yearly",
    filename = filename
  )
  
  print(max(read_csv(filename)$prop_missing))
}

#==============================================================================


# Generate flat file for historical monthly weather predictors 
# (so need grid cells, years, and months)

n.cells <- 413 * 301
years <- 2007:2022
n.years <- length(years)
n.months <- 12

d <- data.frame(
  year = rep(years, each = n.cells * n.months),
  month = rep(rep(month.name, each = n.cells), times = n.years),
  grid_cell = rep(1:n.cells, times = n.years * n.months)
)


# Add NDVI data to the data frame

# Load in NDVI raster files
# files <- list.files(
#   path = "data/rasters/NDVI/processed",
#   pattern = paste(years, collapse = "|"),
#   full.names = TRUE
# )
# 
# files
# 
# r <- rast(files)
# 
# d$monthly_NDVI <- as.numeric(values(r))
# 
# for(year in unique(d$year)) {
#   
#   for(month in month.name) {
#     
#     print(c(year, month))
#   
#     # Subset to the correct raster layer
#     if(year <= 2023) {layer <- r[[paste0("Monthly_NDVI_", year, "_", month.table[month])]]}
#     if(year > 2023) {layer <- r[[paste0("projected_", year, "_", month.table[month])]]}
#     
#     # Extract values
#     d[d$year == year & d$month == month, "monthly_NDVI"] <- as.numeric(values(layer))
#   }
# }


# Add precipitation data to the data frame

# Load in precipitation raster files
files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = paste(years, collapse = "|"),
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_precip <- NA

for(year in 2007:2021) {
  
  for(month in month.name) {
    
    print(c(year, month))
    
    # Subset to the correct raster layer
    layer <- r[[paste0("wc2.1_2.5m_prec_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_precip"] <- as.numeric(values(layer))
  }
}


# Add temperature data to the data frame

# Load in temperature raster files
files <- list.files(
  path = "data/rasters/temperature/processed",
  pattern = paste(years, collapse = "|"),
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_tmax <- NA
d$monthly_tmin <- NA

for(year in 2007:2021) {
  
  for(month in month.name) {
    
    print(c(year, month))
    
    # Subset to the correct raster layer for tmax
    layer <- r[[paste0("wc2.1_2.5m_tmax_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_tmax"] <- as.numeric(values(layer))
    
    # Subset to the correct raster layer for tmin
    layer <- r[[paste0("wc2.1_2.5m_tmin_", year, "-", month.table[month])]]
    
    # Extract values
    d[d$year == year & d$month == month, "monthly_tmin"] <- as.numeric(values(layer))
  }
}


# Calculate lagged values for monthly variables

d <- d %>%
  # convert month to a factor variable for sorting
  mutate(month = factor(month, levels = month.name)) %>%
  # arrange by grid cell, year, and month to get a time series of each
  # cell's values
  arrange(grid_cell, year, month) %>%
  group_by(grid_cell) %>%
  # perform lagged variable calculations on the per-cell level
  mutate(
    monthly_tmax_lag_1 = lag(monthly_tmax, n = 1),
    monthly_tmax_lag_2 = lag(monthly_tmax, n = 2),
    monthly_tmax_lag_3 = lag(monthly_tmax, n = 3),
    
    monthly_tmin_lag_1 = lag(monthly_tmin, n = 1),
    monthly_tmin_lag_2 = lag(monthly_tmin, n = 2),
    monthly_tmin_lag_3 = lag(monthly_tmin, n = 3),
    
    monthly_precip_lag_1 = lag(monthly_precip, n = 1),
    monthly_precip_lag_2 = lag(monthly_precip, n = 2),
    monthly_precip_lag_3 = lag(monthly_precip, n = 3),
    
    cum_precip_3_months_prior = 
      monthly_precip_lag_1 + monthly_precip_lag_2 + monthly_precip_lag_3
  ) %>%
  ungroup() %>%
  # remove the year 2007 as this was only included to generate lagged variables
  # for 2008
  filter(year != 2007)


# Write monthly predictor data to disk

write_csv(d, "data/predictor_flat_files/monthly_predictors_historical_weather.csv")

filename <- "data/predictor_reports/monthly_predictors_historical_weather.csv"

generate_predictor_report(
  dataframe = d,
  type = "monthly",
  filename = filename
)

max(read_csv(filename)$prop_missing)

#==============================================================================


# Generate flat file for historical monthly climate predictors 
# (so need grid cells and months)

n.cells <- 413 * 301
n.months <- 12

d <- data.frame(
  month = rep(month.name, each = n.cells),
  grid_cell = rep(1:n.cells, times = n.months)
)


# Add precipitation data to the data frame

# Load in precipitation raster files
files <- list.files(
  path = "data/rasters/precipitation/processed",
  pattern = "1970-2000",
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_precip <- NA

for(month in month.name) {
  
  # Subset to the correct raster layer
  layer <- r[[paste0("wc2.1_2.5m_prec_1970-2000-", month.table[month])]]
  
  # Extract values
  d[d$month == month, "monthly_precip"] <- as.numeric(values(layer))
}


# Add temperature data to the data frame

# Load in temperature raster files
files <- list.files(
  path = "data/rasters/temperature/processed",
  pattern = "1970-2000",
  full.names = TRUE
)

files

r <- rast(files)

d$monthly_tmax <- NA
d$monthly_tmin <- NA
  
for(month in month.name) {
  
  # Subset to the correct raster layer for tmax
  layer <- r[[paste0("wc2.1_2.5m_tmax_1970-2000-", month.table[month])]]
  
  # Extract values
  d[d$month == month, "monthly_tmax"] <- as.numeric(values(layer))
  
  # Subset to the correct raster layer for tmin
  layer <- r[[paste0("wc2.1_2.5m_tmin_1970-2000-", month.table[month])]]
  
  # Extract values
  d[d$month == month, "monthly_tmin"] <- as.numeric(values(layer))
}


# Calculate lagged values for monthly variables

# Duplicate the data so there is something to calculate lags from
d <- d %>%
  rbind(d) %>%
  mutate(year = rep(c(1, 2), each = n()/2))

d <- d %>%
  # convert month to a factor variable for sorting
  mutate(month = factor(month, levels = month.name)) %>%
  # arrange by grid cell, year, and month to get a time series of each
  # cell's values
  arrange(grid_cell, year, month) %>%
  group_by(grid_cell) %>%
  # perform lagged variable calculations on the per-cell level
  mutate(
    monthly_tmax_lag_1 = lag(monthly_tmax, n = 1),
    monthly_tmax_lag_2 = lag(monthly_tmax, n = 2),
    monthly_tmax_lag_3 = lag(monthly_tmax, n = 3),
    
    monthly_tmin_lag_1 = lag(monthly_tmin, n = 1),
    monthly_tmin_lag_2 = lag(monthly_tmin, n = 2),
    monthly_tmin_lag_3 = lag(monthly_tmin, n = 3),
    
    monthly_precip_lag_1 = lag(monthly_precip, n = 1),
    monthly_precip_lag_2 = lag(monthly_precip, n = 2),
    monthly_precip_lag_3 = lag(monthly_precip, n = 3),
    
    cum_precip_3_months_prior = 
      monthly_precip_lag_1 + monthly_precip_lag_2 + monthly_precip_lag_3
  ) %>%
  ungroup() %>%
  filter(year == 2) %>%
  select(-year)


# Write monthly predictor data to disk

write_csv(d, "data/predictor_flat_files/monthly_predictors_historical_climate.csv")

filename <- "data/predictor_reports/monthly_predictors_historical_climate.csv"

generate_predictor_report(
  dataframe = d,
  type = "monthly_climate",
  filename = filename
)

max(read_csv(filename)$prop_missing)

#==============================================================================


# Generate flat files for future monthly predictors

n.cells <- 413 * 301
years <- c(2029, 2030, 2049, 2050, 2069, 2070)
n.years <- length(years)
n.months <- 12

gcms <- c(
  "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", 
  "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
)
scenarios <- c("SSP126", "SSP245", "SSP370")

for(g in gcms) {
  
  print(g)
  
  for(s in scenarios) {
    
    print(s)
    
    d <- data.frame(
      year = rep(years, each = n.cells * n.months),
      month = rep(rep(month.name, each = n.cells), times = n.years),
      grid_cell = rep(1:n.cells, times = n.years * n.months)
    )
    
    
    # Add precipitation data to the data frame
    
    # Load in precipitation raster files
    files <- list.files(
      path = "data/rasters/precipitation/processed",
      pattern = paste0(g, "_", tolower(s)),
      full.names = TRUE
    )
    
    files
    assert_that(length(files) == 36)
    
    r <- rast(files)
    
    d$monthly_precip <- NA
    
    for(year in c(2030, 2050, 2070)) {
      
      for(month in month.name) {
        
        print(c(year, month))
        
        # Subset to the correct raster layer
        layer <- r[[paste0("wc2.1_2.5m_prec_", g, "_", tolower(s), "_", year, "-", month.table[month])]]
        
        # Extract values
        d[d$year == (year - 1) & d$month == month, "monthly_precip"] <- as.numeric(values(layer))
        d[d$year == year & d$month == month, "monthly_precip"] <- as.numeric(values(layer))
      }
    }
    
    
    # Add temperature data to the data frame
    
    # Load in temperature raster files
    files <- list.files(
      path = "data/rasters/temperature/processed",
      pattern = paste0(g, "_", tolower(s)),
      full.names = TRUE
    )
    
    files
    assert_that(length(files) == 72)
    
    r <- rast(files)
    
    d$monthly_tmax <- NA
    d$monthly_tmin <- NA
    
    for(year in c(2030, 2050, 2070)) {
      
      for(month in month.name) {
        
        print(c(year, month))
        
        # Subset to the correct raster layer for tmax
        layer <- r[[paste0("wc2.1_2.5m_tmax_", g, "_", tolower(s), "_", year, "-", month.table[month])]]
        
        # Extract values
        d[d$year == (year - 1) & d$month == month, "monthly_tmax"] <- as.numeric(values(layer))
        d[d$year == year & d$month == month, "monthly_tmax"] <- as.numeric(values(layer))
        
        # Subset to the correct raster layer for tmin
        layer <- r[[paste0("wc2.1_2.5m_tmin_", g, "_", tolower(s), "_", year, "-", month.table[month])]]
        
        # Extract values
        d[d$year == (year - 1) & d$month == month, "monthly_tmin"] <- as.numeric(values(layer))
        d[d$year == year & d$month == month, "monthly_tmin"] <- as.numeric(values(layer))
      }
    }
    
    
    # Calculate lagged values for monthly variables
    
    d <- d %>%
      # convert month to a factor variable for sorting
      mutate(month = factor(month, levels = month.name)) %>%
      # arrange by grid cell, year, and month to get a time series of each
      # cell's values
      arrange(grid_cell, year, month) %>%
      group_by(grid_cell) %>%
      # perform lagged variable calculations on the per-cell level
      mutate(
        monthly_tmax_lag_1 = lag(monthly_tmax, n = 1),
        monthly_tmax_lag_2 = lag(monthly_tmax, n = 2),
        monthly_tmax_lag_3 = lag(monthly_tmax, n = 3),
        
        monthly_tmin_lag_1 = lag(monthly_tmin, n = 1),
        monthly_tmin_lag_2 = lag(monthly_tmin, n = 2),
        monthly_tmin_lag_3 = lag(monthly_tmin, n = 3),
        
        monthly_precip_lag_1 = lag(monthly_precip, n = 1),
        monthly_precip_lag_2 = lag(monthly_precip, n = 2),
        monthly_precip_lag_3 = lag(monthly_precip, n = 3),
        
        cum_precip_3_months_prior = 
          monthly_precip_lag_1 + monthly_precip_lag_2 + monthly_precip_lag_3
      ) %>%
      ungroup() %>%
      # filter to relevant years 
      filter(year %in% c(2030, 2050, 2070))
    
    
    # Write monthly predictor data to disk
    
    write_csv(d, paste0("data/predictor_flat_files/monthly_predictors_", g, "_", s, ".csv"))
    
    filename <- paste0("data/predictor_reports/monthly_predictors_", g, "_", s, ".csv")
    
    generate_predictor_report(
      dataframe = d,
      type = "monthly",
      filename = filename
    )
    
    print(max(read_csv(filename)$prop_missing))
  }
}
