# Create a lookup table to enable grabbing of correct raster layers

month.table <- list(
  "January" = "01",
  "February" = "02",
  "March" = "03",
  "April" = "04",
  "May" = "05",
  "June" = "06",
  "July" = "07",
  "August" = "08",
  "September" = "09",
  "October" = "10",
  "November" = "11",
  "December" = "12"
)



# Function to load a country map of Kenya, Uganda, and Tanzania

load_country_map <- function() {
  
  map <- rnaturalearth::ne_countries(
    country = c("Kenya", "United Republic of Tanzania", "Uganda"),
    scale = "medium",
    returnclass = "sf"
  )
  
  map <- sf::st_transform(map, 4326)
  
  return(map)
}



# Function to load a country map of Kenya, Uganda, and Tanzania with admin
# level 1 polygons

load_country_map_adm <- function() {
  
  map <- rnaturalearth::ne_states(
    country = c("Kenya", "United Republic of Tanzania", "Uganda"),
    returnclass = "sf"
  )
  
  map <- sf::st_transform(map, 4326)
  
  return(map)
}



# Function to crop and generate a resampled density raster starting from a 
# count data raster

resample_count_raster <- function(count_raster, crop_extent, raster_for_resampling, threshold = 0.01) {
  
  # Crop the count raster to the desired extent
  crop <- terra::crop(count_raster, crop_extent)
  
  # Convert to a density raster and confirm the total counts remain the same
  density <- crop / terra::cellSize(crop, unit = "km")
  tot <- terra::global(crop, "sum", na.rm = TRUE)
  tot.density <- global(
    density * terra::cellSize(density, unit = "km"),
    "sum", na.rm = TRUE
  )
  assertthat::assert_that((1 - threshold) < (tot/tot.density) & (1 + threshold) > (tot/tot.density))
  
  # Resample the density raster to the desired resolution
  density.resample <- terra::resample(density, raster_for_resampling, "bilinear")
  tot.density.resampled <- terra::global(
    density.resample * terra::cellSize(density.resample, unit = "km"),
    "sum", na.rm = TRUE
  )
  assertthat::assert_that((1 - threshold) < (tot/tot.density.resampled) & (1 + threshold) > (tot/tot.density.resampled))
  
  return(density.resample)
}



# Function to generate a projected raster layer based on a regression raster

generate_raster_projection <- function(
    regression_raster, first_year_of_data, projection_year, 
    lower_clamp = NULL, upper_clamp = NULL, layer_name) {
  
  # calculate the projected raster layer using the regression raster
  p <- regression_raster$`(Intercept)` + regression_raster$x*length(first_year_of_data:projection_year)
  
  # Clamp low values, if desired
  if(!is.null(lower_clamp)) {p <- ifel(p < lower_clamp, lower_clamp, p)}
  
  # Clamp high values, if desired
  if(!is.null(upper_clamp)) {p <- ifel(p > upper_clamp, upper_clamp, p)}
  
  # Rename the raster layer
  names(p) <- layer_name
  
  # Return the raster layer, checking for extreme values
  max.r <- max(minmax(r))
  max.p <- max(minmax(p))
  ifelse(
    max.p < 2 * max.r,
    return(p),
    print("Warning! Projected raster layer contains extremely large values!")
  )
}



# Function to extract static raster data

extract_static_raster <- function(dataframe, raster) {
  
  values <- c()
  
  for(i in 1:nrow(dataframe)) {
    
    if((i %% 100) == 0) {print(i)}
    
    # Get relevant data from the observation
    row <- dataframe[i, ]
    
    # Extract values
    values <- c(values, terra::extract(raster, sf::st_coordinates(row)))
  }
  
  values <- unname(unlist(values))
  
  return(values)
}



# Function to extract human population density raster data

extract_humanpop_raster <- function(dataframe, raster) {
  
  values <- c()
  
  for(i in 1:nrow(dataframe)) {
    
    if((i %% 100) == 0) {print(i)}
    
    # Get relevant data from the observation
    row <- dataframe[i, ]
    year <- row$OB_Yr
    # NOTE: Using 2020 data for any years past 2020
    year <- ifelse(year > 2020, 2020, year)
    
    # Subset to the correct raster layer
    layer <- raster[[paste0("pd_", year, "_0.05deg")]]
    
    # Extract values
    values <- c(values, terra::extract(layer, sf::st_coordinates(row)))
  }
  
  values <- unname(unlist(values))
  
  return(values)
}



# Function to extract data from a hydrology shapefile

extract_hydrology_distance <- function(dataframe, hydrology) {
  
  print("Calculating nearest hydrology")
  nearest <- sf::st_nearest_feature(dataframe, hydrology)
  print("Calculating distance to nearest hydrology")
  dist_to_hydro <- sf::st_distance(dataframe, hydrology[nearest, ], by_element = TRUE)
  dist_to_hydro <- as.numeric(dist_to_hydro)
  return(dist_to_hydro)
}



# Function to extract NDVI raster data

extract_NDVI_raster <- function(dataframe, raster) {
  
  # Initiate and fill NDVI variables
  dataframe$monthly_NDVI <- rep(NA)
  dataframe$monthly_NDVI_lag_1 <- rep(NA)
  dataframe$monthly_NDVI_lag_2 <- rep(NA)
  dataframe$monthly_NDVI_lag_3 <- rep(NA)
  
  for(i in 1:nrow(dataframe)) {
    
    if((i %% 100) == 0) {print(i)}
    
    # Get relevant data from the observation
    row <- dataframe[i, ]
    year <- row$OB_Yr
    month <- month.table[row$OB_Mo]
    
    # Subset to the correct NDVI raster layer
    layer <- raster[[paste0("Monthly_NDVI_", year, "_", month)]]
    
    # Get lagged layers
    index <- which(names(raster) == names(layer))
    layer.lag.1 <- raster[[index - 1]]
    layer.lag.2 <- raster[[index - 2]]
    layer.lag.3 <- raster[[index - 3]]
    
    # Extract values
    dataframe[i, "monthly_NDVI"] <- terra::extract(layer, sf::st_coordinates(row))
    dataframe[i, "monthly_NDVI_lag_1"] <- terra::extract(layer.lag.1, sf::st_coordinates(row))
    dataframe[i, "monthly_NDVI_lag_2"] <- terra::extract(layer.lag.2, sf::st_coordinates(row))
    dataframe[i, "monthly_NDVI_lag_3"] <- terra::extract(layer.lag.3, sf::st_coordinates(row))
  }
  
  return(dataframe)
}



# Function to extract precipitation raster data

extract_precipitation_raster <- function(dataframe, raster) {
  
  # Initiate and fill precipitation variables
  dataframe$monthly_precip <- rep(NA)
  dataframe$monthly_precip_lag_1 <- rep(NA)
  dataframe$monthly_precip_lag_2 <- rep(NA)
  dataframe$monthly_precip_lag_3 <- rep(NA)
  
  for(i in 1:nrow(dataframe)) {
    
    if((i %% 100) == 0) {print(i)}
    
    # Get relevant data from the observation
    row <- dataframe[i, ]
    year <- row$OB_Yr
    month <- month.table[row$OB_Mo]
    
    # Subset to the correct precipitation raster layer
    layer <- raster[[paste0("chirps-v2.0.", year, ".", month)]]
    
    # Get lagged layers
    index <- which(names(raster) == names(layer))
    layer.lag.1 <- raster[[index - 1]]
    layer.lag.2 <- raster[[index - 2]]
    layer.lag.3 <- raster[[index - 3]]
    
    # Extract values
    dataframe[i, "monthly_precip"] <- terra::extract(layer, sf::st_coordinates(row))
    dataframe[i, "monthly_precip_lag_1"] <- terra::extract(layer.lag.1, sf::st_coordinates(row))
    dataframe[i, "monthly_precip_lag_2"] <- terra::extract(layer.lag.2, sf::st_coordinates(row))
    dataframe[i, "monthly_precip_lag_3"] <- terra::extract(layer.lag.3, sf::st_coordinates(row))
  }
  
  return(dataframe)
}



# Function to extract temperature raster data

extract_temperature_raster <- function(dataframe, raster) {
  
  # Initiate and fill temperature variables
  dataframe$monthly_temp <- rep(NA)
  dataframe$monthly_temp_lag_1 <- rep(NA)
  dataframe$monthly_temp_lag_2 <- rep(NA)
  dataframe$monthly_temp_lag_3 <- rep(NA)
  
  for(i in 1:nrow(dataframe)) {
    
    if((i %% 100) == 0) {print(i)}
    
    # Get relevant data from the observation
    row <- dataframe[i, ]
    year <- row$OB_Yr
    month <- month.table[row$OB_Mo]
    
    # Subset to the correct temperature raster layer
    layer <- raster[[paste0("LST_Day_CMG_", year, "_", month)]]
    
    # Get lagged layers
    index <- which(names(raster) == names(layer))
    layer.lag.1 <- raster[[index - 1]]
    layer.lag.2 <- raster[[index - 2]]
    layer.lag.3 <- raster[[index - 3]]
    
    # Extract values
    dataframe[i, "monthly_temp"] <- terra::extract(layer, sf::st_coordinates(row))
    dataframe[i, "monthly_temp_lag_1"] <- terra::extract(layer.lag.1, sf::st_coordinates(row))
    dataframe[i, "monthly_temp_lag_2"] <- terra::extract(layer.lag.2, sf::st_coordinates(row))
    dataframe[i, "monthly_temp_lag_3"] <- terra::extract(layer.lag.3, sf::st_coordinates(row))
  }
  
  return(dataframe)
}



# Function to calculate weighted accuracy and TSS from a random forests model

rf_tss <- function(model, weight = 0.5) {
  
  # Extract confusion matrix
  confusion <- model$confusion
  
  # Extract values
  true.negatives <- confusion[1,1]
  false.positives <- confusion[1,2]
  false.negatives <- confusion[2,1]
  true.positives <- confusion[2,2]
  
  true.negative.rate <- true.negatives / (true.negatives + false.positives)
  true.positive.rate <- true.positives / (true.positives + false.negatives)
  weighted.accuracy <- (weight * true.positive.rate) + ((1 - weight) * true.negative.rate)
  tss <- true.positive.rate + true.negative.rate - 1
  
  print("True negative rate:") 
  print(true.negative.rate, digits = 4)
  print("True positive rate:")
  print(true.positive.rate, digits = 4)
  print("Weighted accuracy:")
  print(weighted.accuracy, digits = 4)
  print("True skill statistic:")
  print(tss, digits = 4)
}



# Function to generate an ROCR prediction object from a "last_fit()" object

get_prediction_object <- function(model) {
  
  ROCR::prediction(
    model %>%
      tune::collect_predictions() %>%
      dplyr::pull(`.pred_1`), 
    model %>%
      tune::collect_predictions() %>%
      dplyr::pull(RVF_presence_f) %>%
      as.integer()
  )
}



# Function to generate summary "report" files for predictor flat files

generate_predictor_report <- function(dataframe, type, filename) {
  
  # Pivot the predictor data frame into long format
  d.long <- dataframe %>%
    select(-grid_cell) %>%
    tidyr::pivot_longer(
      !matches("year|month$"),
      names_to = "variable",
      values_to = "value"
    )
  
  # Summarize the predictor variables depending upon the type of predictor
  # data
  if(type == "static") {
    
    d.sum <- d.long %>%
      group_by(variable) %>%
      summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      ungroup()
  }
  
  if(type == "yearly") {
    
    d.sum <- d.long %>%
      group_by(variable, year) %>%
      summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      ungroup()
  }
  
  if(type == "monthly") {
    
    d.sum <- d.long %>%
      group_by(variable, year, month) %>%
      summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      ungroup()
  }
  
  if(type == "monthly_climate") {
    
    d.sum <- d.long %>%
      group_by(variable, month) %>%
      summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      ungroup()
  }
  
  # Round all numeric variables to make nicer output
  d.sum <- d.sum %>%
    mutate_if(is.numeric, round, digits = 3)
  
  # Save the report file
  write_csv(d.sum, file = filename)
}
