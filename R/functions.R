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

generate_raster_projection <- function(regression_raster, first_year_of_data, projection_year, lower_clamp = NULL, upper_clamp = NULL, layer_name) {
  
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
    year <- row$outbreak_year
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
  dist.to.hydro <- sf::st_distance(dataframe, hydrology[nearest, ], by_element = TRUE)
  dist.to.hydro <- as.numeric(dist.to.hydro)
  return(dist.to.hydro)
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
    year <- row$outbreak_year
    month <- month.table[row$outbreak_month]
    
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
    year <- row$outbreak_year
    month <- month.table[row$outbreak_month]
    
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
    year <- row$outbreak_year
    month <- month.table[row$outbreak_month]
    
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
    dplyr::select(-grid_cell) %>%
    tidyr::pivot_longer(
      !matches("year|month$"),
      names_to = "variable",
      values_to = "value"
    )
  
  # Summarize the predictor variables depending upon the type of predictor
  # data
  if(type == "static") {
    
    d.sum <- d.long %>%
      dplyr::group_by(variable) %>%
      dplyr::summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      dplyr::ungroup()
  }
  
  if(type == "yearly") {
    
    d.sum <- d.long %>%
      dplyr::group_by(variable, year) %>%
      dplyr::summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      dplyr::ungroup()
  }
  
  if(type == "monthly") {
    
    d.sum <- d.long %>%
      dplyr::group_by(variable, year, month) %>%
      dplyr::summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      dplyr::ungroup()
  }
  
  if(type == "monthly_climate") {
    
    d.sum <- d.long %>%
      dplyr::group_by(variable, month) %>%
      dplyr::summarize(
        min = min(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        n_missing = sum(is.na(value)),
        prop_missing = n_missing/n()
      ) %>%
      dplyr::ungroup()
  }
  
  # Round all numeric variables to make nicer output
  d.sum <- d.sum %>%
    dplyr::mutate_if(is.numeric, round, digits = 3)
  
  # Save the report file
  readr::write_csv(d.sum, file = filename)
}



# Function to pull centroid coordinates based on known admin level locations

pull_centroids_from_adm <- function(dataframe) {
  
  # Pull admin level 1 maps for all study countries
  kenya.adm1 <- rgeoboundaries::geoboundaries(country = "Kenya", "adm1") %>%
    sf::st_transform(., 4326)
  uganda.adm1 <- rgeoboundaries::geoboundaries(country = "Uganda", "adm1") %>%
    sf::st_transform(., 4326)
  tanzania.adm1 <- rgeoboundaries::geoboundaries(country = "Tanzania", "adm1") %>%
    sf::st_transform(., 4326)
  
  # Pull admin level 2 maps for all study countries
  kenya.adm2 <- rgeoboundaries::geoboundaries(country = "Kenya", "adm2") %>%
    sf::st_transform(., 4326)
  uganda.adm2 <- rgeoboundaries::geoboundaries(country = "Uganda", "adm3") %>%
    sf::st_transform(., 4326)
  tanzania.adm2 <- rgeoboundaries::geoboundaries(country = "Tanzania", "adm2") %>%
    sf::st_transform(., 4326)
  
  for(i in 1:nrow(dataframe)) {
    
    print(i)
    
    # If GPS information is missing
    if (is.na(dataframe$GPS_x[i])) {
      
      # If admin level 2 information is available
      if (!is.na(dataframe$ADM2[i])) {
        
        # Generate a character vector with all known admin level 2 locations
        query <- dataframe$ADM2[i] %>%
          stringr::str_replace(., " District| Subcounty", "") %>%
          stringr::str_split(pattern = ", ") %>%
          purrr::simplify()
        
        # Load the appropriate country map
        if (dataframe$country[i] == "Kenya") {map <- kenya.adm2}
        if (dataframe$country[i] == "Uganda") {map <- uganda.adm2}
        if (dataframe$country[i] == "Tanzania") {map <- tanzania.adm2}
        
        # Subset the map to the appropriate admin level 2 areas
        map.subset <- map %>%
          dplyr::filter(shapeName %in% query)
        
        # Make sure all the relevant admin level 2 areas have been pulled
        assertthat::assert_that(length(query) == nrow(map.subset))
        
        # Pull the map subset centroid
        point <- sf::st_centroid(sf::st_union(map.subset))
        
        # Assign the centroid coordinates to the missing GPS cells
        dataframe$GPS_x[i] <- sf::st_coordinates(point)[,1]
        dataframe$GPS_y[i] <- sf::st_coordinates(point)[,2]
      }
      
      # If only admin level 1 information is available
      if (!is.na(dataframe$ADM1[i]) & is.na(dataframe$ADM2[i])) {
        
        # Generate a character vector with all known admin level 1 locations
        query <- dataframe$ADM1[i] %>%
          stringr::str_replace(., " County| Region", "") %>%
          stringr::str_split(pattern = ", ") %>%
          purrr::simplify()
        
        # Load the appropriate country map
        if (dataframe$country[i] == "Kenya") {map <- kenya.adm1}
        if (dataframe$country[i] == "Uganda") {map <- uganda.adm1}
        if (dataframe$country[i] == "Tanzania") {map <- tanzania.adm1}
        
        # Subset the map to the appropriate admin level 1 areas
        map.subset <- map %>%
          dplyr::filter(shapeName %in% query)
        
        # Make sure all the relevant admin level 1 areas have been pulled
        assertthat::assert_that(length(query) == nrow(map.subset))
        
        # Pull the map subset centroid
        point <- sf::st_centroid(sf::st_union(map.subset))
        
        # Assign the centroid coordinates to the missing GPS cells
        dataframe$GPS_x[i] <- sf::st_coordinates(point)[,1]
        dataframe$GPS_y[i] <- sf::st_coordinates(point)[,2]
      }
    }
  }
  
  return(dataframe)
}



# Function to pull random coordinates based on known admin level locations

pull_random_points_from_adm <- function(dataframe) {
  
  # Pull in lakes layers so that maps can be differenced, preventing random 
  # coordinates from occurring over water
  lakes <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_5.rds") %>%
    sf::st_union()
  
  # Pull admin level 1 maps for all study countries, differencing the lakes
  kenya.adm1 <- rgeoboundaries::geoboundaries(country = "Kenya", "adm1") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  uganda.adm1 <- rgeoboundaries::geoboundaries(country = "Uganda", "adm1") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  tanzania.adm1 <- rgeoboundaries::geoboundaries(country = "Tanzania", "adm1") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  
  # Pull admin level 2 maps for all study countries, differencing the lakes
  kenya.adm2 <- rgeoboundaries::geoboundaries(country = "Kenya", "adm2") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  uganda.adm2 <- rgeoboundaries::geoboundaries(country = "Uganda", "adm3") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  tanzania.adm2 <- rgeoboundaries::geoboundaries(country = "Tanzania", "adm2") %>%
    sf::st_transform(., 4326) %>%
    sf::st_difference(., lakes)
  
  for(i in 1:nrow(dataframe)) {
    
    print(i)
    
    # If GPS information is missing
    if (is.na(dataframe$GPS_x[i])) {
      
      # If admin level 2 information is available
      if (!is.na(dataframe$ADM2[i])) {
        
        # Generate a character vector with all known admin level 2 locations
        query <- dataframe$ADM2[i] %>%
          stringr::str_replace(., " District| Subcounty", "") %>%
          stringr::str_split(pattern = ", ") %>%
          purrr::simplify()
        
        # Load the appropriate country map
        if (dataframe$country[i] == "Kenya") {map <- kenya.adm2}
        if (dataframe$country[i] == "Uganda") {map <- uganda.adm2}
        if (dataframe$country[i] == "Tanzania") {map <- tanzania.adm2}
        
        # Subset the map to the appropriate admin level 2 areas
        map.subset <- map %>%
          dplyr::filter(shapeName %in% query)
        
        # Make sure all the relevant admin level 2 areas have been pulled
        assertthat::assert_that(length(query) == nrow(map.subset))
        
        # Pull a random set of coordinates from the map subset
        point <- sf::st_sample(sf::st_union(map.subset), size = 1)
        
        # Assign the random coordinates to the missing GPS cells
        dataframe$GPS_x[i] <- sf::st_coordinates(point)[,1]
        dataframe$GPS_y[i] <- sf::st_coordinates(point)[,2]
      }
      
      # If only admin level 1 information is available
      if (!is.na(dataframe$ADM1[i]) & is.na(dataframe$ADM2[i])) {
        
        # Generate a character vector with all known admin level 1 locations
        query <- dataframe$ADM1[i] %>%
          stringr::str_replace(., " County| Region", "") %>%
          stringr::str_split(pattern = ", ") %>%
          purrr::simplify()
        
        # Load the appropriate country map
        if (dataframe$country[i] == "Kenya") {map <- kenya.adm1}
        if (dataframe$country[i] == "Uganda") {map <- uganda.adm1}
        if (dataframe$country[i] == "Tanzania") {map <- tanzania.adm1}
        
        # Subset the map to the appropriate admin level 1 areas
        map.subset <- map %>%
          dplyr::filter(shapeName %in% query)
        
        # Make sure all the relevant admin level 1 areas have been pulled
        assertthat::assert_that(length(query) == nrow(map.subset))
        
        # Pull a random set of coordinates from the map subset
        point <- sf::st_sample(sf::st_union(map.subset), size = 1)
        
        # Assign the random coordinates to the missing GPS cells
        dataframe$GPS_x[i] <- sf::st_coordinates(point)[,1]
        dataframe$GPS_y[i] <- sf::st_coordinates(point)[,2]
      }
    }
  }
  
  return(dataframe)
}
