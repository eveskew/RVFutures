library(tidyverse)
library(terra)
library(sf)

sf_use_s2(FALSE)

source("R/functions.R")

# Validate model predictions using human RVF serology data collated by EC

#==============================================================================


# Import human serology data

# Import background outlines of Kenya, Tanzania, and Uganda
east.africa <- load_country_map()

# Import shapefile for use with Cook et al. data
s <- st_read("data/serology_data/Cook_etal_2017/Ken_Sublocations/")
st_crs(s) <- st_crs(east.africa)$proj4string

# Import collated human RVF serology data
col.types <- c(
  "text", "text", "numeric", "text", "text", 
  "text", "numeric", "numeric", "numeric", "text", 
  "text", "numeric", "numeric", "numeric", "text", 
  "text", "text"
)
d <- readxl::read_xlsx(
  "data/serology_data/East_Africa_RVF_data_human_Evan.7.9.xlsx",
  col_types = col.types, na = "NA"
)
colnames(d) <- c(
  "first_author", "country", "sample_ID", "location_ID", "region",
  "species", "age", "RVF_positive", "total_sampled", "date", 
  "year", "latitude", "longitude", "SLID", "notes_1",
  "notes_2", "age_range"
)

nrow(d)
sum(is.na(d$latitude))
sum(is.na(d$longitude))
sum(is.na(d$date))
sum(is.na(d$year))

ggplot() +
  geom_sf(data = east.africa, fill = "white") +
  geom_point(data = d, aes(x = longitude, y = latitude)) +
  theme_minimal()

#==============================================================================


# Data cleaning

d2 <- d

# Modify date information in the data frame
d2 <- d %>%
  mutate(
    # some Excel dates imported incorrectly, so need to convert these
    date_mod = as.character(openxlsx::convertToDate(date)),
    # then there are dates ambiguously specified: fix these to the midpoint of
    # the time range given
    date_mod = case_when(
      date == "May-Feburary" & year == "2018-2019" ~ "2018-10",
      date == "March-August" & year == "2017" ~ "2017-06",
      date == "November-May" & year == "2018-2019" ~ "2019-02",
      TRUE ~ date_mod
    ),
    # modify dates for Situma records
    date_mod = ifelse(
      first_author == "Situma",
      paste0(
        year, "-",
        substr(date_mod, 4, 5), "-",
        substr(date_mod, 1, 2)
      ),
      date_mod
    ),
    # for all other dates, take what we've got
    date_mod = ifelse(is.na(date_mod), date, date_mod),
    # then for dates only presented in "year-month" format, assign these to the
    # 15th day just so everything can be converted to a proper date variable
    date_formatted = ifelse(str_detect(date_mod, "([0-9].*-[0-9].*)"), paste0(date_mod, "-15"), NA_character_),
    date_formatted = ifelse(str_detect(date_mod, "([0-9].*-[0-9].*-[0-9].*)"), date_mod, date_formatted),
    date_formatted = as.Date(date_formatted),
    # calculate year and month values from the formatted dates
    year_from_date = year(date_formatted),
    month_from_date = month(date_formatted)
  )

# Calculate the lat/long of centroid locations for each SLID
s2 <- s %>%
  st_centroid() %>%
  mutate(
    latitude = st_coordinates(.)[,2],
    longitude = st_coordinates(.)[,1]
  ) %>%
  st_drop_geometry() %>%
  select(-SLNAME)
assertthat::assert_that(nrow(s) == nrow(s2))

# Join s2 and d2
d2 <- left_join(
  d2, s2,
  by = "SLID"
) %>%
  mutate(
    latitude_mod = ifelse(is.na(latitude.x), latitude.y, latitude.x),
    longitude_mod = ifelse(is.na(longitude.x), longitude.y, longitude.x)
  ) %>%
  rename(
    latitude = latitude.x,
    longitude = longitude.x
  ) %>%
  select(-c(latitude.y, longitude.y)) %>%
  filter(
    !is.na(latitude_mod),
    !is.na(longitude_mod),
    !is.na(age)
  )

nrow(d2)    
sum(is.na(d2$latitude_mod))
sum(is.na(d2$longitude_mod))
sum(is.na(d2$age))

table(d2$first_author, useNA = "ifany")
table(d2$country, useNA = "ifany")
table(d2$species, useNA = "ifany")
summary(d2$age, useNA = "ifany")
table(d2$RVF_positive, useNA = "ifany")

#==============================================================================


# Add grid cell information to the data frame

r <- terra::rast("data/rasters/precipitation/processed/wc2.1_2.5m_prec_2000-01.tif")

# Add cell-level data

d2$cell <- NA

for(i in 1:nrow(d2)) {
  
  print(i)
  coordinates <- cbind(
    lon = d2$longitude_mod[i],
    lat = d2$latitude_mod[i]
  )
  print(coordinates)
  d2$cell[i] <- cellFromXY(r, coordinates)
}

sum(is.na(d2$cell))
length(unique(d2$cell))

# Write the data to disk

write_csv(d2, "data/serology_data/East_Africa_RVF_data_human_w_cells.csv")

#==============================================================================


# Calculate cell-specific force of infection (FOI)


# Because some cells contain serology data from multiple different studies,
# first need to subset data in these cells so that we're only considering data
# from one study/time period

# Count how many authors/studies contribute data to each cell

cell.author.count <- d2 %>%
  distinct(first_author, cell) %>%
  group_by(cell) %>%
  summarize(n_authors_in_cell = n()) %>%
  ungroup()

# Generate a table that will allow us to identify which author/cell combinations
# should be included in the final serology data for FOI calculation. Where
# multiple studies contribute data to one cell, only want to include the
# author/study with the most data

cell.info.table <- d2 %>%
  group_by(first_author, cell) %>%
  summarize(n_samples = n()) %>%
  ungroup() %>%
  left_join(., cell.author.count, by = "cell") %>%
  arrange(desc(n_authors_in_cell), cell, desc(n_samples)) %>%
  group_by(cell) %>%
  mutate(
    include_in_FOI_calculation = ifelse(row_number() == 1, TRUE, FALSE)
  ) %>%
  ungroup()

# Join this information into our cleaned data and filter accordingly

d3 <- d2 %>%
  left_join(
    ., 
    cell.info.table %>%
      select(first_author, cell, include_in_FOI_calculation),
    by = c("first_author", "cell")
    ) %>%
  filter(include_in_FOI_calculation == TRUE)

nrow(d2)
nrow(d3)


# Calculate FOI using this subset data

# Function to create empty data frame
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}

data <- d3

length(unique(data$cell))

# Remove rows when the number of samples is less than 20
count <- data.frame(sort(table(data$cell)))
count.sub <- subset(count, Freq > 19)
colnames(count.sub) <- c("cell", "Freq")
data.sub <- merge(x = data, y = count.sub, by = "cell", all = TRUE)
data.sub <- subset(data.sub, !is.na(Freq))

length(unique(data.sub$cell))
table(data.sub$country, useNA = "ifany")

# Summarize serology by cell and age
SiteByAge <- data.sub %>%
  group_by(cell) %>%
  summarize(
    mean_RVFserpos = mean(RVF_positive),
    mean_Age = mean(age)
  )

# Check for relationship between average age and IgG
SiteByAge %>%
  ggplot(aes(x = mean_Age, y = mean_RVFserpos)) + 
  geom_point()

# Estimate FOI (lambda) by cell
cells <- unique(data.sub$cell)
df <- create_empty_table(length(cells), 5)
colnames(df) <- c("lambda", "se", "cell", "n_individuals", "n_positive")

for(i in 1:length(cells)) {
  
  site <- subset(data.sub, cell == cells[i])
  age <- site$age
  result <- site$RVF_positive
  n_individuals <- nrow(site)
  n_positive <- sum(result)
  
  if(sum(result) == 0) {
    
    df[i,1] = 0
    df[i,2] = 0
    df[i,3] = cells[i]
    df[i,4] = n_individuals
    df[i,5] = n_positive
  } else {
    
    # Maximize the log likelihood for lambda
    nloglik <- function(lambda) {
      
      if (lambda < 0) {
        
        return(Inf)
      } else {
        
        loglik <- sum(log(exp(-lambda/1000*age*abs(result-1))*(1-exp(-lambda/1000*age))^result))
        return(-loglik)
      }
    }
    
    m <- optim(c(lambda = 0.1), nloglik, method = "SANN", hessian = TRUE)
    df[i,1] = m$par/1000
    df[i,2] = sqrt(diag(solve(m$hessian)))/1000
    df[i,3] = cells[i]
    df[i,4] = n_individuals
    df[i,5] = n_positive
  }
}

#==============================================================================


# Add RVF likelihood predictions to the data frame

years <- 2008:2021

files <- list.files(
  path = "data/prediction_rasters",
  full.names = TRUE,
  pattern = paste(years, collapse = "|")
)

r <- terra::rast(files)
assertthat::assert_that(dim(r)[3] == length(years)*12)
mean <- mean(r)

# Add predicted RVF suitability data

values <- values(mean)
df$RVF_relative_likelihood <- values[df$cell]

#==============================================================================


# Plot

# Relationship between seropositivity and FOI
df %>%
  ggplot(aes(x = lambda, y = n_positive/n_individuals)) +
  geom_point(aes(size = n_individuals)) +
  theme_minimal()

# Relationship between FOI and model-predicted likelihood
df %>%
  ggplot(aes(x = RVF_relative_likelihood, y = lambda)) +
  geom_point(
    aes(size = n_individuals)
  ) +
  geom_smooth(
    method = "lm",
    mapping = aes(weight = n_individuals),
    color = "black",
    linetype = 2,
    se = FALSE
  ) +
  xlab("Mean relative likelihood of RVF from 2008-2021") +
  ylab("Estimated RVFV FOI") +
  guides(size = guide_legend(title = "Number of tests")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "black")
  )

ggsave(
  "outputs/figures/FOI.jpg",
  width = 9, height = 6
)

#==============================================================================


# Fit a linear model to look at the relationship between FOI and model-predicted
# likelihood

m <- lm(
  lambda ~ RVF_relative_likelihood, 
  weight = n_individuals,
  data = df
)

summary(m)
