library(tidyverse)
library(terra)
library(tidyterra)
library(wesanderson)
library(cowplot)
library(assertthat)

source("R/functions.R")

#==============================================================================


# Import key data and objects for plotting

# Saved XGBoost model object
xgb.RVF.final <- readRDS("data/saved_objects/xgb.RVF.final.rds")

# TSS cutoff value
tss.cutoff <- readRDS("data/misc/tss.cutoff.rds")

# Map layers
east.africa <- load_country_map()
lakes.10 <- readRDS("data/rasters/hydrology/saved_objects/lakes_east_africa_10.rds")

# Import all prediction rasters
files <- list.files(
  path = "data/prediction_rasters",
  full.names = TRUE
)

# Mask the prediction rasters
r.mask <- mask(rast(files), east.africa)

# Threshold the masked rasters
r.threshold <- r.mask >= tss.cutoff

# Prediction raster summary data
prs <- read_csv("data/misc/prediction_raster_summary.csv") %>%
  mutate(
    month = factor(month, levels = month.name),
    month_numeric = as.numeric(month)
  )

assert_that(dim(r.mask)[3] == nrow(prs))


# Generate a list of min and max observed prediction values in order to
# set appropriate axis limits throughout
low <- floor(min(minmax(log10(r.mask))))
low
high <- ceiling(max(minmax(log10(r.mask))))
high

list.val <- list(
  "1" = high,
  "0" = low
)

#==============================================================================


# Plot a thresholded map of RVF risk predictions for select months and years

select.years <- c(2008, 2012, 2016, 2020)
select.months <- c("March", "May", "October", "December")

# Which layers do we need to pull?
indices <- prs %>%
  filter(year %in% select.years, month %in% select.months) %>%
  pull(index)
# What names should each of these layers get?
labels <- prs %>%
  dplyr::slice(indices) %>%
  mutate(label = paste0(month, " ", year, " (", round(prop_suitable, 2), ")")) %>%
  pull(label)

# Subset the thresholded predictions accordingly
r.sub <- subset(r.threshold, indices)
names(r.sub) <- labels

ggplot() +
  geom_spatraster(data = r.sub) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_manual(values = c(alpha("lemonchiffon2", 0.5), alpha("indianred3", 0.7)), na.value = NA) +
  facet_wrap(
    ~lyr, dir = "h", ncol = length(select.months)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )

ggsave("outputs/figures/retrodiction_map_through_time_threshold.jpg", 
       height = 14, width = 10)

#==============================================================================


# Plot RVF risk maps for historical observed time period (2008-2022)


# Plot select months and years through time

# Subset the masked predictions accordingly
r.sub <- subset(r.mask, indices)
names(r.sub) <- labels

ggplot() +
  geom_spatraster(data = log10(r.sub)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA,
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative likelihood of RVF"
  ) +
  facet_wrap(
    ~lyr, dir = "h", ncol = length(select.months)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 20),
    strip.text.x = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.justification = "center", 
    legend.key.width = unit(0.65, "inches")
  )

ggsave("outputs/figures/retrodiction_map_through_time_prob.jpg", 
       height = 14, width = 10)


# Plot mean risk over the historical observed time period (2008-2022)

# Generate a raster that represents the mean prediction across all months
# from 2008-2022
r.2008.2022 <- r.mask %>%
  select(matches(as.character(2008:2022)))
assert_that(dim(r.2008.2022)[3] == 15 * 12)
r.2008.2022.mean <- mean(r.2008.2022)

ggplot() +
  geom_spatraster(data = log10(r.2008.2022.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18)
  )

ggsave(
  "outputs/figures/mapped_prediction_2008-2022.jpg", 
  height = 8, width = 8
)


# Look at these predictions with the administrative regions displayed

east.africa.adm <- load_country_map_adm()

ggplot() +
  geom_spatraster(data = log10(r.2008.2022.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa.adm, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18)
  )


# Threshold these predictions to more easily look at areas of high and low risk
# across the study region

r.2008.2022.mean.thresh <- r.2008.2022.mean >= tss.cutoff * 6

ggplot() +
  geom_spatraster(data = r.2008.2022.mean.thresh) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa.adm, linewidth = 0.5, fill = NA) +
  scale_fill_manual(
    values = c(alpha("lemonchiffon2", 0.5), alpha("indianred3", 0.7)), 
    na.value = NA
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18),
    legend.position = "none"
  )


# Plot mean seasonal risk over the historical observed time period (2008-2022)

# Generate mean risk predictions for Jan-Mar, Apr-Jun, Jul-Sep, and Oct-Dec
# for the 2008-2022 time period
r.2008.2022.seasons <- r.2008.2022 %>%
  tapp(
    ., 
    index = rep(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), times = 14),
    fun = "mean"
  )
assert_that(dim(r.2008.2022.seasons)[3] == 4)
names(r.2008.2022.seasons) <- c("Jan\u2013Mar", "Apr\u2013Jun", "Jul\u2013Sep", "Oct\u2013Dec")

p.2008.2022.seasons <- ggplot() +
  geom_spatraster(data = log10(r.2008.2022.seasons)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA,
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative likelihood of RVF"
  ) +
  facet_wrap(
    ~lyr, dir = "h", ncol = 4
  ) +
  theme_void() +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.justification = "center", 
    legend.key.width = unit(0.65, "inches")
  )

#==============================================================================


# Plot mean RVF risk predictions for historical and future climates


# Historical climate

# Subset the masked predictions to the predictions made for historical 
# (1970-2000) climate
r.hist.clim <- r.mask %>%
  select(matches("historical_climate")) %>%
  select(!matches("sensitivity"))
assert_that(dim(r.hist.clim)[3] == 12)
r.hist.clim.mean <- mean(r.hist.clim)

ggplot() +
  geom_spatraster(data = log10(r.hist.clim.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18)
  )

ggsave(
  "outputs/figures/mapped_prediction_1970-2000.jpg", 
  height = 8, width = 8
)

# Also produce a version of this map at the monthly scale to facilitate 
# comparison with future climate scenario change maps 
# (made with the "plot_deltas.R" script)
names(r.hist.clim) <- month.name

p <- ggplot() +
  geom_spatraster(data = log10(r.hist.clim)) +
  geom_sf(data = east.africa, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  ggtitle("Historical climate (1970-2000)") +
  theme_void() +
  facet_wrap(~lyr)

ggsave(
  filename = "outputs/figures/mapped_prediction_1970-2000_monthly.jpg", 
  plot = p,
  height = 10,
  width = 10
)


# Future climates

# Plots for each global climate model separately

gcms <- c(
  "ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg", 
  "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
)

for(gcm in gcms) {
  
  r.future.clim <- r.mask %>%
    select(matches(gcm)) %>%
    select(!matches("sensitivity"))
  assert_that(dim(r.future.clim)[3] == 12 * 9)
  index <- rep(1:(nlyr(r.future.clim)/12), each = 12)
  
  r.future.clim.mean <- tapp(r.future.clim, index, fun = "mean")
  names(r.future.clim.mean) <- 
    str_remove(string = names(r.future.clim), pattern = "_[0-9]{2}$") %>% 
    str_remove(string = ., pattern = paste0(gcm, "_")) %>%
    unique()
  
  ggplot() +
    geom_spatraster(data = log10(r.future.clim.mean)) +
    geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
    geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
    scale_fill_gradient2(
      low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
      midpoint = log10(tss.cutoff), na.value = NA, 
      breaks = unlist(list.val), limits = c(low, high),
      name = "Relative\nlikelihood\nof RVF"
    ) +
    facet_wrap(~lyr) +
    ggtitle(gcm) +
    theme_void() +
    theme(
      text = element_text(size = 16),
      legend.title = element_blank()
    )
  
  ggsave(
    filename = paste0("outputs/figures/gcm_mapped_prediction_", gcm, ".jpg"),
    height = 8, width = 8
  )
}

# Plots for each SSP scenario-year combination, averaging over global climate
# models

ssps <- c("ssp126", "ssp245", "ssp370")
ssps.upper <- toupper(ssps)

future.years <- c("2030", "2050", "2070")

for(ssp in ssps.upper) {
  
  for(year in future.years) {
    
    r.future.clim <- r.mask %>%
      select(matches(ssp)) %>%
      select(matches(year)) %>%
      select(!matches("sensitivity"))
    assert_that(dim(r.future.clim)[3] == (12 * 11))
    
    r.future.clim.mean <- mean(r.future.clim)
    
    ggplot() +
      geom_spatraster(data = log10(r.future.clim.mean)) +
      geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
      geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
      scale_fill_gradient2(
        low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
        midpoint = log10(tss.cutoff), na.value = NA, 
        breaks = unlist(list.val), limits = c(low, high),
        name = "Relative\nlikelihood\nof RVF"
      ) +
      theme_void() +
      theme(
        text = element_text(size = 18)
      )
    
    ggsave(
      filename = paste0("outputs/figures/mapped_prediction_", year, "_", ssp, ".jpg"),
      height = 8, width = 8
    )
  }
}

#==============================================================================


# Create multi-panel figure showing RVF risk predictions for 
# historical climate (1970-2000), historical weather (2008-2022), 
# and future climate (only SSP370, 2061-2080)

theme <- theme(
  text = element_text(size = 20),
  plot.title = element_text(hjust = 0.5, size = 22)
)

# Panel showing mean RVF risk predictions under historical climate (1970-2000)
a <- ggplot() +
  geom_spatraster(data = log10(r.hist.clim.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  ggtitle("Historical climate\n(1970-2000)") +
  theme_void() +
  theme

# Panel showing mean RVF risk predictions under historical weather (2008-2022)
b <- ggplot() +
  geom_spatraster(data = log10(r.2008.2022.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  ggtitle("Historical weather\n(2008-2022)") +
  theme_void() +
  theme

# Calculate difference raster between historical weather and historical climate
diff.contemp.hist <- r.2008.2022.mean - r.hist.clim.mean
minmax(diff.contemp.hist)

# Panel showing the difference raster between historical weather and historical
# climate
c <- ggplot() +
  geom_spatraster(data = diff.contemp.hist) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "floralwhite", high = "darkred",
    na.value = "white",
    name = "Change\nin RVF\nlikelihood",
    limits = c(-0.5, 0.5)
  ) +
  ggtitle("Historical weather (2008-2022) vs.\nhistorical climate (1970-2000)") +
  theme_void() +
  theme

# Pull predictions for future climate conditions under SSP370 and the 2061-2080
# time period
r.future.clim <- r.mask %>%
  select(matches("370")) %>%
  select(matches("2070")) %>%
  select(!matches("sensitivity"))
assert_that(dim(r.future.clim)[3] == (12 * 11))

r.future.clim.mean <- mean(r.future.clim)

# Panel showing mean RVF risk predictions under future climate (2061-2080) and
# the SSP370 scenario
d <- ggplot() +
  geom_spatraster(data = log10(r.future.clim.mean)) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkslategray", mid = "lemonchiffon", high = "indianred4",  
    midpoint = log10(tss.cutoff), na.value = NA, 
    breaks = unlist(list.val), limits = c(low, high),
    name = "Relative\nlikelihood\nof RVF"
  ) +
  ggtitle("Future climate\n(2061-2080, SSP370)") +
  theme_void() +
  theme

# Calculate difference raster between future climate and historical climate
diff.future.hist <- r.future.clim.mean - r.hist.clim.mean
minmax(diff.future.hist)

# Panel showing the difference raster between future climate and historical
# climate
e <- ggplot() +
  geom_spatraster(data = diff.future.hist) +
  geom_sf(data = lakes.10, col = NA, fill = "skyblue") +
  geom_sf(data = east.africa, linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "floralwhite", high = "darkred",
    na.value = "white",
    name = "Change\nin RVF\nlikelihood",
    limits = c(-0.5, 0.5)
  ) +
  ggtitle("Future climate (2061-2080, SSP370) vs.\nhistorical climate (1970-2000)") +
  theme_void() +
  theme
  
# Construct the full figure with all panels
top.panel <- plot_grid(
  a, labels = "a", label_size = 30
)
bottom.panel <- plot_grid(
  b, c, d, e,
  labels = c("b", "c", "d", "e"), label_size = 30
)
full.panel <- plot_grid(
  top.panel, bottom.panel, 
  nrow = 2, rel_heights = c(1, 2)
)

ggsave(
  "outputs/figures/mapped_prediction_multipanel.jpg", plot = full.panel, 
  width = 5000, height = 7000, unit = "px"
)

#==============================================================================


# Plot summary data for prediction rasters over all years 2008-2022

# Panel using thresholded predictions to quantify the proportion of the study
# region suitable for RVF
text.size <- 18
axis.title.y.size <- 18
axis.text.size <- 14
hjust <- -0.8

ymin.suit <- 0.5
ymax.suit <- 1

short.rains <- data.frame(
  xmin = 10,
  xmax = 13,
  ymin = ymin.suit,
  ymax = ymax.suit
)

long.rains <- data.frame(
  xmin = 3,
  xmax = 6,
  ymin = ymin.suit,
  ymax = ymax.suit
)

set.seed(8)

suit <- prs %>%
  filter(year %in% 2008:2022) %>%
  ggplot(aes(x = month_numeric + 0.5, y = prop_suitable, group = year, color = year)) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    data = short.rains, inherit.aes = FALSE,
    fill = alpha("cornflowerblue", 0.15)
  ) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    data = long.rains, inherit.aes = FALSE,
    fill = alpha("cornflowerblue", 0.15)
  ) +
  geom_jitter(height = 0, width = 0.03, size = 3) +
  geom_line(linewidth = 0.2, linetype = 2) +
  scale_color_gradient2(
    low = alpha("darkorange3", 0.8), 
    mid = alpha("ivory2", 0.8),
    high = alpha("dodgerblue3", 0.8),
    midpoint = 2014.5
  ) +
  xlab("") +
  ylab("Prop. study region suitable for RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(ymin.suit, ymax.suit) +
  guides(
    color = guide_colorbar(position = "inside", direction = "horizontal")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.y = element_text(size = axis.title.y.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = hjust),
    legend.position = "none"
  )

# Panel using predictions on the probability scale to quantify the mean risk
# across the study region
ymin.prob <- 0
ymax.prob <- 0.3

short.rains <- data.frame(
  xmin = 10,
  xmax = 13,
  ymin = ymin.prob,
  ymax = ymax.prob
)

long.rains <- data.frame(
  xmin = 3,
  xmax = 6,
  ymin = ymin.prob,
  ymax = ymax.prob
)

set.seed(8)

mean <- prs %>%
  filter(year %in% 2008:2022) %>%
  ggplot(aes(x = month_numeric + 0.5, y = mean_prob_mask, group = year, color = year)) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    data = short.rains, inherit.aes = FALSE,
    fill = alpha("cornflowerblue", 0.15)
  ) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    data = long.rains, inherit.aes = FALSE,
    fill = alpha("cornflowerblue", 0.15)
  ) +
  geom_jitter(height = 0, width = 0.03, size = 3) +
  geom_line(linewidth = 0.2, linetype = 2) +
  scale_color_gradient2(
    low = alpha("darkorange3", 0.8), 
    mid = alpha("ivory2", 0.8),
    high = alpha("dodgerblue3", 0.8),
    midpoint = 2014.5
  ) +
  xlab("") +
  ylab("Mean relative likelihood of RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(ymin.prob, ymax.prob) +
  guides(
    color = guide_colorbar(position = "inside", direction = "horizontal")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.y = element_text(size = axis.title.y.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = hjust),
    legend.position.inside = c(0.58, 0.87),
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_blank()
  )

# Construct the full figure panel
cowplot::plot_grid(
  mean, suit, ncol = 1,
  labels = "auto"
)

ggsave("outputs/figures/retrodiction_summary_2008-2022.jpg", 
       height = 12, width = 12)


# Add seasonal maps to this figure

cowplot::plot_grid(
  mean, suit, p.2008.2022.seasons, ncol = 1,
  labels = "auto", label_size = 20, rel_heights = c(1, 1, 1)
)

ggsave("outputs/figures/retrodiction_summary_2008-2022_w_seasons.jpg", 
       height = 16, width = 12)


# Same plots, but only for select years

palette <- wes_palette("Moonrise2")

suit <- prs %>%
  filter(year %in% select.years) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = month_numeric + 0.5, y = prop_suitable, group = year, color = year)) +
  geom_point(size = 4) +
  geom_line(linewidth = 0.5, linetype = 2) +
  xlab("") +
  ylab("Prop. study region suitable for RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(ymin.suit, ymax.suit) +
  scale_color_manual(values = palette) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = hjust),
    legend.position = "none"
  )

mean <- prs %>%
  filter(year %in% select.years) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = month_numeric + 0.5, y = mean_prob_mask, group = year, color = year)) +
  geom_point(size = 4) +
  geom_line(linewidth = 0.5, linetype = 2) +
  xlab("") +
  ylab("Mean relative likelihood of RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(ymin.prob, ymax.prob) +
  scale_color_manual(values = palette) +
  guides(
    color = guide_legend(position = "inside")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = hjust),
    legend.position.inside = c(0.8, 0.8),
    legend.text = element_text(size = 28),
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_blank()
  )

cowplot::plot_grid(
  mean, suit, ncol = 1,
  labels = "auto"
)

ggsave("outputs/figures/retrodiction_summary_select_years.jpg", 
       height = 12, width = 12)


# Alternative plot

prs %>%
  filter(year %in% 2008:2022) %>%
  ggplot(aes(x = year, y = mean_prob_mask, color = month)) +
  geom_line() +
  xlab("") +
  ylab("Mean relative likelihood of RVF outbreak") +
  ylim(0, 0.25) +
  theme_minimal()

#==============================================================================


# Plot summary data for prediction rasters over all future climate scenarios

text.size <- 30
axis.title.y.size <- 26
axis.text.size <- 14

palette <- wes_palette("Royal1")
palette <- c(alpha("black"), palette[c(1,4,2)])

# Subset prediction summary table to only future climate scenarios
ll.mod <- prs %>%
  filter(year %in% c(2030, 2050, 2070)) %>%
  filter(sensitivity == 0) %>%
  mutate(
    year = case_when(
      year == 2030 ~ "2021\u20132040",
      year == 2050 ~ "2041\u20132060",
      year == 2070 ~ "2061\u20132080"
    ),
    year = as.factor(year)
  )

# Subset prediction summary table to only historical climate scenarios
ll.historical <- prs %>%
  filter(year == 1985) %>%
  select(-year, -scenario)

# Summarize the future predictions in a 9-panel figure using the suitability
# metric
p.prop.suit <- ll.mod %>%
  group_by(year, scenario, month_numeric) %>%
  summarize(
    mean_prop_suitable = mean(prop_suitable),
    max_prop_suitable = max(prop_suitable),
    min_prop_suitable = min(prop_suitable)
  ) %>%
  ggplot(aes(x = month_numeric + 0.5, y = mean_prop_suitable, group = scenario, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = min_prop_suitable, ymax = max_prop_suitable), linewidth = 0) +
  #geom_point(data = ll.mod, aes(y = prop_suitable), size = 0.4) +
  geom_point(data = ll.mod %>% filter(gcm == "BCC-CSM2-MR"), aes(y = prop_suitable), size = 2.5) +
  geom_line(linewidth = 1) +
  geom_line(
    data = ll.historical,
    aes(x = month_numeric + 0.5, y = prop_suitable, group = time_period, color = time_period, fill = time_period),
    linewidth = 1, linetype = 2
  ) +
  xlab("") +
  ylab("Prop. study region suitable for RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(0.5, 1) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = alpha(palette, 0.5)) +
  facet_grid(scenario~year) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.y = element_text(size = axis.title.y.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = 0),
    axis.text.y = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )

ggsave(
  plot = p.prop.suit,
  "outputs/figures/prediction_summary_prop_suitable.jpg", 
  height = 12, width = 16
)

# Summarize the future predictions in a 9-panel figure using the mean probability
# metric
p.prob <- ll.mod %>%
  group_by(year, scenario, month_numeric) %>%
  summarize(
    mean_mean_prob = mean(mean_prob_mask),
    max_mean_prob = max(mean_prob_mask),
    min_mean_prob = min(mean_prob_mask)
  ) %>%
  ggplot(aes(x = month_numeric + 0.5, y = mean_mean_prob, group = scenario, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = min_mean_prob, ymax = max_mean_prob), linewidth = 0) +
  #geom_point(data = ll.mod, aes(y = mean_prob_mask), size = 0.4) +
  geom_point(data = ll.mod %>% filter(gcm == "BCC-CSM2-MR"), aes(y = mean_prob_mask), size = 2.5) +
  geom_line(linewidth = 1) +
  geom_line(
    data = ll.historical,
    aes(x = month_numeric + 0.5, y = mean_prob_mask, group = time_period, color = time_period, fill = time_period),
    linewidth = 1, linetype = 2
  ) +
  xlab("") +
  ylab("Mean relative likelihood of RVF") +
   scale_x_continuous(
     breaks = 1:13, 
     minor_breaks = NULL,
     labels = c(month.abb, ""),
     limits = c(1, 13)
   ) +
  ylim(0, 0.25) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = alpha(palette, 0.5)) +
  facet_grid(scenario ~ year) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.y = element_text(size = axis.title.y.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = 0),
    axis.text.y = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )

ggsave(
  plot = p.prob,
  "outputs/figures/prediction_summary_prob.jpg", 
  height = 12, width = 16
)

# Generate a simplified version of the mean probability figure with only three
# panels (combine SSP scenarios into the same panel)
palette2 <- rep("gainsboro", 4)

ll.all <- bind_rows(ll.historical, ll.historical, ll.historical) %>%
  mutate(
    scenario = rep("Historical", times = nrow(.)),
    year = rep(c("2021\u20132040", "2041\u20132060", "2061\u20132080"), each = 12)
  ) %>%
  bind_rows(ll.mod)

p.prob.simple <- ll.mod %>%
  group_by(year, scenario, month_numeric) %>%
  summarize(
    mean_mean_prob = mean(mean_prob_mask),
    max_mean_prob = max(mean_prob_mask),
    min_mean_prob = min(mean_prob_mask)
  ) %>%
  ggplot(aes(x = month_numeric + 0.5, y = mean_mean_prob, group = scenario, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = min_mean_prob, ymax = max_mean_prob), linewidth = 0) +
  #geom_point(data = ll.mod, aes(y = mean_prob_mask), size = 0.4) +
  #geom_point(data = ll.mod %>% filter(gcm == "BCC-CSM2-MR"), aes(y = mean_prob_mask), size = 2.5) +
  geom_line(linewidth = 1.5) +
  geom_line(
    data = ll.historical,
    aes(x = month_numeric + 0.5, y = mean_prob_mask, group = time_period, color = time_period, fill = time_period),
    linewidth = 1, linetype = 2, color = "black"
  ) +
  xlab("") +
  ylab("Mean relative likelihood of RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(0, 0.25) +
  scale_color_manual(values = palette[2:4], limits = c("SSP126", "SSP245", "SSP370")) +
  scale_fill_manual(values = alpha(palette2, 0.3), limits = c("SSP126", "SSP245", "SSP370")) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.y = element_text(size = axis.title.y.size),
    strip.text = element_text(size = text.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = 0),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank()
  )

p.prob.simple2 <- ll.all %>%
  group_by(year, scenario, month_numeric) %>%
  summarize(
    mean_mean_prob = mean(mean_prob_mask),
    max_mean_prob = max(mean_prob_mask),
    min_mean_prob = min(mean_prob_mask)
  ) %>%
  ggplot(
    aes(x = month_numeric + 0.5, y = mean_mean_prob, group = scenario, color = scenario, fill = scenario, linetype = scenario),
    linetype = rep(2, 1, 1, 1)
  ) +
  geom_ribbon(aes(ymin = min_mean_prob, ymax = max_mean_prob), linewidth = 0) +
  geom_line(linewidth = 1.5) +
  ylab("Mean relative likelihood of RVF") +
  scale_x_continuous(
    breaks = 1:13, 
    minor_breaks = NULL,
    labels = c(month.abb, ""),
    limits = c(1, 13)
  ) +
  ylim(0, 0.25) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = alpha(palette2, 0.3)) +
  scale_linetype_manual(values = c(2, 1, 1, 1)) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = axis.title.y.size),
    strip.text = element_text(size = text.size),
    axis.text = element_text(size = axis.text.size),
    axis.text.x = element_text(hjust = 0),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(0.7, "inches")
  )

#==============================================================================


# Generate a boxplot figure showing human population at risk from RVF

d.humanpop <- read_csv("data/misc/human_pop_at_risk.csv")

d.humanpop.mod <- d.humanpop %>%
  filter(year %in% c(2030, 2050, 2070)) %>%
  filter(sensitivity == 0) %>%
  mutate(
    year = case_when(
      year == 2030 ~ "2021\u20132040",
      year == 2050 ~ "2041\u20132060",
      year == 2070 ~ "2061\u20132080"
    ),
    year = as.factor(year)
  )
assert_that(nrow(d.humanpop.mod) == 99)
  
p.humanpop <- d.humanpop.mod %>%
  ggplot(aes(x = as.factor(year), y = pop_size_at_risk/1000000, fill = scenario)) +
  geom_boxplot(width = 0.8) +
  ylab("Human pop. at risk (millions)") +
  ylim(50, 200) +
  scale_fill_manual(values = palette[2:4]) +
  facet_wrap(~year, scales = "free_x") +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = axis.title.y.size),
    strip.text = element_text(size = text.size),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4),
    panel.grid.minor.y = element_line(linewidth = 0.2)
  )

ggsave(
  plot = p.humanpop,
  "outputs/figures/humanpop_at_risk.jpg",
  height = 10, width = 14
)

#==============================================================================


# Generate a multi-panel figure showing risk predictions under future climate
# as well as future human population at risk

p.humanpop.mod <- p.humanpop + 
  theme(
    legend.position = "none"
  )

cowplot::plot_grid(
  p.prob.simple2, p.humanpop.mod,
  nrow = 2, rel_heights = c(1.2, 1),
  labels = "auto", label_size = 28
)

ggsave(
  "outputs/figures/prediction_summary_prob_humanpop.jpg",
  height = 16, width = 16
)

#==============================================================================


# Figure to illustrate human population at risk sensitivity analyses

d.humanpop.mod <- d.humanpop %>%
  filter(year %in% c(2030, 2050, 2070)) %>%
  mutate(
    year = case_when(
      year == 2030 ~ "2021-2040",
      year == 2050 ~ "2041-2060",
      year == 2070 ~ "2061-2080"
    ),
    year = as.factor(year),
    group = case_when(
      sensitivity == 0 ~ "climate change +\nhuman population change",
      sensitivity == 1 & str_detect(projection_layer, "sensitivity_historical_climate") ~ "fixed climate +\nhuman population change",
      sensitivity == 1 & str_detect(projection_layer, "sensitivity_historical_humanpop") ~ "climate change +\nfixed human population"
    ),
    group = factor(group, levels = c(
      "climate change +\nhuman population change",
      "climate change +\nfixed human population",
      "fixed climate +\nhuman population change"
    )
    )
  )
assert_that(nrow(d.humanpop.mod) == 99 + 9 + 99)

historical.value <- d.humanpop %>%
  filter(year == 1985) %>%
  pull(pop_size_at_risk)

d.humanpop.mod %>%
  ggplot(aes(x = group, y = pop_size_at_risk/1000000, fill = scenario)) +
  geom_boxplot(width = 0.8) +
  geom_hline(yintercept = historical.value/1000000, lty = 2, linewidth = 1) +
  ylab("Human pop. at risk (millions)") +
  ylim(25, 225) +
  scale_fill_manual(values = palette[2:4]) +
  facet_wrap(~year, scales = "free_x") +
  theme_minimal() +
  theme(
    text = element_text(size = text.size),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = axis.title.y.size),
    strip.text = element_text(size = text.size),
    axis.text.x = element_text(angle = 50, size = 12, vjust = 1.1, hjust = 0.9),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4),
    panel.grid.minor.y = element_line(linewidth = 0.2)
  ) +
  facet_wrap(~year)

ggsave(
  "outputs/figures/humanpop_at_risk_sensitivity_analysis.jpg",
  height = 10, width = 16
)

#==============================================================================


# Alternative visualization of change in average prediction for 
# future climate scenarios

# Generate a raster stack that combines global climate model results across 
# months, years, and SSP scenarios
future.predictions <- r.mask %>%
  select(matches("SSP")) %>%
  select(!matches("sensitivity"))
assert_that(dim(future.predictions)[3] == 1188)

combine.gcms <- tapp(
  future.predictions, 
  fun = "mean",
  index = rep(1:108, times = 11)
)
assert_that(dim(combine.gcms)[3] == 12 * 3 * 3)

names(combine.gcms) <- 
  str_replace(
    r.mask %>%
      select(matches("ACCESS")) %>%
      select(!matches("sensitivity")) %>%
      names(), 
    "ACCESS-CM2_", 
    ""
  )

# Calculate difference between this raster stack and historical climate
diffs <- combine.gcms - r.hist.clim

# Calculate the mean for each raster difference layer
diffs.mean <- global(diffs, "mean", na.rm = TRUE)

# Package information into a data frame
diff.values <- data.frame(
  layer = rownames(diffs.mean),
  mean_change = diffs.mean$mean
) %>%
  mutate(
    scenario = layer %>%
      str_extract("SSP[0-9]+") %>%
      as.factor(),
    year = layer %>%
      str_extract("_[0-9]+_") %>%
      str_extract("[0-9]+") %>%
      as.factor(),
    month = layer %>%
      str_extract("_[0-9]+$") %>%
      str_extract("[0-9]+")
  )

# Plot 
palette <- wes_palette("Royal1")
palette <- palette[c(1,4,2)]

diff.values %>%
  ggplot(aes(x = month, y = mean_change, group = scenario, color = scenario)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = palette) +
  theme_minimal() +
  facet_wrap(~year)
