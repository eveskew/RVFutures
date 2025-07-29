library(tidyverse)
library(tidymodels)
library(finetune)
library(future)

source("R/functions.R")

#==============================================================================


# Import dataset with outbreak and pseudo-absence points

d <- read_csv("data/outbreak_data/outbreak_data_w_popweighted_pseudoabsences_predictors.csv") %>%
  select(
    longitude, latitude, RVF_presence,
    year, month, month_numeric, 
    elevation, slope,
    human_pop, travel_time_to_healthcare,
    dist_to_lake_all, dist_to_lake_5, dist_to_lake_10, dist_to_river_10,
    cattle_density, goat_density, sheep_density,
    monthly_precip, monthly_precip_lag_1, monthly_precip_lag_2, monthly_precip_lag_3,
    cum_precip_3_months_prior,
    `bdod_0-5cm_mean`, `cec_0-5cm_mean`, `cfvo_0-5cm_mean`, `clay_0-5cm_mean`, 
    `nitrogen_0-5cm_mean`, `phh2o_0-5cm_mean`, `silt_0-5cm_mean`,
    monthly_tmax, monthly_tmax_lag_1, monthly_tmax_lag_2, monthly_tmax_lag_3,
    monthly_tmin, monthly_tmin_lag_1, monthly_tmin_lag_2, monthly_tmin_lag_3
  ) %>%
  mutate(
    RVF_presence_f = as.factor(RVF_presence),
    training_group = case_when(
      year %in% 2008:2011 ~ "group_1",
      year %in% 2012:2015 ~ "group_2",
      year %in% 2016:2018 ~ "group_3"
    ),
    testing_data = ifelse(year <= 2018, 0, 1)
  )

# Summarize the number of positives (outbreaks) and negatives (background
# points) in the training and test sets
d %>% 
  group_by(testing_data) %>%
  summarize(
    negatives = sum(RVF_presence == 0),
    positives = sum(RVF_presence == 1),
    negative_to_positive_ratio = negatives/positives
  )

#==============================================================================


# Generate data splits

# Set up a data split that reserves all data from post-2018 as test data
d.split <- group_initial_split(d, group = testing_data)
saveRDS(d.split, "data/saved_objects/d.split.rds")
d.split <- readRDS("data/saved_objects/d.split.rds")

# Get training data out of the split object
d.train <- training(d.split)
saveRDS(d.train, "data/saved_objects/d.train.rds")
d.train <- readRDS("data/saved_objects/d.train.rds")
table(d.train$year, d.train$RVF_presence)

# Get testing data out of the split object
d.test <- testing(d.split)
saveRDS(d.test, "data/saved_objects/d.test.rds")
d.test <- readRDS("data/saved_objects/d.test.rds")
table(d.test$year, d.test$RVF_presence)

# Divide the training data into folds
d.folds <- group_vfold_cv(d.train, group = "year")
saveRDS(d.folds, "data/saved_objects/d.folds.rds")
d.folds <- readRDS("data/saved_objects/d.folds.rds")

#==============================================================================


# XGBoost machine learning workflow

# Setup the model recipe
d.rec <- recipe(RVF_presence_f ~ ., data = d.train) %>%
  step_rm(
    longitude, latitude, RVF_presence,
    year, month, month_numeric, training_group, testing_data
  )

d.rec

# Setup the model specification, including which parameters will be tuned
xgb.spec <- boost_tree(
  learn_rate = 0.01,
  min_n = tune(),
  mtry = tune(),
  sample_size = tune(),
  stop_iter = tune(),
  tree_depth = tune(),
  trees = tune()
) %>%
  set_engine("xgboost", scale_pos_weight = tune(), validation = 0.2) %>%
  set_mode("classification")

xgb.spec

# Establish the modeling workflow
xgb.RVF.workflow <- workflow(
  preprocessor = d.rec, 
  spec = xgb.spec
)

xgb.RVF.workflow

# Parameter tuning: generate parameter grid, then tune using the training data
xgb.grid <- grid_space_filling(
  min_n(range = c(2L, 40L)), # default
  mtry(range = c(2L, 15L)), 
  sample_prop(range = c(0.25, 1)),
  scale_pos_weight(range = c(0.5, 2)),
  stop_iter(range = c(3L, 20L)), # default
  tree_depth(range = c(1L, 3L)),
  trees(range = c(50L, 2000L)),
  size = 250
)

xgb.grid

plan(multisession, workers = 4)
set.seed(8)

xgb.RVF.tune <- tune_grid(
  xgb.RVF.workflow,
  resamples = d.folds,
  grid = xgb.grid,
  control = control_grid(verbose = TRUE)
)
saveRDS(xgb.RVF.tune, "data/saved_objects/xgb.RVF.tune.rds")
 
show_best(xgb.RVF.tune, metric = "roc_auc")
autoplot(xgb.RVF.tune, metric = "roc_auc")

# Finalize the machine learning workflow using the best parameter set
xgb.RVF.final <- xgb.RVF.workflow %>%
  finalize_workflow(select_best(xgb.RVF.tune, metric = "roc_auc")) %>%
  last_fit(d.split)

xgb.RVF.final
saveRDS(xgb.RVF.final, "data/saved_objects/xgb.RVF.final.rds")
