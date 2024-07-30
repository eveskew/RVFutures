library(tidyverse)
library(tidymodels)
library(finetune)

source("R/functions.R")

#==============================================================================


# Import dataset with outbreak and pseudo-absence points

d <- read_csv("data/outbreak_data/outbreak_popweighted_predictors.csv") %>%
  select(
    longitude, latitude,
    RVF_presence, Hu_Cs,
    year, month, month_numeric, 
    elevation, slope,
    human_pop, travel_time_to_healthcare,
    dist_to_lake_all, dist_to_lake_1, dist_to_lake_5, dist_to_lake_10,
    dist_to_river_10,
    c3ann, c3nfx, c3per, c4ann, pastr, primf, primn, secdf,
    cattle_density, goat_density, sheep_density,
    monthly_precip, monthly_precip_lag_1, monthly_precip_lag_2, monthly_precip_lag_3,
    cum_precip_3_months_prior,
    `bdod_0-5cm_mean`, `cec_0-5cm_mean`, `cfvo_0-5cm_mean`, `clay_0-5cm_mean`, 
    `nitrogen_0-5cm_mean`, `phh2o_0-5cm_mean`, `sand_0-5cm_mean`, `silt_0-5cm_mean`, 
    `soc_0-5cm_mean`,
    monthly_tmax, monthly_tmax_lag_1, monthly_tmax_lag_2, monthly_tmax_lag_3,
    monthly_tmin, monthly_tmin_lag_1, monthly_tmin_lag_2, monthly_tmin_lag_3
  ) %>%
  mutate(
    RVF_presence_f = as.factor(RVF_presence),
    year_group = case_when(
      year %in% 2008:2012 ~ "group_1",
      year %in% 2013:2017 ~ "group_2",
      year == 2018 ~ "group_3",
      year %in% 2019:2022 ~ "group_4"
    ),
    testing_data = ifelse(year <= 2018, 0, 1)
  )

#==============================================================================


# Generate data splits

# Set up a data split that reserves all data from post-2018 as test data
d.split <- group_initial_split(d, group = testing_data)
saveRDS(d.split, "saved_objects/d.split.rds")
d.split <- readRDS("saved_objects/d.split.rds")

# Get training data out of the split object
d.train <- training(d.split)
saveRDS(d.train, "saved_objects/d.train.rds")
d.train <- readRDS("saved_objects/d.train.rds")
table(d.train$year, d.train$RVF_presence)

# Get testing data out of the split object
d.test <- testing(d.split)
saveRDS(d.test, "saved_objects/d.test.rds")
d.test <- readRDS("saved_objects/d.test.rds")
table(d.test$year, d.test$RVF_presence)

# Divide the training data into folds
d.folds <- vfold_cv(d.train, v = 5, strata = "RVF_presence")
saveRDS(d.folds, "saved_objects/d.folds.rds")
d.folds <- readRDS("saved_objects/d.folds.rds")

#==============================================================================


# XGBoost machine learning workflow

# Setup the model recipe
d.rec <- recipe(RVF_presence_f ~ ., data = d.train) %>%
  step_rm(
    longitude, latitude, RVF_presence, Hu_Cs, 
    year, month, month_numeric, year_group, testing_data
  )

d.rec

# Setup the model specification, including which parameters will be tuned
xgb.spec <- boost_tree(
  learn_rate = tune(),
  min_n = tune(),
  mtry = tune(),
  sample_size = tune(),
  stop_iter = tune(),
  tree_depth = tune(),
  trees = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb.spec

# Establish the modeling workflow
xgb.RVF.workflow <- workflow(
  preprocessor = d.rec, 
  spec = xgb.spec
)

xgb.RVF.workflow

# Parameter tuning: generate parameter grid, then tune using the training data
xgb.grid <- grid_max_entropy(
  learn_rate(c(-4, -1)),
  min_n(c(2L, 20L)),
  mtry(c(2L, 20L)),
  sample_prop(c(0.1, 0.9)),
  stop_iter(c(3L, 20L)),
  tree_depth(c(1L, 3L)),
  trees(c(10L, 1000L)),
  size = 250
)

xgb.grid

doParallel::registerDoParallel()
set.seed(8)

xgb.RVF.tune <- tune_grid(
  xgb.RVF.workflow,
  resamples = d.folds,
  grid = xgb.grid
)
saveRDS(xgb.RVF.tune, "saved_objects/xgb.RVF.tune.rds")
 
show_best(xgb.RVF.tune, metric = "roc_auc")
autoplot(xgb.RVF.tune, metric = "roc_auc")

# Finalize the machine learning workflow using the best parameter set
xgb.RVF.final <- xgb.RVF.workflow %>%
  finalize_workflow(select_best(xgb.RVF.tune, metric = "roc_auc")) %>%
  last_fit(d.split)

xgb.RVF.final
saveRDS(xgb.RVF.final, "saved_objects/xgb.RVF.final.rds")
