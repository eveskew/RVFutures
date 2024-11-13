library(tidyverse)
library(tidymodels)
library(vip)
library(DALEXtra)
library(terra)

source("R/functions.R")

#==============================================================================


# Import saved XGBoost model objects

d.train <- readRDS("saved_objects/d.train.rds")
d.test <- readRDS("saved_objects/d.test.rds")
xgb.RVF.final <- readRDS("saved_objects/xgb.RVF.final.rds")

#==============================================================================


# Inspect XGBoost model and plot ROC curve

xgb.RVF.final %>%
  collect_predictions() %>%
  roc_auc(RVF_presence_f, `.pred_0`)

xgb.RVF.final %>%
  collect_predictions() %>%
  roc_curve(RVF_presence_f, `.pred_0`) %>%
  autoplot()

ggsave(
  "outputs/figures/roc.jpg",
  width = 6, height = 6
)

#==============================================================================


# Calculate accuracy on the test data

pred.object <- get_prediction_object(xgb.RVF.final)

# Extract values
true.negatives <- unlist(pred.object@tn)
false.positives <- unlist(pred.object@fp)
false.negatives <- unlist(pred.object@fn)
true.positives <- unlist(pred.object@tp)

# Calculate TNR, TPR, and TSS
true.negative.rate <- true.negatives / (true.negatives + false.positives)
true.positive.rate <- true.positives / (true.positives + false.negatives)
tss <- true.positive.rate + true.negative.rate - 1

# Look at model predictions using the TSS cutoff
max(tss)
max.tss.index <- which(tss == max(tss))
tss.cutoff <- unlist(pred.object@cutoffs)[max.tss.index]
true.negative.rate[max.tss.index]
true.positive.rate[max.tss.index]
tss.based.preds <- ifelse(unlist(pred.object@predictions) >= tss.cutoff, 1, 0)

table(d.test$RVF_presence, tss.based.preds)

#==============================================================================


# Alternative version of the ROC curve

per.measure <- ROCR::performance(pred.object, "tnr", "fnr")
plot(per.measure, col = "red", lwd = 1)
abline(a = 0, b = 1, lwd = 1, lty = 1, col = "gray")

#==============================================================================


# Variable importance calculations

vi <- xgb.RVF.final %>%
  extract_workflow() %>%
  extract_fit_parsnip() %>%
  vi(type = "gain") %>%
  left_join(
    .,
    read_csv("data/lookup_tables/variable_lookup_table.csv"),
    by = c("Variable" = "variable")
  )

# Plot variable importance values for the top 10 most important predictors
vi %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 10) %>%
  mutate(variable_nice_name = factor(variable_nice_name)) %>%
  ggplot(
    aes(
      x = Importance, 
      y = forcats::fct_reorder(variable_nice_name, Importance), 
      color = variable_group,
      fill = variable_group
    )
  ) +
  geom_col(width = 0.1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(from = 0, to = 0.1, by = 0.02), limits = c(0, 0.1)) +
  xlab("Variable Importance") +
  ylab("") +
  scale_fill_brewer(
    palette = "Set2", name = "Variable Type"
  ) +
  scale_color_brewer(
    palette = "Set2", name = "Variable Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.y = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(
  "outputs/figures/vi.jpg",
  width = 8, height = 6
)

#==============================================================================


# Partial dependence plots

explainer <- explain_tidymodels(
  extract_workflow(xgb.RVF.final),
  data = dplyr::select(d.train, -RVF_presence_f),
  y = as.integer(d.train$RVF_presence_f),
  verbose = FALSE
)

pdp <- model_profile(
  explainer,
  variables = c(
    "travel_time_to_healthcare",
    "goat_density",
    "monthly_precip", "monthly_precip_lag_2",
    "dist_to_river_10", "dist_to_lake_10",
    "phh2o_0-5cm_mean", "silt_0-5cm_mean",
    "monthly_tmax_lag_3",
    "elevation"
  ),
  N = 500
)

plot(
  pdp, 
  title = "", 
  subtitle = "",
  facet_ncol = 3
)

ggsave(
  "outputs/figures/pdp.jpg",
  width = 9, height = 12
)
