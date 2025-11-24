## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(lubridate)

## Read in the Data
train <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/train.csv")
test <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/test.csv")
features <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/features.csv")
stores <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/stores.csv")

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date),
              
              Superbowl_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-02-12"), by = "day", length.out = 7),
                seq.Date(as.Date("2011-02-11"), by = "day", length.out = 7),
                seq.Date(as.Date("2012-02-10"), by = "day", length.out = 7),
                seq.Date(as.Date("2013-02-08"), by = "day", length.out = 7)
              ))),
              
              LaborDay_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-09-10"), by = "day", length.out = 7),
                seq.Date(as.Date("2011-09-09"), by = "day", length.out = 7),
                seq.Date(as.Date("2012-09-07"), by = "day", length.out = 7),
                seq.Date(as.Date("2013-09-06"), by = "day", length.out = 7)
              ))),
              
              
              Thanksgiving_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-11-26"), by = "day", length.out = 7),
                seq.Date(as.Date("2011-11-25"), by = "day", length.out = 7),
                seq.Date(as.Date("2012-11-23"), by = "day", length.out = 7),
                seq.Date(as.Date("2013-11-29"), by = "day", length.out = 7)
              ))),
              
              Christmas_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-12-24"), by="day", length.out=7),
                seq.Date(as.Date("2011-12-23"), by="day", length.out=7),
                seq.Date(as.Date("2012-12-21"), by="day", length.out=7),
                seq.Date(as.Date("2013-12-20"), by="day", length.out=7)
              ))),
              BlackFriday_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-11-26"), by="day", length.out=7),
                seq.Date(as.Date("2011-11-25"), by="day", length.out=7),
                seq.Date(as.Date("2012-11-23"), by="day", length.out=7),
                seq.Date(as.Date("2013-11-29"), by="day", length.out=7)
              ))),
              FourthJuly_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-07-02"), by="day", length.out=7),
                seq.Date(as.Date("2011-07-01"), by="day", length.out=7),
                seq.Date(as.Date("2012-06-29"), by="day", length.out=7),
                seq.Date(as.Date("2013-07-05"), by="day", length.out=7)
              ))),
              MemorialDay_Flag = as.integer(Date %in% as.Date(c(
                seq.Date(as.Date("2010-05-28"), by="day", length.out=7),
                seq.Date(as.Date("2011-05-27"), by="day", length.out=7),
                seq.Date(as.Date("2012-05-25"), by="day", length.out=7),
                seq.Date(as.Date("2013-05-24"), by="day", length.out=7)
              )))
              
              ) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))



train_full <- train %>%
  left_join(imputed_features %>% select(-IsHoliday), by = c("Store", "Date")) %>%
  left_join(stores, by = "Store")
train_full <- train_full %>% drop_na(Weekly_Sales)


sample_store_depts <- train_full %>%
  distinct(Store, Dept) %>%
  slice_sample(n = 20)   # change 20 to whatever sample size you want

train_sample <- train_full %>%
  semi_join(sample_store_depts, by = c("Store", "Dept"))



sales_rec <- recipe(Weekly_Sales ~ ., data = train_sample) %>%
  step_rm(Date) %>%
  step_mutate(
    Store = factor(Store),
    Dept  = factor(Dept),
    Type  = factor(Type)
  ) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

sales_prep <- prep(sales_rec)

train_baked <- bake(sales_prep, new_data = NULL)

n_pred <- train_baked %>%
  select(-Weekly_Sales) %>%
  ncol()


folds <- vfold_cv(train_sample, v = 5)


# Linear Model
lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(sales_rec)

time_lm <- system.time({
  lm_res <- fit_resamples(
    lm_wf,
    resamples = folds,
    metrics   = metric_set(rmse, rsq),
    control   = control_resamples(save_pred = TRUE)
  )
})

lm_metrics <- lm_res %>% collect_metrics()
lm_metrics

# Random Forest
rf_spec <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(sales_rec)

rf_params <- parameters(
  mtry(range = c(1L, n_pred)),
  min_n(range = c(2L, 50L))   # adjust upper bound if you want
)

rf_grid <- grid_space_filling(rf_params, size = 20)

time_rf <- system.time({
  rf_res <- tune_grid(
    rf_wf,
    resamples = folds,
    grid      = rf_grid,
    metrics   = metric_set(rmse, rsq),
    control   = control_grid(save_pred = TRUE)
  )
})

rf_best <- rf_res %>% tune::select_best(metric = "rmse")

rf_metrics <- rf_res %>%
  collect_metrics() %>%
  filter(.config == rf_best$.config)

rf_metrics
