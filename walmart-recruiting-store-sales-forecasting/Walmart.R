## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)

## Read in the Data
train <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/train.csv")
test <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/test.csv")
features <- vroom("/Users/nicholasthomas/Desktop/STATISTICS/STAT 348/Walmart/walmart-recruiting-store-sales-forecasting/features.csv")

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


view(imputed_features)
view(train)
