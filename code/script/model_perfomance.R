library(here)
library(tidymodels)
library(tidyverse)

svm_model=readRDS(here::here('code/RDATA/model_svm_linear.R'))
train_set=readRDS(here::here('code/RDATA/train_set.R'))
test_set=readRDS(here::here('code/RDATA/test_set.R'))

# Cross-Validation ------------------------------- 
set.seed(42)

train_folds = rsample::vfold_cv(train_set, v=10, repeats =  5)
rsample::mc_cv(train_set,prop = 9/10, times = 20)

train_folds$splits[[1]] %>% analysis()


# Validation --------------------
set.seed(42)
val_set = rsample::validation_split(train_set,prop = 3/4)
val_set

# Boostrap --------------------
rsample::bootstraps(train_set,times=5)

# Analysis perfomanace ---------------

## Set params ---------------------
keep_pred <- tune::control_resamples(save_pred = TRUE, save_workflow = 
                                 TRUE)
metrics = metric_set(accuracy, mcc, f_meas)

## Create workflow ---------------------
raw_svm_model = svm_linear() |> set_engine('kernlab') |>  set_mode('classification')
model_workflow = workflow() |> add_model(raw_svm_model) |> add_formula(sleep_disorder ~.)

## Cross-Fold Performance -------------------------------
model_workflow |> fit_resamples(resamples=train_folds,metrics=metrics,control=keep_pred) |> collect_metrics()
model_workflow |> fit_resamples(resamples=train_folds,metrics=metrics,control=keep_pred) |> collect_predictions()

## Validation Perfomance --------------------------------------------
model_workflow |> fit_resamples(resamples=val_set,metrics=metrics,control=keep_pred) |> collect_metrics()
model_workflow |> fit_resamples(resamples=val_set,metrics=metrics,control=keep_pred) |> collect_predictions()




