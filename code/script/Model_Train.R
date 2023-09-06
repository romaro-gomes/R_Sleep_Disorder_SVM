library(here)
library(tidymodels)
library(tidyverse)
library(rpart.plot)

# Import Data --------------------------

data_eda=readRDS(here('code/RDATA/data_eda.R'))


# Split ----------------------------------
parsnip::model_db |> View()
#parsnip_addin()
tidymodels_prefer() 

data$occupation |> table()


set.seed(1)
split <- rsample::initial_split(data_eda, prop = .80,strata = sleep_disorder)

train_set <- rsample::training(split)
test_set <- rsample::testing(split)

#saveRDS(train_set,here('code/RDATA/train_set.R'))
#saveRDS(test_set,here('code/RDATA/test_set.R'))

train_set$occupation |> table()
test_set$occupation |> table()


# Train --------------------------------------------

## Decision Tree ------------------------------
model_decision_tree=parsnip::decision_tree(mode='classification', engine = 'rpart')
model_decision_tree_fit= model_decision_tree |> fit( train_set$sleep_disorder ~ ., data = train_set) 
graph_model_decision_tree_fit <- extract_fit_engine(model_decision_tree_fit)
rpart.plot::rpart.plot(graph_model_decision_tree_fit)

## SVM Linear  ---------------------------------------------------
model_svm_linear=parsnip::svm_linear(mode ='classification',engine = 'kernlab')
model_svm_linear_fit= model_svm_linear |> fit( train_set$sleep_disorder ~ ., data = train_set) 
model_svm_linear_fit

## SVM Radial  ---------------------------------------------------
model_svm_radial=parsnip::svm_rbf(mode ='classification',engine = 'kernlab')
model_svm_radial_fit= model_svm_radial |> fit( train_set$sleep_disorder ~ ., data = train_set) 
model_svm_radial_fit


## Random Flores ---------------------------------------------
model_random_florest= rand_forest() |> 
  set_engine('randomForest') |> 
  set_mode('classification')

model_random_florest_workflow =  workflow() |>
  add_model(model_random_florest) |> 
  add_formula(sleep_disorder ~ .)

model_random_florest_fit = fit(model_random_florest_workflow, train_set)



# Predict and Evaluation ----------------------------

## Preprocess ------------------------------
test_set_without_class=subset(test_set,select = -c(sleep_disorder))
test_set_only_class=subset(test_set,select = c(sleep_disorder))

test_set_without_class |> View()
test_set_only_class 

classification_metrics <- yardstick::metric_set(accuracy, mcc, f_meas)

## Decision Tree ----------------------
predict(model_decision_tree_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  yardstick::conf_mat(truth = sleep_disorder, estimate = .pred_class)

predict(model_decision_tree_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  classification_metrics(truth = sleep_disorder, estimate = .pred_class)


## SVM  Linear ----------------------
predict(model_svm_linear_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  yardstick::conf_mat(truth = sleep_disorder, estimate = .pred_class)

predict(model_svm_linear_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  classification_metrics(truth = sleep_disorder, estimate = .pred_class)

#saveRDS(model_svm_linear_fit,here('code/RDATA/model_svm_linear.R'))
## SVM  Radial ----------------------
predict(model_svm_radial_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  yardstick::conf_mat(truth = sleep_disorder, estimate = .pred_class)

predict(model_svm_radial_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  classification_metrics(truth = sleep_disorder, estimate = .pred_class)



## Random Florest  ----------------------
predict(model_random_florest_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  yardstick::conf_mat(truth = sleep_disorder, estimate = .pred_class)

predict(model_random_florest_fit,test_set_without_class ) |>
  dplyr::bind_cols(test_set_only_class)  |> 
  classification_metrics(truth = sleep_disorder, estimate = .pred_class)
