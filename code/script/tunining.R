library(here)
library(tidymodels)
library(tidyverse)
library(pacman)


tidymodels_prefer() 
#parsnip::model_db |> View()

svm_model=readRDS(here::here('code/RDATA/model_svm_linear.R'))
train_set=readRDS(here::here('code/RDATA/train_set.R'))
test_set=readRDS(here::here('code/RDATA/test_set.R'))

summary(train_set)
str(train_set)

svm_linear = parsnip::svm_linear(cost=tune(),rbf_sigma = tune(), margin=tune()) |>
              set_engine(engine= 'kernlab') |> 
              set_mode('classification')

svm_params =  extract_parameter_set_dials(svm_linear)
svm_params

svm_params |> extract_parameter_dials('cost')
svm_params |>  extract_parameter_dials('margin')

tidyr::crossing(
  cost=1:5,
  margin=c(0,0.1,0.2)
)

dials::grid_regular(svm_params,levels = 3)

svm_params |> 
  dials::grid_regular(levels = c(cost=2,margin=3))


svm_params |> 
  dials::grid_random(size = 1000) |> 
  summary()


pacman::p_load(ggforce)                     
set.seed(108)
svm_params |> 
  dials::grid_random(size = 20) |>
  ggplot(aes(x=cost,y=margin)) +
  geom_point() +
  geom_blank() 
  
# space-filling designs
set.seed(108)
svm_params |> 
  dials::grid_latin_hypercube(size = 20,original = FALSE) |>
  ggplot(aes(x=cost,y=margin)) +
  geom_point() +
  geom_blank() +
  labs(title = "Latin Hypercube design with 20 candidates")

#In model ---------------------------------------------
set.seed(108)
svm_folds = vfold_cv(train_set)

svm_rec = recipe(sleep_disorder ~ ., data=train_set)
svm_rec

svm_linear = parsnip::svm_linear(cost=parsnip::tune(),margin = tune()) |>
  set_engine(engine= 'kernlab') |> 
  set_mode('classification')


svm_workflow = workflow() |> 
                add_model(svm_linear) |> 
                add_recipe(svm_rec) |> 
                step_YeoJohnson(all_numeric_predictors()) %>% 
                step_normalize(all_numeric_predictors()) %>% 
                step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
                step_normalize(all_numeric_predictors())


svm_workflow


str(train_set)

svm_params =  extract_parameter_set_dials(svm_workflow)
svm_params

roc_res <- metric_set(accuracy)
roc_res

svm_reg_tune <- 
  svm_workflow %>% 
  tune_grid( 
    resamples=svm_folds, 
    grid = 2, 
     )


svm_reg_tune

svm_folds

show_best(svm_reg_tune, metric = c('roc_auc','accuracy'))
roc_res <- metric_set(roc_auc)

## exoande grid ----------------------------
### ---------------------------------------
spline_grid <- expand.grid(cost = 2:5, margin = 2:5)
svm_reg_tune <- 
  svm_workflow %>% 
  tune_grid( 
    resamples=svm_folds, 
    grid = spline_grid, 
  )
svm_reg_tune
show_best(svm_reg_tune, metric = 'roc_auc')
show_best(svm_reg_tune, metric = 'accuracy')

### ---------------------------------------
svm_reg_tune <- 
  svm_workflow %>% 
  tune_grid( 
    resamples=svm_folds, 
    grid = grid_random(svm_params)
  )
show_best(svm_reg_tune, metric = 'roc_auc')
show_best(svm_reg_tune, metric = 'accuracy')

### ---------------------------------------

classification_metrics <- yardstick::metric_set(accuracy )
svm_reg_tune <- 
  svm_workflow %>% 
  tune_grid( 
    resamples=svm_folds, 
    grid = grid_regular(svm_params, levels = c(c(cost=2,margin=3))),
    metrics =classification_metrics
  )
show_best(svm_reg_tune)

autoplot(svm_reg_tune) + 
  scale_color_viridis_d(direction = -1) + 
  theme(legend.position = "top")

### -------------------------------
svm_reg_tune <- 
  svm_workflow |> 
  tune_grid( 
    resamples=svm_folds, 
    grid = grid_latin_hypercube(svm_params, size=3),
    metrics =classification_metrics
  )

show_best(svm_reg_tune)

autoplot(svm_reg_tune) + 
  scale_color_viridis_d(direction = -1) + 
  theme(legend.position = "top")


###-----------------------------
###
# To use a space-filling design, either the grid argument can be given an
# integer or one of the grid_*() functions can produce a data frame. To
# evaluate the same range using a maximum entropy design with 20 candidate
###
svm_reg_tune <- 
  svm_workflow |> 
  tune_grid( 
    resamples=svm_folds, 
    grid =20,
    metrics =classification_metrics,
    param_info = svm_params
  )

show_best(svm_reg_tune)

autoplot(svm_reg_tune) + 
  scale_color_viridis_d(direction = -1) + 
  theme(legend.position = "top")

# Final Model  ---------------------------
svm_reg_tune <- 
  svm_workflow |> 
  tune_grid( 
    resamples=svm_folds, 
    grid =20,
    metrics =classification_metrics,
    param_info = svm_params
  )

reg=select_best(svm_reg_tune, metric = "roc_auc")
reg$cost
final_param <- 
  tibble( 
    cost = reg$cost, 
    margin= reg$margin
  ) 

final_param

final_svm_workflow <- 
  svm_workflow %>% 
  finalize_workflow(final_param)
final_svm_workflow

final_svm_fit <- 
  final_svm_workflow %>% 
  fit(test_set)


final_svm_fit  |> 
  extract_fit_engine() |> 
  kernlab::plot()

augment(final_svm_fit,test_set) |> colnames()

augment(final_svm_fit,test_set) |>
  yardstick::roc_auc(truth=sleep_disorder,estimate=c(".pred_Insomnia",".pred_None",".pred_Sleep Apnea"))

augment(final_svm_fit,test_set) |>
  yardstick::roc_auc(truth=sleep_disorder,estimate=c(".pred_Insomnia",".pred_None",".pred_Sleep Apnea")) |> 
  autoplot()



# If you did not use a workflow, finalization of a m,del and/or recipe is done
# using finalize_model() and finalize_recipe().


