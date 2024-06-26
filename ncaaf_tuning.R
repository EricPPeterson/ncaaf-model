#grid tune models from ncaaf_model.R
library(recipes)
library(rsample)
library(parsnip)
library(workflows)
library(ggplot2)
library(doParallel)

metrics = metric_set(rmse)
#######################################################################################################################
#tune random forest parameters
#######################################################################################################################
off_boot <- bootstraps(train_tree_off, times = 10)

tune_spec <- 
  rand_forest(
    mode = 'regression',
    min_n = tune(),
    mtry = tune(),
    trees = 1000
  ) %>% 
  set_engine("ranger")

tune_flow <- workflow() %>%
  add_recipe(tree_start_off) %>%
  add_model(tune_spec)

set.seed(234)
folds <- vfold_cv(train_tree_off)

doParallel::registerDoParallel()
set.seed(345)
tune_results <- tune_grid(
  tune_flow,
  resamples = folds,
  grid = 20
)
###################################################################################################################
#check best values and graph
###################################################################################################################
tune_results %>% 
  collect_metrics()

tune_results %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = 'rmse')

####################################################################################################################
#check more specific values now that we have a range for best values of min_n and mtry
####################################################################################################################
rf_grid <- grid_regular(
  mtry(range = c(20, 55)),
  min_n(range = c(20, 30)),
  levels = 5
)

rf_grid
####################################################################################################################
#tune parameters again
####################################################################################################################
set.seed(456)
tune_results <- tune_grid(
  tune_flow,
  resamples = folds,
  grid = rf_grid
)
###################################################################################################################
#select best model
###################################################################################################################
best_rmse <- select_best(tune_results, metric = 'rmse')

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf
###################################################################################################################
#tune gbm
###################################################################################################################
set.seed(1)
off_boot_gbm <- bootstraps(train_gbm_off, times = 10)

set.seed(123)
tune_spec_gbm <- 
  boost_tree(
    mode = 'regression',
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune()
     ) %>% 
  set_engine("lightgbm")

set.seed(456)
tune_flow_gbm <- workflow() %>%
  add_recipe(gbm_off) %>%
  add_model(tune_spec_gbm)

set.seed(234)
folds <- vfold_cv(train_gbm_off)

doParallel::registerDoParallel()
set.seed(345)
tune_results_gbm <- tune_grid(
  tune_flow_gbm,
  resamples = folds,
  grid = 20
)
###################################################################################################################
#check best values and graph
###################################################################################################################
tune_results_gbm %>% 
  collect_metrics()

tune_results_gbm %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  select(mean, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size) %>%
  pivot_longer(trees:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = 'rmse')
####################################################################################################################
#check more specific values now that we have a range for best values of min_n and mtry
####################################################################################################################
gbm_grid <- grid_regular(
  learn_rate(range = c(0.005, 0.04)),
  loss_reduction(range = c(25, 30)),
  min_n(range = c(10,20)),
  tree_depth(range = c(6,8)),
  trees(range = c(1400,1700)),
  sample_prop(range = c(0.55,0.75)),
  levels = 5
)

gbm_grid
####################################################################################################################
#tune parameters again
####################################################################################################################
set.seed(999)
tune_results_gbm <- tune_grid(
  tune_flow_gbm,
  resamples = folds,
  grid = gbm_grid
)
###################################################################################################################
#select best model
###################################################################################################################
best_rmse_gbm <- select_best(tune_results_gbm, metric = 'rmse')

final_gbm <- finalize_model(
  tune_spec_gbm,
  best_rmse_gbm
)

final_gbm
###################################################################################################################
#nn parameter tuning
###################################################################################################################
set.seed(1)
off_boot_nn <- bootstraps(train_nn_off, times = 10)

set.seed(123)
tune_spec_nn <- 
  mlp(
    mode = 'regression',
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>% 
  set_engine("nnet")

set.seed(456)
tune_flow_nn <- workflow() %>%
  add_recipe(nn_off) %>%
  add_model(tune_spec_nn)

set.seed(234)
folds <- vfold_cv(train_nn_off)

doParallel::registerDoParallel()
set.seed(345)
tune_results_nn <- tune_grid(
  tune_flow_nn,
  resamples = folds,
  grid = 20
)
###################################################################################################################
#check best values and graph
###################################################################################################################
tune_results_nn %>% 
  collect_metrics()

tune_results_nn %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  select(mean, hidden_units, penalty, epochs) %>%
  pivot_longer(hidden_units:epochs,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = 'rmse')
####################################################################################################################
#check more specific values now that we have a range for best values of parameters
####################################################################################################################
nn_grid <- grid_regular(
  hidden_units(range = (c(1,4))),
  penalty(range = (c(.2,.6))),
  epochs(range = (c(375,500))),
  levels = 5
)

nn_grid
####################################################################################################################
#tune parameters again
####################################################################################################################
set.seed(999)
tune_results_nn <- tune_grid(
  tune_flow_nn,
  resamples = folds,
  grid = nn_grid
)
###################################################################################################################
#select best model
###################################################################################################################
best_rmse_nn <- select_best(tune_results_nn, metric = 'rmse')

final_nn <- finalize_model(
  tune_spec_nn,
  best_rmse_nn
)

final_nn