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
####################################################################################################################
#graph with new grid
####################################################################################################################
tune_results %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = 'rmse')
###################################################################################################################
#select best model
###################################################################################################################
best_rmse <- select_best(tune_results, metric = 'rmse')

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf