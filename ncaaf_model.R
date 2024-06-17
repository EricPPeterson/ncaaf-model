setwd("/Users/ericp/OneDrive/Documents/GitHub/ncaaf-model")
library(dplyr)
library(cfbfastR)
library(ggplot2)
library(progressr)
library(data.table)
library(lookup)
#####################################################################################################################
#function to pull pbp_data
#####################################################################################################################
pbp_func <- function(start,end){
  df <- data.frame()
  while(start <= end){
    for(x in 1:14){
      pbp_data <- cfbd_pbp_data(
        year = c(start),
        season_type = 'regular',
        week = x,
        epa_wpa = TRUE) %>%
        progressr::with_progress()
      df <- bind_rows(df,pbp_data)
      
    }
    start = start + 1
  }
  return(df)
}

pbp_data <- pbp_func(2021,2023)
dplyr::n_distinct(pbp_data$game_id)
fwrite(pbp_data, 'pbp_data.csv', row.names = FALSE)
#####################################################################################################################
pbp_data <- read.csv("~/GitHub/ncaaf-model/pbp_data.csv")
#####################################################################################################################
#filter out end of half / games , etc. NA EPA plays
#####################################################################################################################
data_prep <- function(df){
  for(i in 1:nrow(df)){
    if(is.na(df$wpa[i])){
      df$wpa[i] = df$wp_before[i+1] - df$wp_before[i]
    }
  }
  for(i in 1:nrow(df)){
    if(is.na(df$EPA[i])){
      df$EPA[i] = df$ep_before[i+1] - df$ep_before[i]
    }
  }
  df <- df %>% dplyr::filter(!is.na(EPA)) 
  df <- df %>% dplyr::filter(!is.na(wpa))
  return(df)
}

pbp_clean <- data_prep(pbp_data)
#################################################################################################################
#off and def efficiency
#################################################################################################################
off_efficiency <- function(df, pos_def, oppo_tm, pass_run){
  if(pass_run == 1){
    df <- df %>% dplyr::filter(pass_attempt == 1)
  } else {
    df <- df %>% dplyr::filter(rush == 1)
  }
  out <- df %>% 
    dplyr::group_by(!!sym(pos_def), game_id) %>%
    dplyr::mutate(pass_epa = sum(EPA),
                  pass_wpa = sum(wpa),
                  pass_success = sum(success),
                  pass_ppa = sum(ppa)) %>%
    dplyr::select(game_id, !!sym(pos_def), pass_epa, pass_wpa, pass_success, pass_ppa) %>% 
    distinct()
  if(pass_run == 0) {colnames(out)[3:6] <- c('rush_epa', 'rush_wpa', 'rush_success', 'rush_ppa')}
  return(out)
}
off_pass_eff <- off_efficiency(pbp_clean, 'pos_team', 'def_pos_team',1)
def_pass_eff <- off_efficiency(pbp_clean, 'def_pos_team', 'pos_team',1)
off_rush_eff <- off_efficiency(pbp_clean, 'pos_team', 'def_pos_team',0)
def_rush_eff <- off_efficiency(pbp_clean, 'def_pos_team', 'pos_team',0)
#################################################################################################################
#total plays per team by game
#################################################################################################################
total_plays <- function(df, pos_def, pass_run){
  if(pass_run == 1){
    df <- df %>% dplyr::filter(pass_attempt == 1)
  } else {
    df <- df %>% dplyr::filter(rush == 1)
  }
  
  out <- df %>%
    dplyr::group_by(game_id,!!sym(pos_def)) %>%
    dplyr::summarise(n = n())
  x <- ifelse(pass_run == 1,'pass','run')
  colnames(out)[3] <- paste0(pos_def,'_plays_',x)
  return(out)
}

pos_plays_run <- total_plays(pbp_clean, 'pos_team',0)
def_plays_run <- total_plays(pbp_clean, 'def_pos_team',0)
pos_plays_pass <- total_plays(pbp_clean, 'pos_team',1)
def_plays_pass <- total_plays(pbp_clean, 'def_pos_team',1)

game_plays_pos <- left_join(pos_plays_run, pos_plays_pass, by = c('game_id', 'pos_team'))
game_plays_def <- left_join(def_plays_run, def_plays_pass, by = c('game_id', 'def_pos_team'))
#################################################################################################################
#join plays with efficiency data
#################################################################################################################
off_total_efficiency <- left_join(off_pass_eff,off_rush_eff, by = c('game_id', 'pos_team'))
def_total_efficiency <- left_join(def_pass_eff, def_rush_eff, by = c('game_id', 'def_pos_team'))
off_total_efficiency <- left_join(off_total_efficiency, game_plays_pos, by = c('game_id','pos_team'))
def_total_efficiency <- left_join(def_total_efficiency, game_plays_def, by = c('game_id', 'def_pos_team'))
#################################################################################################################
#total positive EPA / PPA / WP plays
#################################################################################################################
positive_plays <- function(df, pos_def, stat, run_pass){
  df <- df %>% dplyr::filter(!!sym(stat) > 0)
  if(run_pass == 1){
    df <- df %>% dplyr::filter(pass_attempt == 1)
    x <- 'pass'
  } else {
    df <- df %>% dplyr::filter(rush == 1)
    x <- 'rush'
  }
  out <- df %>% 
    dplyr::group_by(game_id, !!sym(pos_def)) %>%
    dplyr::summarize(total_plays = n())
  colnames(out)[3] <- paste0('total_positive_plays_',x,stat)
  return(out)
}
off_pass_EPA_pos <- positive_plays(pbp_clean, 'pos_team', 'EPA', 1)
off_rush_EPA_pos <- positive_plays(pbp_clean, 'pos_team', 'EPA', 0)
off_pass_WP_pos <- positive_plays(pbp_clean, 'pos_team', 'wpa', 1)
off_rush_WP_pos <- positive_plays(pbp_clean, 'pos_team', 'wpa', 0)
off_pass_ppa_pos <- positive_plays(pbp_clean, 'pos_team', 'ppa',1)
off_rush_ppa_pos <- positive_plays(pbp_clean, 'pos_team', 'ppa',0)
off_pass_success_pos <- positive_plays(pbp_clean, 'pos_team', 'success',1)
off_rush_success_pos <- positive_plays(pbp_clean, 'pos_team', 'success',0)

def_pass_EPA_pos <- positive_plays(pbp_clean, 'def_pos_team', 'EPA', 1)
def_rush_EPA_pos <- positive_plays(pbp_clean, 'def_pos_team', 'EPA', 0)
def_pass_WP_pos <- positive_plays(pbp_clean, 'def_pos_team', 'wpa', 1)
def_rush_WP_pos <- positive_plays(pbp_clean, 'def_pos_team', 'wpa', 0)
def_pass_ppa_pos <- positive_plays(pbp_clean, 'def_pos_team', 'ppa',1)
def_rush_ppa_pos <- positive_plays(pbp_clean, 'def_pos_team', 'ppa',0)
def_pass_success_pos <- positive_plays(pbp_clean, 'def_pos_team', 'success',1)
def_rush_success_pos <- positive_plays(pbp_clean, 'def_pos_team', 'success',0)
#################################################################################################################
#combine positive play data to offense and def efficiency
#################################################################################################################
library(purrr)
off_total_efficiency <- list(off_total_efficiency,off_pass_EPA_pos,off_rush_EPA_pos,off_pass_ppa_pos,
                             off_rush_ppa_pos, off_pass_WP_pos, off_rush_WP_pos, off_pass_success_pos,
                             off_rush_success_pos) %>% 
  reduce(left_join, by = c('game_id', 'pos_team'))

def_total_efficiency <- list(def_total_efficiency,def_pass_EPA_pos,def_rush_EPA_pos,def_pass_ppa_pos,
                             def_rush_ppa_pos, def_pass_WP_pos, def_rush_WP_pos, def_pass_success_pos,
                             def_rush_success_pos) %>% 
  reduce(left_join, by = c('game_id', 'def_pos_team'))
#################################################################################################################
#turn efficiency stats into per play stats
#################################################################################################################
off_total_efficiency <- off_total_efficiency %>%
  dplyr::mutate(per_pass_success = pass_success / pos_team_plays_pass,
         per_rush_success = rush_success / pos_team_plays_run,
         per_pass_epa = pass_epa / pos_team_plays_pass,
         per_rush_epa = rush_epa / pos_team_plays_run,
         per_pass_wpa = pass_wpa / pos_team_plays_pass,
         per_rush_wpa = rush_wpa / pos_team_plays_run)
def_total_efficiency <- def_total_efficiency %>%
  dplyr::mutate(per_pass_success = pass_success / def_pos_team_plays_pass,
         per_rush_success = rush_success / def_pos_team_plays_run,
         per_pass_epa = pass_epa / def_pos_team_plays_pass,
         per_rush_epa = rush_epa / def_pos_team_plays_run,
         per_pass_wpa = pass_wpa / def_pos_team_plays_pass,
         per_rush_wpa = rush_wpa / def_pos_team_plays_run)
#################################################################################################################
#pct plays positive vs negative
#################################################################################################################
off_total_efficiency <- off_total_efficiency %>%
  dplyr::mutate(pos_pass_epa_pct = total_positive_plays_passEPA / pos_team_plays_pass,
                pos_pass_wp_pct = total_positive_plays_passwpa / pos_team_plays_pass,
                pos_pass_success_pct = total_positive_plays_passsuccess / pos_team_plays_pass,
                pos_pass_ppa_pct = total_positive_plays_passppa / pos_team_plays_pass,
                neg_pass_epa_pct = (pos_team_plays_pass - total_positive_plays_passEPA) / pos_team_plays_pass,
                neg_pass_wp_pct = (pos_team_plays_pass - total_positive_plays_passwpa) / pos_team_plays_pass,
                neg_pass_success_pct = (pos_team_plays_pass - total_positive_plays_passsuccess) / pos_team_plays_pass,
                neg_pass_ppa_pct = (pos_team_plays_pass - total_positive_plays_passppa) / pos_team_plays_pass,
                pos_rush_epa_pct = total_positive_plays_rushEPA / pos_team_plays_run,
                pos_rush_wp_pct = total_positive_plays_rushwpa / pos_team_plays_run,
                pos_rush_success_pct = total_positive_plays_rushsuccess / pos_team_plays_run,
                pos_rush_ppa_pct = total_positive_plays_rushppa / pos_team_plays_run,
                neg_rush_epa_pct = (pos_team_plays_run - total_positive_plays_rushEPA) / pos_team_plays_run,
                neg_rush_wp_pct = (pos_team_plays_run - total_positive_plays_rushwpa) / pos_team_plays_run,
                neg_rush_success_pct = (pos_team_plays_run - total_positive_plays_rushsuccess) / pos_team_plays_run,
                neg_rush_ppa_pct = (pos_team_plays_run - total_positive_plays_rushppa) / pos_team_plays_run)

def_total_efficiency <- def_total_efficiency %>%
  dplyr::mutate(def_pos_pass_epa_pct = total_positive_plays_passEPA / def_pos_team_plays_pass,
                def_pos_pass_wp_pct = total_positive_plays_passwpa / def_pos_team_plays_pass,
                def_pos_pass_success_pct = total_positive_plays_passsuccess / def_pos_team_plays_pass,
                def_pos_pass_ppa_pct = total_positive_plays_passppa / def_pos_team_plays_pass,
                def_neg_pass_epa_pct = (def_pos_team_plays_pass - total_positive_plays_passEPA) / def_pos_team_plays_pass,
                def_neg_pass_wp_pct = (def_pos_team_plays_pass - total_positive_plays_passwpa) / def_pos_team_plays_pass,
                def_neg_pass_success_pct = (def_pos_team_plays_pass - total_positive_plays_passsuccess) / def_pos_team_plays_pass,
                def_neg_pass_ppa_pct = (def_pos_team_plays_pass - total_positive_plays_passppa) / def_pos_team_plays_pass,
                def_pos_rush_epa_pct = total_positive_plays_rushEPA / def_pos_team_plays_run,
                def_pos_rush_wp_pct = total_positive_plays_rushwpa / def_pos_team_plays_run,
                def_pos_rush_success_pct = total_positive_plays_rushsuccess / def_pos_team_plays_run,
                def_pos_rush_ppa_pct = total_positive_plays_rushppa / def_pos_team_plays_run,
                def_neg_rush_epa_pct = (def_pos_team_plays_run - total_positive_plays_rushEPA) / def_pos_team_plays_run,
                def_neg_rush_wp_pct = (def_pos_team_plays_run - total_positive_plays_rushwpa) / def_pos_team_plays_run,
                def_neg_rush_success_pct = (def_pos_team_plays_run - total_positive_plays_rushsuccess) / def_pos_team_plays_run,
                def_neg_rush_ppa_pct = (def_pos_team_plays_run - total_positive_plays_rushppa) / def_pos_team_plays_run)
#################################################################################################################
#efficiency by down
#################################################################################################################
down_efficiency <- function(df, play_type, pos_def, dwn){
  if(play_type == 1){
    df <- df %>%
      dplyr::filter(pass_attempt == 1) %>%
      dplyr::filter(down == dwn)
  } else {
    df <- df %>%
      dplyr::filter(rush == 1) %>%
      dplyr::filter(down == dwn)    
  }
 
  out <- df %>%
    dplyr::group_by(game_id, !!sym(pos_def)) %>%
    dplyr::summarise(total_epa = sum(EPA),
                     total_wpa = sum(wpa),
                     total_success = sum(success),
                     total_downs = n(),
                     success_rate = total_success / total_downs,
                     avg_ytg_log = mean(log_ydstogo))
   x <- ifelse(play_type == 1,'pass','rush')  
   colnames(out)[3:8] <- paste(colnames(out)[3:8],x,dwn,sep = '_') 
   return(out)
}

off_pass_eff_1 <- down_efficiency(pbp_clean,1,'pos_team',1)
off_pass_eff_2 <- down_efficiency(pbp_clean,1,'pos_team',2)
off_pass_eff_3 <- down_efficiency(pbp_clean,1,'pos_team',3)
off_rush_eff_1 <- down_efficiency(pbp_clean,0,'pos_team',1)
off_rush_eff_2 <- down_efficiency(pbp_clean,0,'pos_team',2)
off_rush_eff_3 <- down_efficiency(pbp_clean,0,'pos_team',3)

def_pass_eff_1 <- down_efficiency(pbp_clean,1,'def_pos_team',1)
def_pass_eff_2 <- down_efficiency(pbp_clean,1,'def_pos_team',2)
def_pass_eff_3 <- down_efficiency(pbp_clean,1,'def_pos_team',3)
def_rush_eff_1 <- down_efficiency(pbp_clean,0,'def_pos_team',1)
def_rush_eff_2 <- down_efficiency(pbp_clean,0,'def_pos_team',2)
def_rush_eff_3 <- down_efficiency(pbp_clean,0,'def_pos_team',3)
##################################################################################################################
#time per drive and drives per game
##################################################################################################################
drive_function <- function(df, pos_def){
  df <- df %>%
    mutate(drive_time_seconds = (60*drive_time_minutes_elapsed) + drive_time_seconds_elapsed)
  out <- df %>%
    group_by(game_id, !!sym(pos_def)) %>%
      dplyr::summarise(total_drives = n_distinct(id_drive),
                       avg_drive_time = mean(drive_time_seconds))
  return(out)
}

avg_dr_off <- drive_function(pbp_clean, 'pos_team')
avg_dr_def <- drive_function(pbp_clean, 'def_pos_team')
##################################################################################################################
#combine play and drive data with efficiency DFs
##################################################################################################################
library(tidyverse)
off_total_efficiency <- list(off_total_efficiency,off_pass_eff_1,off_pass_eff_2,off_pass_eff_3,
                             off_rush_eff_1, off_rush_eff_2, off_rush_eff_3) %>% 
  reduce(left_join, by = c('game_id', 'pos_team'))
def_total_efficiency <- list(def_total_efficiency,def_pass_eff_1,def_pass_eff_2,def_pass_eff_3,
                             def_rush_eff_1, def_rush_eff_2, def_rush_eff_3) %>% 
  reduce(left_join, by = c('game_id', 'def_pos_team'))
off_total_efficiency <- left_join(off_total_efficiency, avg_dr_off, by = c('game_id', 'pos_team'))
def_total_efficiency <- left_join(def_total_efficiency, avg_dr_def, by = c('game_id', 'def_pos_team'))
##################################################################################################################
#touchdown points
##################################################################################################################
td_pts <- function(df, pos_def){
  out <- df %>%
    dplyr::group_by(game_id, !!sym(pos_def)) %>%
    dplyr::summarise(td_pts = sum(touchdown) * 7)
  return(out)
}

off_tds <- td_pts(pbp_clean, 'pos_team')
def_tds <- td_pts(pbp_clean, 'def_pos_team')
##################################################################################################################
#expected FG points
##################################################################################################################
exp_fg <- function(df, pos_def){
  out <- df %>%
    dplyr::group_by(game_id, !!sym(pos_def)) %>%
    dplyr::filter(fg_inds == 1) %>%
    dplyr::summarise(fgs_pts = sum(3 * fg_make_prob))
  out$fgs_pts <- ifelse(is.na(out$fgs_pts),0,out$fgs_pts)
  return(out)
}

off_fg_pts <- exp_fg(pbp_clean, 'pos_team')
def_fg_pts <- exp_fg(pbp_clean, 'def_pos_team')
##################################################################################################################
#total points offense
##################################################################################################################
off_total_pts <- left_join(off_tds, off_fg_pts, by = c('game_id', 'pos_team'))
off_total_pts$fgs_pts <- ifelse(is.na(off_total_pts$fgs_pts), 0, off_total_pts$fgs_pts)
off_total_pts <- off_total_pts %>%
  dplyr::mutate(total_pts = td_pts + fgs_pts)
off_total_pts <- off_total_pts %>% select(-c(3,4))
##################################################################################################################
#total points defense
##################################################################################################################
def_total_pts <- left_join(def_tds, def_fg_pts, by = c('game_id', 'def_pos_team'))
def_total_pts$fgs_pts <- ifelse(is.na(def_total_pts$fgs_pts), 0, def_total_pts$fgs_pts)
def_total_pts <- def_total_pts %>%
  dplyr::mutate(total_pts = td_pts + fgs_pts)
def_total_pts <- def_total_pts %>% select(-c(3,4))
##################################################################################################################
#join pts to offensive efficiency
##################################################################################################################
library(mice)
off_total_efficiency <- left_join(off_total_efficiency, off_total_pts, by = c('game_id', 'pos_team'))
off_total_efficiency <- off_total_efficiency %>% 
  dplyr::select(-c(11:20, 79))
lapply(off_total_efficiency, function(x) { length(which(is.na(x)))})
temp_off_eff <- mice(off_total_efficiency, m = 5, maxit = 5, meth = 'rf', seed = 500)
summary(temp_off_eff)
off_total_efficiency <- complete(temp_off_eff,1)
sum(is.na(off_total_efficiency))
off_total_efficiency <- off_total_efficiency[complete.cases(off_total_efficiency),]
##################################################################################################################
#join pts to defensive efficiency
##################################################################################################################
def_total_efficiency <- left_join(def_total_efficiency, def_total_pts, by = c('game_id', 'def_pos_team'))
def_total_efficiency <- def_total_efficiency %>% 
  dplyr::select(-c(11:20, 79))
lapply(def_total_efficiency, function(x) { length(which(is.na(x)))})
temp_def_eff <- mice(def_total_efficiency, m = 5, maxit = 5, meth = 'rf', seed = 500)
summary(temp_def_eff)
def_total_efficiency <- complete(temp_def_eff,1)
sum(is.na(def_total_efficiency))
def_total_efficiency <- def_total_efficiency[complete.cases(def_total_efficiency),]
##################################################################################################################
#linear model
##################################################################################################################
library(tidyverse)
library(tidymodels)
##################################################################################################################
#split data
##################################################################################################################
data_split_lm_off <- initial_split(off_total_efficiency, prop = 3/4)
train_off_lm <- training(data_split_lm_off)
test_off_lm <- testing(data_split_lm_off)

data_split_lm_def <- initial_split(def_total_efficiency, prop = 3/4)
train_def_lm <- training(data_split_lm_def)
test_def_lm <- testing(data_split_lm_def)
##################################################################################################################
#create recipes for different linear models
##################################################################################################################
lm_start_off <- recipe(total_pts~., data = train_off_lm) %>%
  update_role(game_id, pos_team, new_role = 'id_variable')

lm_start_def <- recipe(total_pts~., data = train_def_lm) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable')
##################################################################################################################
#select model types / select engine
##################################################################################################################
lm_mod <- linear_reg(mode = 'regression', engine = 'glm')
##################################################################################################################
# set workflows
##################################################################################################################
off_start_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_start_off)

def_start_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_start_def)
##################################################################################################################
#fit models
##################################################################################################################
off_start_fit <- off_start_wflow %>%
  fit(data = train_off_lm)
model_summary_off <- off_start_fit %>%
  extract_fit_parsnip() %>%
  tidy()

def_start_fit <- def_start_wflow %>%
  fit(data = train_def_lm)
model_summary_def <- def_start_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
start_preds_off <- predict(off_start_fit, test_off_lm)
bind_preds_off <- bind_cols(start_preds_off, test_off_lm %>% select(total_pts))
rmse(.pred, total_pts, data = bind_preds_off)

start_preds_def <- predict(def_start_fit, test_def_lm)
bind_preds_def <- bind_cols(start_preds_def, test_def_lm %>% select(total_pts))
rmse(.pred, total_pts, data = bind_preds_def)
##################################################################################################################
#collect significant variables
##################################################################################################################
sig_names_off <- model_summary_off %>% 
    dplyr::filter(p.value < 0.1)
sig_names_def <- model_summary_def %>%
  dplyr::filter(p.value < 0.1)
sig_names_off <- list(sig_names_off$term[sig_names_off$p.value < 0.1])
sig_names_off <- sig_names_off[[1]][-1]
sig_names_def <- list(sig_names_def$term[sig_names_def$p.value < 0.1])
sig_names_def <- sig_names_def[[1]][-1]
new_cols_off <- c('game_id', 'pos_team', 'total_pts')
new_cols_def <- c('game_id', 'def_pos_team', 'total_pts')
sig_names_off <- append(sig_names_off, new_cols_off)
sig_names_def <- append(sig_names_def, new_cols_def)
off_tot_eff_sig <- off_total_efficiency %>%
  dplyr::select(all_of(sig_names_off))
def_tot_eff_sig <- def_total_efficiency %>%
  dplyr::select(all_of(sig_names_def))
##################################################################################################################
#visualizations
##################################################################################################################
#do ggplot loops here
##################################################################################################################
#feature engineering / interactions / transformations
##################################################################################################################
lm_reduced_off <- recipe(total_pts~., data = off_tot_eff_sig) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_ppa:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

lm_reduced_def <- recipe(total_pts~., data = def_tot_eff_sig) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_ppa:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#split data
##################################################################################################################
data_split_reduced_off <- initial_split(off_tot_eff_sig, prop = 3/4)
train_reduced_off <- training(data_split_reduced_off)
test_reduced_off <- testing(data_split_reduced_off)

data_split_reduced_def <- initial_split(def_tot_eff_sig, prop = 3/4)
train_reduced_def <- training(data_split_reduced_def)
test_reduced_def <- testing(data_split_reduced_def)
##################################################################################################################
# set workflows
##################################################################################################################
off_reduced_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_reduced_off)

def_reduced_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_reduced_def)
##################################################################################################################
#fit models
##################################################################################################################
off_reduced_fit <- off_reduced_wflow %>%
  fit(data = train_reduced_off)
model_reduced_summary_off <- off_reduced_fit %>%
  extract_fit_parsnip() %>%
  tidy()

def_reduced_fit <- def_reduced_wflow %>%
  fit(data = train_reduced_def)
model_reduced_summary_def <- def_reduced_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
reduced_preds_off <- predict(off_reduced_fit, test_reduced_off)
bind_reduced_preds_off <- bind_cols(reduced_preds, test_reduced_off %>% select(total_pts))
rmse(.pred, total_pts, data = bind_reduced_preds)

reduced_preds_def <- predict(def_reduced_fit, test_reduced_def)
bind_reduced_preds_off <- bind_cols(reduced_preds, test_reduced_def %>% select(total_pts))
rmse(.pred, total_pts, data = bind_reduced_preds)
##################################################################################################################
#tree based models
##################################################################################################################
##################################################################################################################
#split data
##################################################################################################################
data_split_tree_off <- initial_split(off_total_efficiency, prop = 3/4)
train_tree_off <- training(data_split_tree_off)
test_tree_off <- testing(data_split_tree_off)

data_split_tree_def <- initial_split(def_total_efficiency, prop = 3/4)
train_tree_def <- training(data_split_tree_def)
test_tree_def <- testing(data_split_tree_def)
##################################################################################################################
#create recipes for different RF off and def
##################################################################################################################
tree_start_off <- recipe(total_pts~., data = train_tree_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

tree_start_def <- recipe(total_pts~., data = train_tree_def) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#select model types / select engine
##################################################################################################################
library(randomForest)
tree_mod <- rand_forest(mode = 'regression', trees = 500, mtry = 3) %>%
  set_engine('randomForest')
##################################################################################################################
# set workflows
##################################################################################################################
off_tree_wflow <- workflow() %>%
  add_model(tree_mod) %>%
  add_recipe(tree_start_off)

def_tree_wflow <- workflow() %>%
  add_model(tree_mod) %>%
  add_recipe(tree_start_def)
##################################################################################################################
#fit models
##################################################################################################################
off_tree_fit <- off_tree_wflow %>%
  fit(data = train_tree_off)

def_tree_fit <- def_tree_wflow %>%
  fit(data = train_tree_def)
##################################################################################################################
#make predictions
##################################################################################################################
tree_preds_off <- predict(off_tree_fit, test_tree_off)
tree_bind_off <- bind_cols(tree_preds_off, test_tree_off %>% select(total_pts))
rmse(.pred, total_pts, data = tree_bind_off)

tree_preds_def <- predict(def_tree_fit, test_tree_def)
tree_bind_def <- bind_cols(tree_preds_def, test_tree_def %>% select(total_pts))
rmse(.pred, total_pts, data = tree_bind_def)
##################################################################################################################
#neural net model
##################################################################################################################
data_split_nn_off <- initial_split(off_total_efficiency, prop = 3/4)
train_nn_off <- training(data_split_tree)
test_nn_off <- testing(data_split_tree)

data_split_nn_def <- initial_split(def_total_efficiency, prop = 3/4)
train_nn_def <- training(data_split_tree_def)
test_nn_def <- testing(data_split_tree_def)
##################################################################################################################
#select model types / select recipe
##################################################################################################################
nn_off <- recipe(total_pts~., data = train_tree_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

nn_def <- recipe(total_pts~., data = train_tree_def) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#select model types / select engine
##################################################################################################################
nn_mod <- mlp(mode = 'regression') %>%
  set_engine('nnet')
##################################################################################################################
# set workflows
##################################################################################################################
off_nn_wflow <- workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(nn_off)

def_nn_wflow <- workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(nn_def)
##################################################################################################################
#fit models
##################################################################################################################
off_nn_fit <- off_nn_wflow %>%
  fit(data = train_nn_off)

def_nn_fit <- def_nn_wflow %>%
  fit(data = train_nn_def)
##################################################################################################################
#make predictions
##################################################################################################################
nn_preds_off <- predict(off_nn_fit, test_nn_off)
nn_bind_off <- bind_cols(nn_preds_off, test_nn_off %>% select(total_pts))
rmse(.pred, total_pts, data = nn_bind_off)

nn_preds_def <- predict(def_nn_fit, test_nn_def)
nn_bind_def <- bind_cols(nn_preds_def, test_nn_def %>% select(total_pts))
rmse(.pred, total_pts, data = nn_bind_def)
##################################################################################################################
#gbm model
##################################################################################################################
data_split_gbm_off <- initial_split(off_total_efficiency, prop = 3/4)
train_gbm_off <- training(data_split_gbm_off)
test_gbm_off <- testing(data_split_gbm_off)

data_split_gbm_def <- initial_split(def_total_efficiency, prop = 3/4)
train_gbm_def <- training(data_split_tree_def)
test_gbm_def <- testing(data_split_tree_def)
##################################################################################################################
#select model types / select recipe
##################################################################################################################
gbm_off <- recipe(total_pts~., data = train_tree_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

gbm_def <- recipe(total_pts~., data = train_tree_def) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#select model types / select engine
##################################################################################################################
gbm_mod <- boost_tree(mode = 'regression') %>%
  set_engine('lightgbm')
##################################################################################################################
# set workflows
##################################################################################################################
library(lightgbm)
library(bonsai)
off_gbm_wflow <- workflow() %>%
  add_model(gbm_mod) %>%
  add_recipe(gbm_off)

def_gbm_wflow <- workflow() %>%
  add_model(gbm_mod) %>%
  add_recipe(gbm_def)
##################################################################################################################
#fit models
##################################################################################################################
def_gbm_fit <- off_gbm_wflow %>%
  fit(data = train_gbm_off)

def_gbm_fit <- def_gbm_wflow %>%
  fit(data = train_gbm_def)
##################################################################################################################
#make predictions
##################################################################################################################
gbm_preds_off <- predict(off_gbm_fit, test_gbm_off)
gbm_bind_off <- bind_cols(gbm_preds_off, test_gbm_off %>% select(total_pts))
rmse(.pred, total_pts, data = gbm_bind_off)

gbm_preds_def <- predict(def_gbm_fit, test_gbm_def)
gbm_bind_def <- bind_cols(gbm_preds_def, test_gbm_def %>% select(total_pts))
rmse(.pred, total_pts, data = gbm_bind_def)
##################################################################################################################
#stack models
##################################################################################################################
library(stacks)
off_stack <- 
  stacks() %>%
  add_candidates(off_reduced_wflow) %>%
  add_candidates(off_tree_wflow) %>%
  add_candidates(off_nn_wflow) %>%
  add_candidates(off_gbm_wflow)