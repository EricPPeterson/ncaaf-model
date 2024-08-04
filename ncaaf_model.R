setwd("/Users/ericp/OneDrive/Documents/GitHub/ncaaf-model")
library(dplyr)
library(cfbfastR)
library(ggplot2)
library(progressr)
library(data.table)
library(lookup)
library(tidymodels)
library(broom)
library(yardstick)
library(stringr) 
library(stacks)
library(recipes)
library(parsnip)
library(magrittr)
library(mice)
library(tidyverse)
library(tidymodels)
library(rsample)
library(workflows)
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
#pull data for neutral games
#################################################################################################################
x <- cfbd_game_info(year = 2021)
y <- cfbd_game_info(year = 2022)
z <- cfbd_game_info(year = 2023)
game_info <- bind_rows(x,y,z)
rm(x,y,z)
game_info <- game_info %>% dplyr::filter(home_division == 'fbs' & away_division == 'fbs')
game_info <- game_info %>% dplyr::select(game_id, season, neutral_site, conference_game, venue_id)
sched_str <- left_join(pbp_clean, game_info, by = c('game_id', 'season'))
sched_str <- sched_str %>% select(c(game_id, season, wk, pos_team, def_pos_team, neutral_site, home, away, EPA))
sched_str$neutral_site <- ifelse(is.na(sched_str$neutral_site),'FALSE',sched_str$neutral_site)
sched_str$hfa <- NA
sched_str$hfa <- ifelse(sched_str$neutral_site == 'TRUE',0,1)
sched_str$hfa <- ifelse(sched_str$home == sched_str$def_pos_team & sched_str$neutral_site == FALSE,-1,sched_str$hfa)
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
##################################################################################################################
#split data
##################################################################################################################
data_split_off <- initial_split(off_total_efficiency, prop = 3/4)
train_off <- training(data_split_off)
test_off <- testing(data_split_off)

data_split_def <- initial_split(def_total_efficiency, prop = 3/4)
train_def <- training(data_split_def)
test_def <- testing(data_split_def)
##################################################################################################################
#create recipes for different linear models
##################################################################################################################
lm_start_off <- recipe(total_pts~., data = train_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable')

lm_start_def <- recipe(total_pts~., data = train_def) %>%
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
  fit(data = train_off)
model_summary_off <- off_start_fit %>%
  extract_fit_parsnip() %>%
  tidy()

def_start_fit <- def_start_wflow %>%
  fit(data = train_def)
model_summary_def <- def_start_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
start_preds_off <- predict(off_start_fit, test_off)
bind_preds_off <- bind_cols(start_preds_off, test_off %>% select(total_pts))
rmse(.pred, total_pts, data = bind_preds_off)

start_preds_def <- predict(def_start_fit, test_def)
bind_preds_def <- bind_cols(start_preds_def, test_def %>% select(total_pts))
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
  fit(data = train_off)
model_reduced_summary_off <- off_reduced_fit %>%
  extract_fit_parsnip() %>%
  tidy()

def_reduced_fit <- def_reduced_wflow %>%
  fit(data = train_def)
model_reduced_summary_def <- def_reduced_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
reduced_preds_off <- predict(off_reduced_fit, test_off)
bind_reduced_preds_off <- bind_cols(reduced_preds_off, test_off %>% select(total_pts))
rmse(.pred, total_pts, data = bind_reduced_preds_off)

reduced_preds_def <- predict(def_reduced_fit, test_def)
bind_reduced_preds_def <- bind_cols(reduced_preds_def, test_def %>% select(total_pts))
rmse(.pred, total_pts, data = bind_reduced_preds_def)
##################################################################################################################
#tree based models
##################################################################################################################
##################################################################################################################
#create recipes for different RF off and def
##################################################################################################################
tree_start_off <- recipe(total_pts~., data = train_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

tree_start_def <- recipe(total_pts~., data = train_def) %>%
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
#, trees = 1000, mtry = 55, min_n = 22
tree_mod <- rand_forest(mode = 'regression') %>%
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
  fit(data = train_off)

def_tree_fit <- def_tree_wflow %>%
  fit(data = train_def)
##################################################################################################################
#make predictions
##################################################################################################################
tree_preds_off <- predict(off_tree_fit, test_off)
tree_bind_off <- bind_cols(tree_preds_off, test_off %>% select(total_pts))
rmse(.pred, total_pts, data = tree_bind_off)

tree_preds_def <- predict(def_tree_fit, test_def)
tree_bind_def <- bind_cols(tree_preds_def, test_def %>% select(total_pts))
rmse(.pred, total_pts, data = tree_bind_def)
##################################################################################################################
#neural net model
##################################################################################################################
##################################################################################################################
#select model types / select recipe
##################################################################################################################
nn_off <- recipe(total_pts~., data = train_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

nn_def <- recipe(total_pts~., data = train_def) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#select model types / select engine
##################################################################################################################
#, hidden_units = 2, penalty = 1.9953, epochs = 437
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
  fit(data = train_off)

def_nn_fit <- def_nn_wflow %>%
  fit(data = train_def)
##################################################################################################################
#make predictions
##################################################################################################################
nn_preds_off <- predict(off_nn_fit, test_off)
nn_bind_off <- bind_cols(nn_preds_off, test_off %>% select(total_pts))
rmse(.pred, total_pts, data = nn_bind_off)

nn_preds_def <- predict(def_nn_fit, test_def)
nn_bind_def <- bind_cols(nn_preds_def, test_def %>% select(total_pts))
rmse(.pred, total_pts, data = nn_bind_def)
##################################################################################################################
#gbm model
##################################################################################################################
##################################################################################################################
#select model types / select recipe
##################################################################################################################
gbm_off <- recipe(total_pts~., data = train_off) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

gbm_def <- recipe(total_pts~., data = train_def) %>%
  update_role(game_id, def_pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_success:avg_drive_time) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
##################################################################################################################
#select model types / select engine
##################################################################################################################
#, trees = 1400, min_n = 10, tree_depth = 6, loss_reduction = 20, learn_rate = .01, sample_size = 0.55
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
off_gbm_fit <- off_gbm_wflow %>%
  fit(data = train_off)

def_gbm_fit <- def_gbm_wflow %>%
  fit(data = train_def)
##################################################################################################################
#make predictions
##################################################################################################################
gbm_preds_off <- predict(off_gbm_fit, test_off)
gbm_bind_off <- bind_cols(gbm_preds_off, test_off %>% select(total_pts))
rmse(.pred, total_pts, data = gbm_bind_off)

gbm_preds_def <- predict(def_gbm_fit, test_def)
gbm_bind_def <- bind_cols(gbm_preds_def, test_def %>% select(total_pts))
rmse(.pred, total_pts, data = gbm_bind_def)
##################################################################################################################
#fit all off and def workflows to 5-fold CV
##################################################################################################################
folds_off <- rsample::vfold_cv(off_total_efficiency)
folds_def <- rsample::vfold_cv(def_total_efficiency)
metric <- yardstick::metric_set(rmse)
ctrl_grid <- control_stack_grid() 
ctrl_res <- control_stack_resamples()
##################################################################################################################
#offensive models
##################################################################################################################
set.seed(1234)
#linear model
off_reduced_res <- fit_resamples(
  off_reduced_wflow,
  resamples = folds_off,
  metrics = metric,
  control = ctrl_res
)
#gbm off
off_gbm_spec <- boost_tree(
  trees = tune('trees'),
  min_n = tune('min_n'),
  tree_depth = tune('tree_depth'),
  learn_rate = tune('learn_rate'),
  loss_reduction = tune('loss_reduction'),
  sample_size = tune('sample_size')) %>%
  set_mode('regression') %>%
  set_engine('lightgbm')

gbm_wflow_off <- 
  workflow() %>% 
  add_model(off_gbm_spec) %>%
  add_recipe(gbm_off)

off_gbm_res <- tune_grid(
  gbm_wflow_off,
  resamples = folds_off,
  grid = 6,
  metrics = metric,
  control = ctrl_grid
)

#nn off
off_nn_spec <- mlp(
  hidden_units = tune('hidden_units'),
  penalty = tune('penalty'),
  epochs = tune('epochs')) %>%
  set_mode('regression') %>%
  set_engine('nnet')

nn_wflow_off <- 
  workflow() %>% 
  add_model(off_nn_spec) %>%
  add_recipe(nn_off)

off_nn_res <- tune_grid(
  nn_wflow_off,
  resamples = folds_off,
  grid = 6,
  metrics = metric,
  control = ctrl_grid
)
#random forest
off_rf_spec <- rand_forest(
  min_n = tune('min_n'),
  mtry = tune('mtry'),
  trees = tune('trees')) %>%
  set_mode('regression') %>%
  set_engine('randomForest')

rf_wflow_off <- 
  workflow() %>% 
  add_model(off_rf_spec) %>%
  add_recipe(tree_start_off)

off_rf_res <- tune_grid(
  rf_wflow_off,
  resamples = folds_off,
  grid = 6,
  metrics = metric,
  control = ctrl_grid
)

##################################################################################################################
set.seed(2345)
#lm
def_reduced_res <- fit_resamples(
  def_reduced_wflow,
  resamples = folds_def,
  metrics = metric,
  control = ctrl_res
)
#gbm
def_gbm_spec <- boost_tree(
  trees = tune('trees'),
  min_n = tune('min_n'),
  tree_depth = tune('tree_depth'),
  learn_rate = tune('learn_rate'),
  loss_reduction = tune('loss_reduction'),
  sample_size = tune('sample_size')) %>%
  set_mode('regression') %>%
  set_engine('lightgbm')

gbm_wflow_def <- 
  workflow() %>% 
  add_model(def_gbm_spec) %>%
  add_recipe(gbm_def)

def_gbm_res <- tune_grid(
  gbm_wflow_def,
  resamples = folds_def,
  grid = 6,
  metrics = metric,
  control = ctrl_grid
)

#neural net
def_nn_spec <- mlp(
  hidden_units = tune('hidden_units'),
  penalty = tune('penalty'),
  epochs = tune('epochs')) %>%
  set_mode('regression') %>%
  set_engine('nnet')

nn_wflow_def <- 
  workflow() %>% 
  add_model(def_nn_spec) %>%
  add_recipe(nn_def)

def_nn_res <- tune_grid(
  nn_wflow_def,
  resamples = folds_def,
  grid = 6, 
  metrics = metric,
  control = ctrl_res
)

#random forest
def_rf_spec <- rand_forest(
  min_n = tune('min_n'),
  mtry = tune('mtry'),
  trees = tune('trees')) %>%
  set_mode('regression') %>%
  set_engine('randomForest')

rf_wflow_def <- 
  workflow() %>% 
  add_model(def_rf_spec) %>%
  add_recipe(tree_start_def)

def_rf_res <- tune_grid(
  rf_wflow_def,
  resamples = folds_def,
  grid = 6,
  metrics = metric,
  control = ctrl_res
)
##################################################################################################################
#stack models
##################################################################################################################
library(stacks)
off_stack <- 
  stacks() %>%
  add_candidates(off_reduced_res) %>%
  add_candidates(off_rf_res) %>%
  add_candidates(off_nn_res) %>%
  add_candidates(off_gbm_res)
def_stack <- 
  stacks() %>%
  add_candidates(def_reduced_res) %>%
  add_candidates(def_rf_res) %>%
  add_candidates(def_nn_res) %>%
  add_candidates(def_gbm_res) 
###################################################################################################################
#stack models
###################################################################################################################
off_blend_st <-
  off_stack %>%
  blend_predictions()
def_blend_st <- 
  def_stack %>%
  blend_predictions()
###################################################################################################################
#autoplots
###################################################################################################################
autoplot(off_blend_st)
autoplot(def_blend_st)
autoplot(off_blend_st, type = "members")
autoplot(def_blend_st, type = "members")
autoplot(off_blend_st, type = "weights")
autoplot(def_blend_st, type = "weights")
###################################################################################################################
#fit members
###################################################################################################################
off_blend_st <- off_blend_st %>%
  fit_members()
def_blend_st <- def_blend_st %>%
  fit_members()
###################################################################################################################
#collect parameters
###################################################################################################################
collect_parameters(off_blend_st, "off_rf_res")
collect_parameters(off_blend_st, "off_nn_res")
collect_parameters(off_blend_st, "off_lm_res")
collect_parameters(off_blend_st, "off_gbm_res")
collect_parameters(def_blend_st, "def_rf_res")
collect_parameters(def_blend_st, "def_nn_res")
collect_parameters(def_blend_st, "def_lm_res")
collect_parameters(def_blend_st, "def_gbm_res")
###################################################################################################################
#predict
###################################################################################################################
test_off <- 
  test_off %>%
  bind_cols(predict(off_blend_st, .))
test_def <- 
  test_def %>%
  bind_cols(predict(def_blend_st, .))
###################################################################################################################
#graph preds vs real total_pts
###################################################################################################################
ggplot(test_off) +
  aes(x = total_pts, 
      y = .pred) +
  geom_point() + 
  coord_obs_pred()
ggplot(test_def) +
  aes(x = total_pts, 
      y = .pred) +
  geom_point() + 
  coord_obs_pred()
###################################################################################################################
#preds check
###################################################################################################################
member_preds_off <- 
  test_off %>%
  select(total_pts) %>%
  bind_cols(predict(off_blend_st, test_off, members = TRUE))

map(member_preds_off, rmse_vec, truth = member_preds_off$total_pts) %>%
  as_tibble()

member_preds_def <- 
  test_def %>%
  select(total_pts) %>%
  bind_cols(predict(def_blend_st, test_def, members = TRUE))

map(member_preds_def, rmse_vec, truth = member_preds_def$total_pts) %>%
  as_tibble()
##################################################################################################################
#ridge regression to adj for str of schedule
##################################################################################################################
library(fastDummies)
library(ISLR)
library(glmnet)
colnames(sched_str)[4:5] <- c('Offense', 'Defense')
sched_str_2023 <- sched_str %>% dplyr::filter(season == 2023)

# define the model
lm_mod <-
  linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

#set grid
lambda_grid <-  tibble(penalty = seq(0.001, 10, length.out = 100))

# define the recipe
data_red <- sched_str_2023 %>%
  select(-c(game_id, season, wk, neutral_site, home, away))
lm_rec <- recipe(EPA ~ ., data = data_red) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
# create cross-validation folds
folds <- vfold_cv(data_red, v = 5)
# define the workflow
lm_wflow <-
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_rec)
  
# get the tuning results
lm_res <-
  lm_wflow %>%
  tune_grid(
    resamples = folds,
    grid = lambda_grid,
    control = control_grid(save_pred = TRUE)
    )
  
# get the model with the lowest root mean squared error
lowest_rmse <- lm_res %>% select_best()
  
# finalize the workflow with the best model
lm_final_wf <-
  lm_wflow %>%
  finalize_workflow(lowest_rmse)
  
# get the final fit
lm_final_fit <-
  lm_final_wf %>%
  fit(sched_str_2023 %>% select(-c(game_id, season, wk, neutral_site, home, away)))
  
# extract the coefficients
adjStats <-
  broom::tidy(lm_final_fit) %>%
  separate(term, into = c("side", "team"), sep = "_", fill = "left") %>%
  select(-penalty)

# separate the intercept and home field advantage coefficients
otherTerms <-
  adjStats %>%
  slice(1:2) %>%
  select(-side)
# get the remaining coefficients
adjStats <-
  adjStats %>%
  slice(3:nrow(adjStats))
# add the intercept term to the other coefficients
adjStats <-
  adjStats %>%
  mutate(estimate = estimate + otherTerms %>% filter(team == "(Intercept)") %>% .$estimate)
# make dataframe wider
adjStats <-
  adjStats %>%
  pivot_wider(names_from = side, values_from = estimate) %>%
  rename("adjOff" = "Offense", "adjDef" = "Defense")
# get the unadjusted (raw) stats - although I don't need this for the plot later
rawOff <-
  sched_str_2023 %>% group_by(Offense) %>% summarize(meanEPA = mean(EPA)) %>%
  rename('team' = 'Offense', "rawOff" = "meanEPA")
# same thing for raw defense
rawDef <-
  sched_str_2023 %>% group_by(Defense) %>% summarize(meanEPA = mean(EPA)) %>%
  rename('team' = 'Defense', 'rawDef' = 'meanEPA')

#fix some team names
adjStats$team <- chartr(".", " ", adjStats$team)

adjStats <-
  adjStats %>% mutate(team = case_when(
    team == "Hawai i" ~ "Hawai'i",
    team == "Miami  OH " ~ "Miami (OH)",
    team == "Texas A M" ~ "Texas A&M",
    team == 'Texas A M Commerce' ~ 'Texas A&M Commerce',
    team == 'Alabama A M' ~ 'Alabama A&M',
    team == 'Florida A M' ~ 'Florida A&M',
    team == 'North Carolina A T' ~ 'North Carolina A&T',
    team == 'Northwestern IA' ~ 'Northwestern (IA)',
    team == 'St Francis PA' ~ 'St Francis (PA)',
    team == 'St Andrews' ~ 'St. Andrews',
    team == 'St Thomas MN' ~ 'St. Thomas (MN)',
    team == 'Stephen F Austin' ~ 'Steven F. Austin',
    team == 'Lincoln University CA' ~ 'Lincoln University (CA)',
    team == 'Gardner Webb' ~ 'Gardner-Webb',
    team == 'Arkansas Pine Bluff' ~ 'Arkansas-Pine Bluff',
    team == 'Bethune Cookman' ~ 'Bethune-Cookman',
    team == 'Northwestern IA' ~ 'Northwestern (IA)',
    team == 'William  Mary' ~ 'William & Mary',
    TRUE ~ team
  ))

# bind everything together into one dataframe
adj_final <-
  adjStats %>%
    left_join(rawOff, by = 'team') %>%
    left_join(rawDef, by = 'team') %>%
  dplyr::mutate(off_change = adjOff - rawOff,
                def_change = adjDef - rawDef)
#################################################################################################################
#add offensive and defensive plays
#################################################################################################################
pbp_data_2023 <- pbp_func(2023,2023)
fwrite(pbp_data_2023, 'pbp_data_2023.csv', row.names = FALSE)
pbp_data_2023 <- read.csv("~/GitHub/ncaaf-model/pbp_data_2023.csv")

total_plays <- function(df, pos_def){
  out <- df %>% group_by(!!sym(pos_def)) %>%
    dplyr::summarise(plays = n(),
                     games = n_distinct(game_id),
                     plays_per_game = plays/games)
  x <- ifelse(pos_def == 'pos_team','off_plays_per_game', 'def_plays_per_game')
  colnames(out) <- c('team', 'plays', 'games', x)
  return(out)
}
off_plays <- total_plays(pbp_data_2023, 'pos_team')
def_plays <- total_plays(pbp_data_2023, 'def_pos_team')
plays <- left_join(off_plays, def_plays, by = c('team')) %>%
  select(c('team', 'off_plays_per_game', 'def_plays_per_game'))
###################################################################################################################
#join to strength of schedule
###################################################################################################################
adj_final_plays <- left_join(adj_final, plays, by = c('team'))
adj_pts_final <- adj_final_plays %>%
  dplyr::mutate(off_adj = off_change * off_plays_per_game,
         def_adj = def_change * def_plays_per_game) %>%
  dplyr::select(team, off_adj, def_adj)