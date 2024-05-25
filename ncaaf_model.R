setwd("/Users/ericp/OneDrive/Documents/GitHub/ncaaf-model")
library(dplyr)
library(cfbfastR)
library(ggplot2)
library(progressr)
library(data.table)

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
                  pass_success = sum(success)) %>%
    dplyr::select(game_id, !!sym(pos_def), pass_epa, pass_wpa, pass_success) %>% 
    distinct()
  if(pass_run == 0) {colnames(out)[3:5] <- c('rush_epa', 'rush_wpa', 'rush_success')}
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
library(lookup)
off_total_efficiency <- left_join(off_pass_eff,off_rush_eff, by = c('game_id', 'pos_team'))
def_total_efficiency <- left_join(def_pass_eff, def_rush_eff, by = c('game_id', 'def_pos_team'))
off_total_efficiency <- left_join(off_total_efficiency, game_plays_pos, by = c('game_id','pos_team'))
def_total_efficiency <- left_join(def_total_efficiency, game_plays_def, by = c('game_id', 'def_pos_team'))
#################################################################################################################
#turn efficiency stats into per play stats
#################################################################################################################
off_total_efficiency <- off_total_efficiency %>%
  mutate(per_pass_success = pass_success / pos_team_plays_pass,
         per_rush_success = rush_success / pos_team_plays_run,
         per_pass_epa = pass_epa / pos_team_plays_pass,
         per_rush_epa = rush_epa / pos_team_plays_run,
         per_pass_wpa = pass_wpa / pos_team_plays_pass,
         per_rush_wpa = rush_wpa / pos_team_plays_run)
def_total_efficiency <- def_total_efficiency %>%
  mutate(per_pass_success = pass_success / def_pos_team_plays_pass,
         per_rush_success = rush_success / def_pos_team_plays_run,
         per_pass_epa = pass_epa / def_pos_team_plays_pass,
         per_rush_epa = rush_epa / def_pos_team_plays_run,
         per_pass_wpa = pass_wpa / def_pos_team_plays_pass,
         per_rush_wpa = rush_wpa / def_pos_team_plays_run)
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
   colnames(out)[3:7] <- paste(colnames(out)[3:7],x,dwn,sep = '_') 
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
#combine play data with efficiency dfs
##################################################################################################################
library(tidyverse)
off_total_efficiency <- list(off_total_efficiency,off_pass_eff_1,off_pass_eff_2,off_pass_eff_3,
                             off_rush_eff_1, off_rush_eff_2, off_rush_eff_3) %>% 
  reduce(left_join, by = c('game_id', 'pos_team'))
def_total_efficiency <- list(def_total_efficiency,def_pass_eff_1,def_pass_eff_2,def_pass_eff_3,
                             def_rush_eff_1, def_rush_eff_2, def_rush_eff_3) %>% 
  reduce(left_join, by = c('game_id', 'def_pos_team'))
##################################################################################################################
#touchdown points
##################################################################################################################
td_pts <- function(df,pos_def){
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
##################################################################################################################
#join pts to offensive efficiency
##################################################################################################################
off_total_efficiency$total_pts <- lookup(off_total_efficiency$game_id, off_total_pts$game_id, off_total_pts$total_pts)
off_total_efficiency_NA <- na.omit(off_total_efficiency) %>%
  ungroup()
##################################################################################################################
#linear model
##################################################################################################################
library(tidyverse)
library(tidymodels)
##################################################################################################################
#split data
##################################################################################################################
data_split_lm <- initial_split(off_total_efficiency, prop = 4/5)
train_lm <- training(data_split_lm)
test_lm <- testing(data_split_lm)
##################################################################################################################
#create recipes for different linear models
##################################################################################################################
lm_start <- recipe(total_pts~., data = train_lm) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_impute_mean()
##################################################################################################################
#select model types / select engine
##################################################################################################################
lm_mod <- linear_reg() %>%
  set_engine('glm')
##################################################################################################################
# set workflows
##################################################################################################################
off_start_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_start)
##################################################################################################################
#fit models
##################################################################################################################
off_start_fit <- off_start_wflow %>%
  fit(data = train_lm)
model_summary <- off_start_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
start_preds <- predict(off_start_fit, test_lm)
bind_preds <- bind_cols(start_preds, test_lm %>% select(total_pts))
rmse(total_pts, .pred, data = bind_preds)