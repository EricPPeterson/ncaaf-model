insetwd("/Users/ericp/OneDrive/Documents/GitHub/ncaaf-model")
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
td_pts <- function(df){
  out <- df %>%
    dplyr::group_by(game_id, home) %>%
    dplyr::summarise(td_pts = sum(touchdown) * 7)
  colnames(out)[2] <- 'pos_team'
  out2 <- df %>%
    dplyr::group_by(game_id, away) %>%
    dplyr::summarise(td_pts = sum(touchdown) * 7)
  colnames(out2)[2] <- 'pos_team'  
  out <- bind_rows(out,out2)
  return(out)
}

off_tds <- td_pts(pbp_clean)
def_tds <- td_pts(pbp_clean)
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
off_total_pts$def_pts <- lookup(off_total_pts$game_id, total_def_pts$game_id, total_def_pts$def_pts)
off_total_pts$def_pts <- ifelse(is.na(off_total_pts$def_pts) == TRUE, 0, off_total_pts$def_pts)
off_total_pts <- off_total_pts %>%
  dplyr::mutate(total_pts = td_pts + fgs_pts)
##################################################################################################################
#join pts to offensive efficiency
##################################################################################################################
library(mice)
off_total_efficiency$total_pts <- lookup(off_total_efficiency$game_id, off_total_pts$game_id, off_total_pts$total_pts)
off_total_efficiency <- off_total_efficiency %>% 
  dplyr::select(-c(3,8,19,20,25,26,31,32,37,38,43,44,49,50))
lapply(off_total_efficiency, function(x) { length(which(is.na(x)))})
temp_off_eff <- mice(off_total_efficiency, m = 5, maxit = 50, meth = 'rf', seed = 500)
summary(temp_off_eff)
off_total_efficiency <- complete(temp_off_eff,1)
##################################################################################################################
#linear model
##################################################################################################################
library(tidyverse)
library(tidymodels)
##################################################################################################################
#split data
##################################################################################################################
data_split_lm <- initial_split(off_total_efficiency, prop = 3/4)
train_lm <- training(data_split_lm)
test_lm <- testing(data_split_lm)
##################################################################################################################
#create recipes for different linear models
##################################################################################################################
lm_start <- recipe(total_pts~., data = train_lm) %>%
  update_role(game_id, pos_team, new_role = 'id_variable')
##################################################################################################################
#select model types / select engine
##################################################################################################################
lm_mod <- linear_reg(mode = 'regression', engine = 'lm')
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
rmse(.pred, total_pts, data = bind_preds)
##################################################################################################################
#collect significant variables
##################################################################################################################
sig_names_lm <- model_summary %>% 
  dplyr::filter(p.value < 0.05) 
sig_names_lm <- list(sig_names_lm$term[sig_names_lm$p.value < 0.05])
sig_names_lm <- sig_names_lm[[1]][-1]
new_cols <- c('game_id', 'pos_team', 'total_pts')
sig_names_lm <- append(sig_names_lm, new_cols)
off_tot_eff_sig <- off_total_efficiency_NA %>%
  dplyr::select(all_of(sig_names_lm))
##################################################################################################################
#visualizations
##################################################################################################################
ggplot(off_tot_eff_sig, aes(x = total_pts, y = pass_wpa)) + geom_point() + ggtitle('Pass WPA vs. total_pts')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = rush_epa)) + geom_point() + ggtitle('Rush EPA vs. total_pts')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = per_pass_epa)) + geom_point() + ggtitle('Per Pass EPA vs. total_pts')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = per_rush_epa)) + geom_point() + ggtitle('Per Rush EPA vs. total_pts')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = avg_ytg_log_pass_1)) + geom_point() + ggtitle('avg_ytg_log_pass first down') 
ggplot(off_tot_eff_sig, aes(x = total_pts, y = avg_ytg_log_pass_2)) + geom_point() + ggtitle('avg_ytg_log_pass 2nd down')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = total_epa_pass_3)) + geom_point() + ggtitle('total_epa_pass 3rd down')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = avg_ytg_log_rush_1)) + geom_point() + ggtitle('avg_ytg_log_rush 1st down')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = total_drives)) + geom_point() + ggtitle('Total Drives')
ggplot(off_tot_eff_sig, aes(x = total_pts, y = avg_drive_time)) + geom_point() + ggtitle('Average Drive Time')

ggplot(off_tot_eff_sig, aes(x = pass_wpa)) + geom_density() + ggtitle('Pass WPA')
ggplot(off_tot_eff_sig, aes(x = rush_epa)) + geom_density() + ggtitle('Rush EPA')
ggplot(off_tot_eff_sig, aes(x = per_pass_epa)) + geom_density() + ggtitle('per_pass_epa Histogram')
ggplot(off_tot_eff_sig, aes(x = per_rush_epa)) + geom_density() + ggtitle('per_rush_epa Histogram')
ggplot(off_tot_eff_sig, aes(x = avg_ytg_log_pass_1)) + geom_density() + ggtitle('avg_ytg_log_pass first down')
ggplot(off_tot_eff_sig, aes(x = avg_ytg_log_pass_2)) + geom_density() + ggtitle('avg_ytg_log_pass second down')
ggplot(off_tot_eff_sig, aes(x = total_epa_pass_3)) + geom_density() + ggtitle('total_epa_pass 3rd down')
ggplot(off_tot_eff_sig, aes(x = avg_ytg_log_rush_1)) + geom_density() + ggtitle('avg_ytg_log_rush 1st down')
ggplot(off_tot_eff_sig, aes(x = total_drives)) + geom_histogram() + ggtitle('total_drives')
ggplot(off_tot_eff_sig, aes(x = avg_drive_time)) + geom_density() + ggtitle('avg_drive_time')
#do total drives, avg_ytg_log 1st down need transformations
##################################################################################################################
#feature engineering / interactions / transformations
##################################################################################################################
lm_reduced <- recipe(total_pts~., data = off_tot_eff_sig) %>%
  update_role(game_id, pos_team, new_role = 'id_variable') %>%
  step_interact(terms = ~pass_wpa:avg_drive_time)
##################################################################################################################
#split data
##################################################################################################################
data_split_reduced <- initial_split(off_tot_eff_sig, prop = 3/4)
train_reduced <- training(data_split_reduced)
test_reduced <- testing(data_split_reduced)
##################################################################################################################
# set workflows
##################################################################################################################
off_reduced_wflow <- workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(lm_reduced)
##################################################################################################################
#fit models
##################################################################################################################
off_reduced_fit <- off_reduced_wflow %>%
  fit(data = train_lm)
model_reduced_summary <- off_reduced_fit %>%
  extract_fit_parsnip() %>%
  tidy()
##################################################################################################################
#make predictions
##################################################################################################################
reduced_preds <- predict(off_reduced_fit, test_reduced)
bind_reduced_preds <- bind_cols(reduced_preds, test_reduced %>% select(total_pts))
rmse(.pred, total_pts, data = bind_reduced_preds)
