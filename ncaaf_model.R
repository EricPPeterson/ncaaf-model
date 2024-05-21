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
                     success_rate = total_success / total_downs)
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
