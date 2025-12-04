rm(list = ls())
library(tidyverse)
library(rvest)
library(ggpattern)
library(shadowtext)
library(vistime)
library(ggthemes)
library(nflverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(ggimage)
library(gt)
library(gtExtras)
################################################################################
play_by_play <- load_pbp(seasons = 2025) %>% 
  clean_pbp() %>% 
  select(game_id,week,season,season_type,posteam,defteam,drive,play_id,qtr,down,ydstogo,
         goal_to_go,wp,play_type,play_type_nfl,desc,epa,success,yards_gained,
         pass,rush,pass_attempt,incomplete_pass,qb_scramble,rusher_player_id,
         penalty,penalty_team,penalty_yards,touchdown,field_goal_attempt,
         field_goal_result,extra_point_result,two_point_conv_result,
         safety,sack,interception,fumble,fumble_lost,fourth_down_failed,
         punt_attempt,punt_blocked)

pbp <- play_by_play %>% 
  merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3),
        by.x = 'posteam',by.y = 'team_abbr')

## Getting plays that should count towards play sequencing
plays <- pbp %>% 
  filter( #game_id == '2024_01_BAL_KC' &
    !(play_type %in% c('kickoff','no_play','field_goal','extra_point','punt','qb_kneel','qb_spike')) & 
      !is.na(posteam)
  ) %>%
  group_by(season,game_id,posteam) %>%
  mutate(drive_no = dense_rank(drive)) %>% 
  group_by(season,game_id,posteam,drive_no) %>% 
  mutate(play_no = dense_rank(play_id)) %>% 
  ungroup() 

## Now, we have to build out the data for the chart
chart_data <- plays %>% 
  ################################################################################
mutate(
  playType = case_when(
    rush == 1 ~ 'Run',
    play_type %in% c('punt','field_goal') ~ 'ST',
    pass == 1  ~ 'Pass',
    TRUE ~ 'No Play' ),
  logo = paste0('~/Extracurricular/NFL Data/Logos/',posteam,'.png')
) 
################################################################################
seq_epa <- chart_data %>%
  ##############################################################################
filter(pass == 1 | rush == 1) %>%
  select(game_id,week,season_type,posteam,defteam,qtr,down,drive_no, play_no, 
         play_type,playType,desc, epa, success, wp) %>% 
  arrange(game_id,posteam,drive_no,play_no) %>% 
  group_by(game_id,posteam,drive_no) %>% 
  mutate(t_last_play = lag(playType),
         t_next_play = lead(playType),
         t_last_down = lag(down),
         t_next_down = lead(down),
         seq_as_start = paste(playType,t_next_play,sep = '-'),
         seq_as_end = lag(seq_as_start)
  )
  ##############################################################################

s <- c('Pass-Pass','Run-Run','Pass-Run','Run-Pass')
all_seq <- c()
for (i in 1:4) {
  seq_data <- s[i]
  
  # First play in sequence
  Seq_1 <- seq_epa %>% 
    filter((seq_as_start == seq_data | seq_as_end == seq_data)) %>% 
    mutate(seq_group = seq_data)
  
  all_seq <- bind_rows(all_seq,Seq_1)
}

charting_data <- nflreadr::load_ftn_charting() %>% 
  select(-week, -season)

play_data <- load_pbp() %>%
  clean_pbp() %>% 
  merge(charting_data, by.x = c('game_id','play_id'), by.y = c('nflverse_game_id','nflverse_play_id'), all.x = TRUE) %>% 
  mutate(
    playType = case_when(
      rush == 1 ~ 'Run',
      play_type %in% c('punt','field_goal') ~ 'ST',
      pass == 1  ~ 'Pass',
      TRUE ~ 'No Play' )
  ) %>% 
  select(game_id,week,season,season_type,posteam,defteam,drive,play_id,qtr,down,ydstogo, time, playType,
         side_of_field, yardline_100, yrdln, pass_location, pass_length, run_location, run_gap, two_point_attempt,
         air_yards,goal_to_go,wp,play_type,play_type_nfl,desc,epa,success,yards_gained,
         pass,rush,pass_attempt,incomplete_pass,qb_scramble,rusher_player_id,
         penalty,penalty_team,penalty_yards,touchdown,field_goal_attempt,
         field_goal_result,extra_point_result,two_point_conv_result,
         safety,sack,interception,fumble,fumble_lost,fourth_down_failed,
         punt_attempt,punt_blocked, is_catchable_ball, is_screen_pass) %>%
  separate(yrdln, into = c('side','line'),sep = ' ',extra = 'merge') %>% 
  mutate(plus_side = if_else(side == posteam,paste0('+ ',line),paste0('- ',line)),
         yrdln = paste(side,line, by = '')) %>% 
  merge(teams_colors_logos %>% select(posteam = team_abbr, 
                                      posteam_logo_1 = team_logo_wikipedia, 
                                      posteam_logo_2 = team_logo_espn, 
                                      posteam_wordmark = team_wordmark), 
        by = 'posteam') %>% 
  merge(teams_colors_logos %>% select(defteam =team_abbr, 
                                      defteam_logo_1 = team_logo_wikipedia, 
                                      defteam_logo_2 = team_logo_espn, 
                                      defteam_wordmark = team_wordmark), 
        by = 'defteam') %>% 
  mutate(dist = case_when(
    two_point_attempt == 1 ~ '2PC',
    goal_to_go == 1 ~ 'GTG',
    ydstogo > 10 ~ '10+',
    ydstogo <= 10 & ydstogo >= 7 ~ '10-7',
    ydstogo <= 6 & ydstogo >= 4 ~ '6-4',
    ydstogo <= 3 & ydstogo >= 1 ~ '3-1',
    TRUE ~ 'Other'
    )
  )

# FIXED: Write to the correct path that matches your app.R
all_seq %>% write_csv('All Seq.csv')
seq_epa %>% write_csv('NFL pbp.csv')
play_data %>% write_csv('Full pbp.csv')
# ADDED: Error checking to ensure file was created successfully
if (file.exists("All Seq.csv")) {
  cat("✅ All Seq.csv created successfully at", as.character(Sys.time()), "\n")
  file_size <- file.size("All Seq.csv")
  cat("📊 File size:", round(file_size / 1024, 2), "KB\n")
  cat("📈 Rows in dataset:", nrow(all_seq), "\n")
} else {
  stop("❌ Failed to create All Seq.csv file")
}
if (file.exists("NFL pbp.csv")) {
  cat("✅ All Seq.csv created successfully at", as.character(Sys.time()), "\n")
  file_size <- file.size("NFL pbp.csv")
  cat("📊 File size:", round(file_size / 1024, 2), "KB\n")
  cat("📈 Rows in dataset:", nrow(seq_epa), "\n")
} else {
  stop("❌ Failed to create All Seq.csv file")
}
if (file.exists('Full pbp.csv')) {
  cat("✅ All Seq.csv created successfully at", as.character(Sys.time()), "\n")
  file_size <- file.size('Full pbp.csv')
  cat("📊 File size:", round(file_size / 1024, 2), "KB\n")
  cat("📈 Rows in dataset:", nrow(play_data), "\n")
} else {
  stop("❌ Failed to create Full pbp.csv file")
}
