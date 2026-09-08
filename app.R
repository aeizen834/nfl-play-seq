library(shiny)
library(shinyjs)
library(shinyWidgets)
library(scales)
library(bslib)
library(tidyverse)
library(nflverse)
library(gt)
library(gtExtras)
library(ggtext)
library(rsconnect)
library(DT)
library(shadowtext)


split_data_for_display <- function(data, pbp_data) {
  
  n <- nrow(data)
  h <- round(n/2, 0)
  data_left <- data[1:h, ]
  data_right <- data[(h+1):n, ]
  
  # Balance the tables
  if (nrow(data_left) > nrow(data_right)) {
    nc <- as.data.frame(
      matrix(NA, nrow = 1, ncol = ncol(data_right)), 
      stringsAsFactors = FALSE
    )
    names(nc) <- names(data_right)
    nc[1, 1:4] <- ""
    data_right <- rbind(data_right, nc)
  } else if (nrow(data_left) < nrow(data_right)) {
    nc <- as.data.frame(
      matrix(NA, nrow = 1, ncol = ncol(data_left)), 
      stringsAsFactors = FALSE
    )
    names(nc) <- names(data_left)
    nc[1, 1:4] <- ""
    data_left <- rbind(data_left, nc)
  }
  
  # Rename right columns
  names(data_right) <- paste0(names(data_right), "_2")
  
  return(cbind(data_left, data_right))
}

seq_table <- function(play_data, pbp_data){
  
  seq_chart <- play_data %>% 
    group_by(posteam,seq_group) %>% 
    reframe(epa_per_play = mean(epa),
            success_rate = mean(success),
            plays = n()) %>% 
    merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3, team_wordmark,team_logo_wikipedia),
          by.x = 'posteam',by.y = 'team_abbr') %>% 
    unique()
  
  whole_totals <- pbp_data %>% 
    group_by(posteam) %>% 
    mutate(total_epa = mean(epa),
           total_sr = mean(success),
           plays = n()) 
  
  
  # print(seq_chart)
  new_table <- c()
  tms <- seq_chart %>% pull(posteam) %>% unique()
  for (tm in tms) {
    primary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color) %>% unique() 
    secondary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color2) %>% unique() 
    tertiary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color3) %>% unique()
    wordmark <- seq_chart %>% filter(posteam == tm) %>% pull(team_wordmark) %>% unique()
    logo <- seq_chart %>% filter(posteam == tm) %>% pull(team_logo_wikipedia) %>% unique()
    
    
    # Get Pass-Pass Metrics
    SR <- whole_totals %>% 
      filter(posteam == tm) %>% 
      pull(total_sr) %>%
      unique()
    # percent(accuracy = 0.1)
    EPA <- whole_totals %>% 
      filter(posteam == tm) %>% 
      pull(total_epa) %>% 
      round(3) %>%
      unique()
    
    plays <- whole_totals %>% 
      filter(posteam == tm) %>% 
      pull(plays) %>% 
      unique()
    
    # Get Pass-Pass Metrics
    PP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    PP_plays <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(plays)
    
    # Get Pass-Run Metrics
    PR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    PR_plays <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(plays)
    
    # Get Run-Pass Metrics
    RP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    RP_plays <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(plays)
    
    # Get Run-Run Metrics
    RR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    RR_plays <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(plays)
    
    team_row <- data.frame(
      posteam = tm,
      primary, 
      secondary,
      tertiary,
      wordmark,
      # logo,
      EPA = if (length(EPA) == 0) 0 else EPA,
      SR = if (length(SR) == 0) 0 else SR,
      plays = if (length(plays) == 0) 0 else plays,
      PP_EPA = if (length(PP_EPA) == 0) 0 else PP_EPA,
      PP_SR = if (length(PP_SR) == 0) 0 else PP_SR,
      PP_plays = if (length(PP_plays) == 0) 0 else PP_plays,
      PR_EPA = if (length(PR_EPA) == 0) 0 else PR_EPA,
      PR_SR = if (length(PR_SR) == 0) 0 else PR_SR,
      PR_plays = if (length(PR_plays) == 0) 0 else PR_plays,
      RP_EPA = if (length(RP_EPA) == 0) 0 else RP_EPA,
      RP_SR = if (length(RP_SR) == 0) 0 else RP_SR,
      RP_plays = if (length(RP_plays) == 0) 0 else RP_plays,
      RR_EPA = if (length(RR_EPA) == 0) 0 else RR_EPA,
      RR_SR = if (length(RR_SR) == 0) 0 else RR_SR,
      RR_plays = if (length(RR_plays) == 0) 0 else RR_plays
    )
    
    new_table <- bind_rows(new_table, team_row)
  }
  
  return(new_table)
}

calculate_league_averages <- function(play_data, pbp_data) {
  lg_avg <- pbp_data %>%
    # group_by(playType) %>%
    reframe(EPA = mean(epa),
            SR = mean(success))
  
  seq_avg <- play_data %>%
    group_by(seq_group) %>%
    reframe(EPA = mean(epa),
            SR = mean(success))
  
  
  SR <- lg_avg %>% 
    pull(SR) %>%
    unique()
  
  EPA <- lg_avg %>% 
    pull(EPA) %>% 
    round(3) %>%
    unique()
  
  plays <- (pbp_data %>% nrow()/pbp_data %>% pull(posteam) %>% unique() %>% length()) %>%
    round()
  
  # Get Pass-Pass Metrics
  PP_SR <- seq_avg %>% 
    filter(seq_group == 'Pass-Pass') %>% 
    pull(SR) 
  
  PP_EPA <- seq_avg %>% 
    filter(seq_group == 'Pass-Pass') %>% 
    pull(EPA) %>% 
    round(3)
  
  PP_plays <- (play_data %>% filter(seq_group == 'Pass-Pass') %>% nrow()/
                 play_data %>% filter(seq_group == 'Pass-Pass') %>% pull(posteam) %>% unique() %>% length()) %>%
    round()
  
  # Get Pass-Run Metrics
  PR_SR <- seq_avg %>% 
    filter(seq_group == 'Pass-Run') %>% 
    pull(SR) 
  
  PR_EPA <- seq_avg %>% 
    filter(seq_group == 'Pass-Run') %>% 
    pull(EPA) %>% 
    round(3)
  
  PR_plays <- (play_data %>% filter(seq_group == 'Pass-Run') %>% nrow()/
                 play_data %>% filter(seq_group == 'Pass-Run') %>% pull(posteam) %>% unique() %>% length()) %>%
    round()
  
  # Get Run-Pass Metrics
  RP_SR <- seq_avg %>% 
    filter(seq_group == 'Run-Pass') %>% 
    pull(SR) 
  
  RP_EPA <- seq_avg %>% 
    filter(seq_group == 'Run-Pass') %>% 
    pull(EPA) %>% 
    round(3)
  
  RP_plays <- (play_data %>% filter(seq_group == 'Run-Pass') %>% nrow()/
                 play_data %>% filter(seq_group == 'Run-Pass') %>% pull(posteam) %>% unique() %>% length()) %>%
    round()
  
  # Get Run-Run Metrics
  RR_SR <- seq_avg %>% 
    filter(seq_group == 'Run-Run') %>% 
    pull(SR) 
  
  RR_EPA <- seq_avg %>% 
    filter(seq_group == 'Run-Run') %>% 
    pull(EPA) %>% 
    round(3)
  
  RR_plays <- (play_data %>% filter(seq_group == 'Run-Run') %>% nrow()/
                 play_data %>% filter(seq_group == 'Run-Run') %>% pull(posteam) %>% unique() %>% length()) %>%
    round()
  
  
  team_row <- data.frame(
    posteam = 'NFL',
    primary = NA, 
    secondary = NA,
    tertiary = NA,
    wordmark = "https://raw.githubusercontent.com/nflverse/nflfastR-data/master/NFL.png",
    # logo = NA,
    EPA = if (length(EPA) == 0) 0 else EPA,
    SR = if (length(SR) == 0) 0 else SR,
    plays = if (length(plays) == 0) 0 else plays,
    PP_EPA = if (length(PP_EPA) == 0) 0 else PP_EPA,
    PP_SR = if (length(PP_SR) == 0) 0 else PP_SR,
    PP_plays = if (length(PP_plays) == 0) 0 else PP_plays,
    PR_EPA = if (length(PR_EPA) == 0) 0 else PR_EPA,
    PR_SR = if (length(PR_SR) == 0) 0 else PR_SR,
    PR_plays = if (length(PR_plays) == 0) 0 else PR_plays,
    RP_EPA = if (length(RP_EPA) == 0) 0 else RP_EPA,
    RP_SR = if (length(RP_SR) == 0) 0 else RP_SR,
    RP_plays = if (length(RP_plays) == 0) 0 else RP_plays,
    RR_EPA = if (length(RR_EPA) == 0) 0 else RR_EPA,
    RR_SR = if (length(RR_SR) == 0) 0 else RR_SR,
    RR_plays = if (length(RR_plays) == 0) 0 else RR_plays,
    rank = 'AVG',
    posteam_2 = 'NFL',
    primary_2 = NA, 
    secondary_2 = NA,
    tertiary_2 = NA,
    rank_2 = 'AVG',
    wordmark_2 = "https://raw.githubusercontent.com/nflverse/nflfastR-data/master/NFL.png",
    # logo = NA,
    EPA_2 = if (length(EPA) == 0) 0 else EPA,
    SR_2 = if (length(SR) == 0) 0 else SR,
    plays_2 = if (length(plays) == 0) 0 else plays,
    PP_EPA_2 = if (length(PP_EPA) == 0) 0 else PP_EPA,
    PP_SR_2 = if (length(PP_SR) == 0) 0 else PP_SR,
    PP_plays_2 = if (length(PP_plays) == 0) 0 else PP_plays,
    PR_EPA_2 = if (length(PR_EPA) == 0) 0 else PR_EPA,
    PR_SR_2 = if (length(PR_SR) == 0) 0 else PR_SR,
    PR_plays_2 = if (length(PR_plays) == 0) 0 else PR_plays,
    RP_EPA_2 = if (length(RP_EPA) == 0) 0 else RP_EPA,
    RP_SR_2 = if (length(RP_SR) == 0) 0 else RP_SR,
    RP_plays_2 = if (length(RP_plays) == 0) 0 else RP_plays,
    RR_EPA_2 = if (length(RR_EPA) == 0) 0 else RR_EPA,
    RR_SR_2 = if (length(RR_SR) == 0) 0 else RR_SR,
    RR_plays_2 = if (length(RR_plays) == 0) 0 else RR_plays
  )
  
  return(team_row)
  
}

calculate_sequence_frequencies <- function(play_data, side = 'Off') {
  
  logos <- teams_colors_logos %>% 
    select(team_abbr,team_name,
           team_color, team_color2, team_color3, team_color4,
           team_logo_wikipedia, team_logo_espn, team_wordmark)
  
  if (side == 'Off') {
    # Calculate total plays by team and sequence type
    sequence_counts <- play_data %>%
      filter(!is.na(t_last_play)) %>%  # Only plays that have a next play
      group_by(posteam, playType, t_last_play) %>%
      summarise(sequence_count = n(),
                epa_play = mean(epa),
                SR = mean(success),
                .groups = 'drop') %>%
      mutate(sequence = paste(playType, t_last_play, sep = "-"))
    
    # Calculate total plays by team and first play type for percentages
    total_by_first_play <- play_data %>%
      # filter(!is.na(t_next_play)) %>%
      group_by(posteam, playType) %>%
      summarise(total_first_plays = n(),
                first_epa_play = mean(epa),
                first_SR = mean(success),
                .groups = 'drop') %>% 
      mutate(play_freq = total_first_plays/sum(total_first_plays))
    
    first_LA <- play_data %>%
      group_by(playType) %>%
      summarise(first_LA_epa_play = mean(epa),
                first_LA_SR = mean(success),
                .groups = 'drop')
    
    second_LA <- play_data %>%
      group_by(playType) %>%
      summarise(second_LA_epa_play = mean(epa),
                second_LA_SR = mean(success),
                .groups = 'drop')
    
    # Join and calculate frequencies
    frequency_data <- sequence_counts %>%
      merge(total_by_first_play,
            by.x = c('posteam','t_last_play'),
            by.y = c('posteam','playType')) %>%
      merge(first_LA, by.x = c("t_last_play"), by.y = c("playType")) %>%
      left_join(second_LA, by = c("playType")) %>%
      group_by(posteam,t_last_play) %>% 
      mutate(frequency = sequence_count / sum(sequence_count)) %>% 
      group_by(sequence) %>%
      mutate(first_epa_rk = dense_rank(-first_epa_play),
             first_SR_rk = dense_rank(-first_SR),
             second_epa_rk = dense_rank(-epa_play),
             second_SR_rk = dense_rank(-SR)) %>% 
      merge(logos, by.x = 'posteam',by.y = 'team_abbr')
  } else {
    # Calculate total plays by team and sequence type
    sequence_counts <- play_data %>%
      filter(!is.na(t_last_play)) %>%  # Only plays that have a next play
      group_by(defteam, playType, t_last_play) %>%
      summarise(sequence_count = n(),
                epa_play = mean(epa),
                SR = mean(success),
                .groups = 'drop') %>%
      mutate(sequence = paste(playType, t_last_play, sep = "-"))
    
    # Calculate total plays by team and first play type for percentages
    total_by_first_play <- play_data %>%
      # filter(!is.na(t_next_play)) %>%
      group_by(defteam, playType) %>%
      summarise(total_first_plays = n(),
                first_epa_play = mean(epa),
                first_SR = mean(success),
                .groups = 'drop') %>% 
      mutate(play_freq = total_first_plays/sum(total_first_plays))
    
    first_LA <- play_data %>%
      group_by(playType) %>%
      summarise(first_LA_epa_play = mean(epa),
                first_LA_SR = mean(success),
                .groups = 'drop')
    
    second_LA <- play_data %>%
      group_by(playType) %>%
      summarise(second_LA_epa_play = mean(epa),
                second_LA_SR = mean(success),
                .groups = 'drop')
    
    # Join and calculate frequencies
    frequency_data <- sequence_counts %>%
      merge(total_by_first_play,
            by.x = c('defteam','t_last_play'),
            by.y = c('defteam','playType')) %>%
      merge(first_LA, by.x = c("t_last_play"), by.y = c("playType")) %>%
      left_join(second_LA, by = c("playType")) %>%
      group_by(defteam,t_last_play) %>% 
      mutate(frequency = sequence_count / sum(sequence_count)) %>%
      group_by(sequence) %>%
      mutate(first_epa_rk = dense_rank(first_epa_play),
             first_SR_rk = dense_rank(first_SR),
             second_epa_rk = dense_rank(epa_play),
             second_SR_rk = dense_rank(SR)) %>% 
      merge(logos, by.x = 'defteam',by.y = 'team_abbr')
  }
  
  return(frequency_data)
}

create_sequence_matrix <- function(data, subtitle, team = 'DET', side = 'Off', color = 'EPA/Play') {
  
  team_col <- if (side == 'Off') 'posteam' else 'defteam'
  n_teams <- data %>% pull(.data[[team_col]]) %>% n_distinct()
  
  freq_data <- data %>%
    filter(.data[[team_col]] == team)
  
  team_name <- freq_data %>% pull(team_name) %>% unique()
  primary <- freq_data %>% pull(team_color) %>% unique()
  secondary <- freq_data %>% pull(team_color2) %>% unique()
  third <- freq_data %>% pull(team_color3) %>% unique()
  
  mid_color <- "#DDDDDD"
  low_color <- if (side == 'Off') '#4B0092' else '#1AFF1A'
  high_color <- if (side == 'Off') '#1AFF1A' else '#4B0092'
  
  tile_data <- freq_data %>%
    mutate(
      first_play = factor(t_last_play, levels = c('Pass','Run')),
      second_play = factor(playType, levels = c('Pass','Run')),
      diff = if (color == 'EPA/Play') epa_play - second_LA_epa_play else SR - second_LA_SR,
      tile_label = paste0(playType,' after ',t_last_play, "\n",
                          round(frequency * 100), "% of plays",
                          "\nEPA: ", round(epa_play, 2), " (", second_epa_rk, "/", n_teams, ") | SR: ", round(SR * 100), "% (", second_SR_rk, "/", n_teams, ")")
    )
  
  p <- ggplot(tile_data, aes(x = first_play, y = second_play, fill = diff)) +
    annotate("rect", xmin = 0.4, xmax = 2.6, ymin = 0.4, ymax = 2.6, fill = third, alpha = .5) +
    geom_tile(color = 'white', linewidth = 2) +
    geom_text(aes(label = tile_label), color = 'black', fontface = 'bold', size = 6.5, lineheight = 1.1) +
    scale_fill_gradient2(low = low_color, mid = mid_color, high = high_color, midpoint = 0,
                         name = paste0(color, " vs league avg")) +
    scale_x_discrete(position = 'top', name = "2nd play") +
    scale_y_discrete(name = "1st play") +
    labs(
      title = paste0(team_name, if (side == 'Off') ' Offensive' else ' Defensive', ' Sequence Efficiency'),
      subtitle = subtitle,
      caption = paste0("**Analysis:** @AriEizen | **Data:** nflfastR | **Fill:** ", color, " vs league average, purple (below) to green (above). Ranks are league rank for that exact sequence (1 = best).")
    ) +
    theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = primary),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.caption = element_markdown(size = 14, hjust = 0),
      axis.title = element_text(size = 14, face = 'bold'),
      axis.text = element_text(size = 13, face = 'bold'),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = secondary, fill = NA, linewidth = 2.5)
    )
  
  return(p)
}


load_app_data <- function(){
  list(all_seq = read_csv('All Seq.csv', show_col_types = F),
       pbp = read_csv('NFL pbp.csv', show_col_types = F) %>% 
         mutate(Run = if_else(playType == 'Run',1,0)),
       full_data = read_csv('Full pbp.csv', show_col_types = F)
  )
}

apply_theme <- function(){
  theme(
    plot.background = element_rect(fill = "#468944", color = NA),
    panel.background = element_rect(fill = "#468944", color = NA),
    panel.border = element_rect(colour = 'white', fill = NA, linewidth = 2.5),
    axis.text = element_text(color = 'white', face = 'bold', size = 14),
    axis.title.y = element_text(color = 'white', face = 'bold', size = 16, angle = 90),
    axis.title.x = element_text(color = 'white', face = 'bold', size = 16),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = 'white'),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = 'white'),
    plot.caption = element_markdown(face = "bold", size = 14, color = 'white', hjust = 0),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
}

# Guards against pickerInput's "Deselect All" leaving a filter with zero
# selections, which would otherwise filter every row out. Falls back to
# `full` (the complete set of choices) whenever nothing is selected.
or_all <- function(x, full) {
  if (is.null(x) || length(x) == 0) full else x
}

# The full set of personnel groupings, used as both the picker choices
# and the default/fallback selection now that 'All' is no longer a
# distinct choice (a fully-selected picker means the same thing).
# Covers every standard RB-TE personnel grouping (WR count is implied as
# 5 - RB - TE); 'Other' catches truly exotic/rare packages (e.g. 3-RB
# goal-line sets) or any unlabeled rows.
personnel_choices <- c('00p','01p','10p','11p','12p','13p','20p','21p','22p','23p','Other')
# Define UI for application that draws a histogram
icon <- div(
  style = "position: absolute; top: 10px; right: 20px; 
           background-color: #E63946; color: white; 
           padding: 8px 15px; border-radius: 5px; 
           font-weight: bold; font-size: 12px; 
           box-shadow: 2px 2px 5px rgba(0,0,0,0.3);",
  "By: @AriEizen | Data: nflfastR/Wikipedia | Inspo: @reinhardNFL/@csv_enjoyer"
)

ui <- navbarPage(
  title = "NFL Play Calling Trends & Insights",
  theme = bs_theme(
    bg = "#F8F9FA",           # Off-white background
    fg = "#000000",           # Black text
    primary = "#E63946",      # Bold red accents
    secondary = "#457B9D",    # Blue highlights
    base_font = "Oswald"      # Sports-like font
  ),
  # Shown above every tab: how current the data is overall, and how far
  # personnel charting has caught up (it tends to lag the rest of the pbp
  # data during the season).
  header = uiOutput('data_status'),
  useShinyjs(),
  ############################ HTML and CSS ############################
  tags$head(
    tags$style(HTML("
    body {
      background-color: #F8F9FA !important;
      color: #000000 !important;
    }

    /* Change Navbar Background & Text Color */
    .navbar {
      background-color: #EAEAEA !important;  /* Light Grey Background */
      border-bottom: 2px solid #CCCCCC; /* Optional: Adds subtle border */
    }

    .navbar-nav > li > a {
      font-size: 18px !important;
      font-weight: bold !important;
      padding: 15px 20px !important;
      color: #000000 !important;
    }

    .navbar-brand {
      font-size: 24px !important;
      font-weight: bold !important;
      color: #000000 !important;
    }

    .tab-content {
      background-color: #FFFFFF !important;
      padding: 20px;
      border-radius: 10px;
    }

    h1, h4 {
      font-weight: bold !important;
      color: #000000 !important;
    }

    .custom-container {
      background-color: #EAEAEA;
      padding: 20px;
      border-radius: 10px;
      margin-bottom: 20px;
    }
    
     #s label {
      display: inline-block;
      width: 30%;
      margin-bottom: 5px;
    }
  "))
    #############################################################################
    # TAB #1: Main Dashboard
    #############################################################################
  ),
  tabPanel("Main Dashboard",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(2,
                               sliderInput("week", "Week:", 
                                           min = 1, max = 22, value = c(1,18))
                        ),
                        column(2,
                               numericRangeInput("wp", "Win %:", 
                                                 min = 0, max = 100, value = c(5,95))
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Down:", style = "font-size: 14px;"),
                               pickerInput('down',
                                           '',# "Down:", 
                                           choices = 1:4, 
                                           selected = 1:4, 
                                           multiple = T,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Down:")
                                           # options = pickerOptions(
                                           #   actionsBox = TRUE,      # Adds 'Select All' and 'Deselect All' buttons
                                           #   size = 4,
                                           #   
                                           # )
                               )
                               # )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Quarter:", style = "font-size: 14px;"),
                               pickerInput('qtr',
                                           '' ,#"Quarter:", 
                                           choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                           selected = 1:5,
                                           multiple = T,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Quarter:")
                                           # options = pickerOptions(
                                           #   actionsBox = TRUE,      # Adds 'Select All' and 'Deselect All' buttons
                                           #   size = 5,
                                           #   
                                           # ))
                               )
                        ),
                        column(1,
                               pickerInput('season',
                                           '',# "Season:",
                                           choices = c(2023,2024,2025),
                                           selected = most_recent_season(), #,c(2023,2024,2025),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Season:")
                               )
                        ),
                        column(1,
                               pickerInput('order','',# 'Sort By:',
                                           choices = c('Overall SR' = 'SR', 
                                                       'Overall EPA' = 'EPA',
                                                       'Pass-Pass SR' = 'PP_SR', 
                                                       'Pass-Pass EPA' = 'PP_EPA',
                                                       'Pass-Run SR' = 'PR_SR', 
                                                       'Pass-Run EPA' = 'PR_EPA',
                                                       'Run-Pass SR' = 'RP_SR', 
                                                       'Run-Pass EPA' = 'RP_EPA',
                                                       'Run-Run SR' = 'RR_SR', 
                                                       'Run-Run EPA' = 'RR_EPA'),
                                           selected = 'SR',
                                           options = list(`selected-text-format`= "static",
                                                          title = "Sort By:"))
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        pickerInput('fp','', #First Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "1st Play Personnel:")
                                        )
                                 ),
                                 column(6,
                                        pickerInput('sp','', #Second Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "2nd Play Personnel:")
                                        )
                                 )
                               )
                        ),
                        # column(1,
                        #         # actionButton('toggle_adv1',
                        #         #              'More Filters',
                        #         #              class = "btn-primary",
                        #         #              # icon = icon('filter'),
                        #         #              style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;"
                        #         #              )
                        #        actionButton('toggle_adv1', 'More Filters',
                        #                     class = "btn-outline-secondary",
                        #                     icon = icon('chevron-down'),
                        #                     style = "width: 100%; height: 38px; margin-top: 25px; font-size: 14px; font-weight: bold;")
                        #        ),
                        
                        column(1,
                               actionButton("apply_filters", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      ),
                      # shinyjs::hidden(
                      #   div(id = "adv_filters_1",
                      #       fluidRow(
                      #         column(2,
                      #                selectInput('fp','First Play Personnel',
                      #                            choices = personnel_choices)
                      #                ),
                      #         column(2,
                      #                selectInput('sp','Second Play Personnel',
                      #                            choices = personnel_choices)
                      #         )
                      #       )
                      #     )
                      #   )
                    )
             )
           ),
           # Show a plot of the generated distribution
           fluidRow(
             # DTOutput("raw_data")
             column(12,gt_output('overview'))
           )
  ),
  #############################################################################
  # TAB #2: Offensive Play Sequencing Analysis
  #############################################################################
  tabPanel("Offensive Play Sequencing Analysis",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(1,
                               selectInput('tm', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        sliderInput("week_2", "Week:", 
                                                    min = 1, max = 22, value = c(1,18))
                                 ),
                                 column(6,
                                        numericRangeInput("wp_2", "Win %:", 
                                                          min = 0, max = 100, value = c(5,95))
                                 )
                               )       
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Down:", style = "font-size: 14px;"),
                               pickerInput('down_2',
                                           '',# "Down:", 
                                           choices = 1:4, 
                                           selected = 1:4, 
                                           multiple = T,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Down:")
                                           # options = pickerOptions(
                                           #   actionsBox = TRUE,      # Adds 'Select All' and 'Deselect All' buttons
                                           #   size = 4,
                                           #   
                                           # )
                               )
                               # )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Quarter:", style = "font-size: 14px;"),
                               pickerInput('qtr_2',
                                           '' ,#"Quarter:", 
                                           choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                           selected = 1:5,
                                           multiple = T,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Quarter:")
                                           # options = pickerOptions(
                                           #   actionsBox = TRUE,      # Adds 'Select All' and 'Deselect All' buttons
                                           #   size = 5,
                                           #   
                                           # ))
                               )
                        ),
                        # column(1,
                        #        actionButton('toggle_adv2',
                        #                     'More Filters',
                        #                     class = "btn-primary",
                        #                     # icon = icon('filter'),
                        #                     style = "width: 100%; height: 38px; margin-top: 25px; font-size: 12px; font-weight: bold;"
                        #        )
                        # ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        pickerInput('fp2','', #First Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "1st Play Personnel:")
                                        )
                                 ),
                                 column(6,
                                        pickerInput('sp2','', #Second Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "2nd Play Personnel:")
                                        )
                                 )
                               )
                        ),
                        column(1,
                               pickerInput('season_2',
                                           '',# "Season:",
                                           choices = c(2023,2024,2025),
                                           selected = most_recent_season(), #,c(2023,2024,2025),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Season:")
                               )
                        ),
                        column(1,
                               radioButtons('epa','Rank:', 
                                            choices = c('EPA/Play','Success Rate'),
                                            selected = 'EPA/Play')
                               
                        ),
                        column(1,
                               actionButton("apply_filters_2", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      ),
                      # shinyjs::hidden(
                      #   div(id = "adv_filters_2",
                      #       fluidRow(
                      #         column(2,
                      #                selectInput('fp2','First Play Personnel',
                      #                            choices = personnel_choices)
                      #         ),
                      #         column(2,
                      #                selectInput('sp2','Second Play Personnel',
                      #                            choices = personnel_choices)
                      #         )
                      #       )
                      #   )
                      # )
                    )
             )
           ),
           fluidRow(
             column(12, plotOutput('tree', height = "85vh"
             )
             )
           )
  ),
  #############################################################################
  # TAB #3: Defensive Play Sequencing Analysis
  #############################################################################
  tabPanel("Defensive Play Sequencing Analysis",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(1,
                               selectInput('t', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        sliderInput("week_3", "Week:", 
                                                    min = 1, max = 22, value = c(1,18))
                                 ),
                                 column(6,
                                        numericRangeInput("wp_3", "Win %:", 
                                                          min = 0, max = 100, value = c(5,95))
                                 )
                               )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Down:", style = "font-size: 14px;"),
                               pickerInput('down_3',
                                           '',#"Down:",
                                           # NULL, 
                                           choices = 1:4, 
                                           selected = 1:4,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Down:"))
                               # )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Quarter:", style = "font-size: 14px;"),
                               pickerInput('qtr_3',
                                           '', # "Quarter:",
                                           # NULL, 
                                           choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                           selected = 1:5,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Quarter:"))
                               # )
                        ),
                        # column(1,
                        #        actionButton('toggle_adv3',
                        #                     'More Filters',
                        #                     class = "btn-primary",
                        #                     # icon = icon('filter'),
                        #                     style = "width: 100%; height: 38px; margin-top: 25px; font-size: 12px; font-weight: bold;"
                        #        )
                        # ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        pickerInput('fp3','', #First Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "1st Play Personnel:")
                                        )
                                 ),
                                 column(6,
                                        pickerInput('sp3','', #Second Play Personnel',
                                                    choices = personnel_choices,
                                                    multiple = TRUE,
                                                    selected = personnel_choices,
                                                    options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                                   title = "2nd Play Personnel:")
                                        )
                                 )
                               )
                        ),
                        column(1,
                               pickerInput('season_3',
                                           '',# "Season:",
                                           choices = c(2023,2024,2025),
                                           selected = most_recent_season(), #,c(2023,2024,2025),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Season:")
                               )
                        ),
                        column(1,
                               radioButtons('epa_2','Rank:', 
                                            choices = c('EPA/Play','Success Rate'),
                                            selected = 'EPA/Play')
                               
                        ),
                        column(1,
                               actionButton("apply_filters_3", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      ),
                      # shinyjs::hidden(
                      #   div(id = "adv_filters_3",
                      #       fluidRow(
                      #         column(2,
                      #                selectInput('fp3','First Play Personnel',
                      #                            choices = personnel_choices)
                      #         ),
                      #         column(2,
                      #                selectInput('sp3','Second Play Personnel',
                      #                            choices = personnel_choices)
                      #         )
                      #       )
                      #   )
                      # )
                    )
             )
           ),
           fluidRow(
             column(12, plotOutput('def_tend', height = "85vh"))
           )
  ),
  #############################################################################
  # TAB #4: Down and Distance Tendencies
  #############################################################################
  tabPanel("Offensive Play Calling Tendencies",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(1,
                               selectInput('t', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        sliderInput("week_4", "Week:", 
                                                    min = 1, max = 22, value = c(1,18))
                                 ),
                                 column(6,
                                        numericRangeInput("wp_4", "Win %:", 
                                                          min = 0, max = 100, value = c(5,95))
                                 )
                               )       
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Down:", style = "font-size: 14px;"),
                               pickerInput('down_4',
                                           '',# "Down:",
                                           # NULL, 
                                           choices = 1:4,
                                           selected = 1:4,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Down:"))
                               # )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               #     strong("Distance To Go:", style = "font-size: 14px;"),
                               pickerInput('dist',
                                           '',# "Distance:", 
                                           choices = c('10+','10-7','6-4','3-1','GTG'), 
                                           selected = c('10+','10-7','6-4','3-1','GTG'),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Distance:")
                               )
                               # )
                        ),
                        
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Quarter:", style = "font-size: 14px;"),
                               pickerInput('qtr_4',
                                           '',# "Quarter:",
                                           # NULL, 
                                           choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                           selected = 1:5,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Quarter:"))
                               # )
                        ),
                        column(1,
                               pickerInput('personnel',
                                           '', #Personnel',
                                           choices = personnel_choices,
                                           multiple = TRUE,
                                           selected = personnel_choices,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Personnel:"))
                        ),
                        column(1,
                               pickerInput('season_4',
                                           '',# "Season:",
                                           choices = c(2023,2024,2025),
                                           selected = most_recent_season(), #,c(2023,2024,2025),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Season:")
                               )
                        ),
                        column(1,
                               radioButtons('tile','Tile Color:',
                                            choices = c('EPA/Play','Success Rate','Frequency'),
                                            selected = 'EPA/Play')
                        ),
                        column(1,
                               actionButton("apply_filters_4", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      )
                    )
             )
           ),
           fluidRow(
             column(12, uiOutput('kpi_4'))
           ),
           fluidRow(column(6,
                           # div(style = "display: flex; flex-direction: column; height: 100%;",
                           plotOutput('pass_chart'#, height = "50%"
                           )
                           # )
           ),
           column(6,
                  plotOutput('run_chart'#, height = "50%"
                  ))),
           fluidRow(
             column(12,gt_output('pbp_table')))
  ),
  #############################################################################
  # TAB #5: Defensive Down and Distance Tendencies
  #############################################################################
  tabPanel("Defensive Play Calling Tendencies",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(1,
                               selectInput('t_2', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        sliderInput("week_5", "Week:", 
                                                    min = 1, max = 22, value = c(1,18))
                                 ),
                                 column(6,
                                        numericRangeInput("wp_5", "Win %:", 
                                                          min = 0, max = 100, value = c(5,95))
                                 )
                               )       
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Down:", style = "font-size: 14px;"),
                               pickerInput('down_5',
                                           '',# "Down:",
                                           # NULL, 
                                           choices = 1:4,
                                           selected = 1:4,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Down:")
                               )
                               # )
                        ),
                        column(1,
                               # div(style = "margin-top: 5px;",
                               #     strong("Distance To Go:", style = "font-size: 14px;"),
                               pickerInput('dist_2',
                                           '',# "Distance:", 
                                           choices = c('10+','10-7','6-4','3-1','GTG'), 
                                           selected = c('10+','10-7','6-4','3-1','GTG'),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Distance:")
                               )
                               # )
                        ),
                        
                        column(1,
                               # div(style = "margin-top: 5px;",
                               # strong("Quarter:", style = "font-size: 14px;"),
                               pickerInput('qtr_5',
                                           '',# "Quarter:", NULL, 
                                           choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                           selected = 1:5,
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Quarter:"))
                               # )
                        ),
                        column(1,
                               pickerInput('personnel_2',
                                           '', #Personnel',
                                           choices = personnel_choices,
                                           multiple = TRUE,
                                           selected = personnel_choices,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Personnel:"))
                        ),
                        column(1,
                               pickerInput('season_5',
                                           '',# "Season:",
                                           choices = c(2023,2024,2025),
                                           selected = most_recent_season(), #,c(2023,2024,2025),
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE, `selected-text-format`= "static",
                                                          title = "Season:")
                               )
                        ),
                        column(1,
                               radioButtons('tile_2','Tile Color:',
                                            choices = c('EPA/Play','Success Rate','Frequency'),
                                            selected = 'EPA/Play')
                        ),
                        column(1,
                               actionButton("apply_filters_5", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      )
                    )
             )
           ),
           fluidRow(
             column(12, uiOutput('kpi_5'))
           ),
           fluidRow(column(6,
                           # div(style = "display: flex; flex-direction: column; height: 100%;",
                           plotOutput('pass_chart_def'#, height = "50%"
                           )
                           # )
           ),
           column(6,
                  plotOutput('run_chart_def'#, height = "50%"
                  ))),
           fluidRow(
             column(12,gt_output('pbp_table_def')))
  )
)
#############################################################################

server <- function(input, output) {
  
  app_data <- load_app_data()
  
  # Data freshness banner: compares the most recent week/season present in
  # the overall play-by-play data against the most recent week/season for
  # which personnel groupings (offense_p) are actually populated. Personnel
  # charting can lag a week or more behind the rest of the pbp data during
  # the season, so this makes that gap visible rather than silent.
  output$data_status <- renderUI({
    full <- app_data$full_data
    
    latest_all <- full %>% 
      filter(!is.na(week), !is.na(season)) %>% 
      arrange(desc(season), desc(week)) %>% 
      slice(1)
    all_str <- if (nrow(latest_all) > 0) {
      paste0("Week ", latest_all$week[1], ", ", latest_all$season[1])
    } else "Unknown"
    
    has_personnel <- full %>% filter(!is.na(offense_p) & offense_p != '')
    latest_personnel <- has_personnel %>% 
      filter(!is.na(week), !is.na(season)) %>% 
      arrange(desc(season), desc(week)) %>% 
      slice(1)
    personnel_str <- if (nrow(latest_personnel) > 0) {
      paste0("Week ", latest_personnel$week[1], ", ", latest_personnel$season[1])
    } else "Unknown"
    
    lag_note <- if (nrow(latest_all) > 0 && nrow(latest_personnel) > 0 &&
                    !(latest_all$season[1] == latest_personnel$season[1] &&
                      latest_all$week[1] == latest_personnel$week[1])) {
      " ⚠ Personnel data is behind the rest of the data"
    } else ""
    
    div(
      style = "background-color: #2C2C2C; color: #FFFFFF; padding: 6px 20px;
               font-size: 12px; font-weight: bold; text-align: right;
               border-bottom: 2px solid #E63946;",
      paste0("Data through: ", all_str,
             "  |  Personnel data through: ", personnel_str, lag_note,
             "  |  By: @AriEizen | Data: nflfastR/Wikipedia | Inspo: @reinhardNFL")
    )
  })
  
  # Action for Tab 1
  applied_overview <- reactiveValues(
    week = c(app_data$all_seq %>% pull(week) %>% min(na.rm = TRUE),
             if_else(app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE) > 18,
                     18,
                     app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE))),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5,
    season = most_recent_season(),
    fp = personnel_choices,
    sp = personnel_choices,
    # bp = 'All'
  )
  
  observeEvent(input$apply_filters, {
    applied_overview$week <- input$week
    applied_overview$wp <- input$wp
    applied_overview$down <- or_all(input$down, 1:4)
    applied_overview$qtr <- or_all(input$qtr, 1:5)
    applied_overview$season <- or_all(input$season, c(2023,2024,2025))
    applied_overview$fp <- or_all(input$fp, personnel_choices)
    applied_overview$sp <- or_all(input$sp, personnel_choices)
  })
  
  output$overview <- render_gt({
    t <- app_data$all_seq %>% 
      filter(between(week,as.numeric(min(applied_overview$week)),as.numeric(max(applied_overview$week))),
             between(wp,min(applied_overview$wp)/100,max(applied_overview$wp)/100),
             down %in% applied_overview$down,
             qtr %in% applied_overview$qtr,
             season %in% applied_overview$season)
    
    # Filters for Formation
    t <- t %>%
      mutate(
        first_play_p  = if_else(seq_as_start == seq_group, offense_p, t_last_p),
        second_play_p = if_else(seq_as_start == seq_group, t_next_p, offense_p)
      ) %>%
      filter(
        first_play_p %in% applied_overview$fp,
        second_play_p %in% applied_overview$sp
      )
    
    pbp <- app_data$pbp %>% 
      filter(between(week,as.numeric(min(applied_overview$week)),as.numeric(max(applied_overview$week))),
             between(wp,min(applied_overview$wp)/100,max(applied_overview$wp)/100),
             down %in% applied_overview$down,
             qtr %in% applied_overview$qtr,
             season %in% applied_overview$season,
             (offense_p %in% applied_overview$fp | offense_p %in% applied_overview$sp) )
    
    lg_avg <- calculate_league_averages(t,pbp)
    # print(head(app_data$all_seq))
    # print(head(app_data$pbp))
    play_table <- seq_table(t, pbp) %>% 
      # select(-logo) %>% 
      arrange(desc(.data[[input$order]])) %>%
      unique() %>% 
      mutate(rank = as.character(row_number()))
    
    table_split <- split_data_for_display(play_table) %>% 
      bind_rows(lg_avg) %>% 
      relocate(rank_2, .before = wordmark_2) #%>% 
    # select(-PP_plays,-PR_plays,-RR_plays,-RP_plays,
    #        -PP_plays_2,-PR_plays_2,-RR_plays_2,-RP_plays_2)
    
    tab_subtitle <- paste0(paste(sort(applied_overview$season), collapse = ", "), " Season(s) • Weeks ", min(pbp$week), "-", max(pbp$week), 
                           " • Win Probability ", min(applied_overview$wp), "%-", max(applied_overview$wp), "%",
                           if(length(applied_overview$down) < 4) paste0(" • Downs: ", paste(applied_overview$down, collapse=", ")) else "",
                           if(length(applied_overview$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview$qtr), collapse=", ")) else "",
                           if(length(applied_overview$fp) < length(personnel_choices)) paste0(" • 1st Play Personnel: ", paste(applied_overview$fp, collapse = ', ')) else "",
                           if(length(applied_overview$sp) < length(personnel_choices)) paste0(" • 2nd Play Personnel: ", paste(applied_overview$sp, collapse = ', ')) else "")
    
    # Option 1:
    # low_color = "#8E44AD"
    # high_color = "#27AE60"
    # Option 2:
    # low_color = '#3B4CC0'
    # high_color = '#B40426'
    mid_color = "#DDDDDD"
    # Option 3:
    low_color = '#4B0092'
    high_color = '#1AFF1A'
    
    table_split %>% 
      gt() %>% 
      # Move rank columns to start
      cols_move_to_start(rank) %>% 
      # Hide color and team abbreviation columns
      cols_hide(c(primary, secondary, tertiary, posteam,
                  primary_2, secondary_2, tertiary_2, posteam_2,
                  PP_plays,PR_plays,RR_plays,RP_plays,
                  PP_plays_2,PR_plays_2,RR_plays_2,RP_plays_2)) %>% 
      
      # MAIN STYLING - Background and text colors
      tab_style(
        style = list(
          cell_fill(color = "#FFFFFF"),  # White background for cells
          cell_text(color = "#000000", size = px(12))  # Black text
        ),
        locations = cells_body(columns = everything())
      ) %>%
      
      # Header styling
      tab_style(
        style = list(
          # cell_fill(color = "#E63946"),  # Red background for headers
          cell_text(color = "#000000", weight = "bold", size = px(13))  # White bold text
        ),
        locations = cells_column_labels()
      ) %>%
      
      # Spanner styling
      tab_style(
        style = list(
          cell_fill(color = "#457B9D"),  # Blue background for spanners
          cell_text(color = "#FFFFFF", weight = "bold", size = px(14))  # White bold text
        ),
        locations = cells_column_spanners()
      ) %>%
      
      # Alternating row colors for better readability
      tab_style(
        style = cell_fill(color = "#F8F9FA"),  # Light gray for alternating rows
        locations = cells_body(rows = seq(2, nrow(table_split), 2))
      ) %>%
      
      ## LEFT COLUMN SPANNERS
      tab_spanner('Overall Stats', columns = c(SR, EPA, plays)) %>%
      tab_spanner('Pass-Pass', columns = c(PP_SR, PP_EPA, PP_plays)) %>%
      tab_spanner('Pass-Run', columns = c(PR_SR, PR_EPA, PR_plays)) %>%
      tab_spanner('Run-Pass', columns = c(RP_SR, RP_EPA, RP_plays)) %>%
      tab_spanner('Run-Run', columns = c(RR_SR, RR_EPA, RR_plays)) %>%
      
      ## RIGHT COLUMN SPANNERS
      tab_spanner('Overall Stats ', columns = c(SR_2, EPA_2, plays_2)) %>%
      tab_spanner('Pass-Pass ', columns = c(PP_SR_2, PP_EPA_2, PP_plays_2)) %>%
      tab_spanner('Pass-Run ', columns = c(PR_SR_2, PR_EPA_2, PR_plays_2)) %>%
      tab_spanner('Run-Pass ', columns = c(RP_SR_2, RP_EPA_2, RP_plays_2)) %>%
      tab_spanner('Run-Run ', columns = c(RR_SR_2, RR_EPA_2, RR_plays_2)) %>%
      
      # Column labels
      cols_label(
        # Left side
        PP_SR = 'SR', PP_EPA = 'EPA', PP_plays = 'Plays',
        PR_SR = 'SR', PR_EPA = 'EPA', PR_plays = 'Plays',
        RR_SR = 'SR', RR_EPA = 'EPA', RR_plays = 'Plays',
        RP_SR = 'SR', RP_EPA = 'EPA', RP_plays = 'Plays',
        SR = 'SR', EPA = 'EPA', plays = 'Plays',
        wordmark = 'Team', rank = 'Rank',
        # Right side
        PP_SR_2 = 'SR', PP_EPA_2 = 'EPA', PP_plays_2 = 'Plays',
        PR_SR_2 = 'SR', PR_EPA_2 = 'EPA', PR_plays_2 = 'Plays',
        RR_SR_2 = 'SR', RR_EPA_2 = 'EPA', RR_plays_2 = 'Plays',
        RP_SR_2 = 'SR', RP_EPA_2 = 'EPA', RP_plays_2 = 'Plays',
        SR_2 = 'SR', EPA_2 = 'EPA', plays_2 = 'Plays',
        wordmark_2 = 'Team', rank_2 = 'Rank'
      ) %>% 
      
      # Format percentages
      fmt_percent(columns = c(SR, PP_SR, PR_SR, RP_SR, RR_SR, 
                              SR_2, PP_SR_2, PR_SR_2, RP_SR_2, RR_SR_2), 
                  decimals = 1) %>% 
      
      # COLOR CODING: Purple (bad) to Green (good) gradient
      # Left side color coding
      data_color(columns = EPA, palette = c(low_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR, palette = c(low_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>% 
      data_color(columns = plays, palette = c(low_color, high_color), domain = range(play_table$plays, na.rm = TRUE)) %>% 
      data_color(columns = PP_EPA, palette = c(low_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR, palette = c(low_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PP_plays, palette = c(low_color, high_color), domain = range(play_table$PP_plays, na.rm = TRUE)) %>%
      data_color(columns = PR_EPA, palette = c(low_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR, palette = c(low_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_plays, palette = c(low_color, high_color), domain = range(play_table$PR_plays, na.rm = TRUE)) %>%
      data_color(columns = RP_EPA, palette = c(low_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR, palette = c(low_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_plays, palette = c(low_color, high_color), domain = range(play_table$RP_plays, na.rm = TRUE)) %>%
      data_color(columns = RR_EPA, palette = c(low_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR, palette = c(low_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_plays, palette = c(low_color, high_color), domain = range(play_table$RR_plays, na.rm = TRUE)) %>%
      
      # Right side color coding
      data_color(columns = EPA_2, palette = c(low_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR_2, palette = c(low_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>% 
      data_color(columns = plays_2, palette = c(low_color, high_color), domain = range(play_table$plays, na.rm = TRUE)) %>% 
      data_color(columns = PP_EPA_2, palette = c(low_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR_2, palette = c(low_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PP_plays_2, palette = c(low_color, high_color), domain = range(play_table$PP_plays, na.rm = TRUE)) %>%
      data_color(columns = PR_EPA_2, palette = c(low_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR_2, palette = c(low_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_plays_2, palette = c(low_color, high_color), domain = range(play_table$PR_plays, na.rm = TRUE)) %>%
      data_color(columns = RP_EPA_2, palette = c(low_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR_2, palette = c(low_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_plays_2, palette = c(low_color, high_color), domain = range(play_table$RP_plays, na.rm = TRUE)) %>%
      data_color(columns = RR_EPA_2, palette = c(low_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR_2, palette = c(low_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_plays_2, palette = c(low_color, high_color), domain = range(play_table$RR_plays, na.rm = TRUE)) %>%  
      # Add team logos/wordmarks
      gt_img_rows(wordmark, height = 25) %>% 
      gt_img_rows(wordmark_2, height = 25) %>% 
      
      # Table outline
      opt_table_outline(color = "#000000", width = px(2)) %>% 
      
      # Title and subtitle
      tab_header(
        title = "NFL Play Sequence Efficiency By Team", 
        subtitle = tab_subtitle
      ) %>% 
      
      # Title styling
      tab_style(
        style = list(
          cell_text(weight = "bold", size = px(24), color = "#000000"),
          cell_fill(color = "#FFFFFF")
        ),
        locations = cells_title(groups = "title")
      ) %>%
      
      # Subtitle styling  
      tab_style(
        style = list(
          cell_text(size = px(16), color = "#666666"),
          cell_fill(color = "#FFFFFF")
        ),
        locations = cells_title(groups = "subtitle")
      ) %>%
      
      # Add attribution footer
      tab_source_note(
        source_note = md("**Analysis:** @AriEizen | **Data:** nflfastR | 
                         **Distance From League Average:**
                         <span style='color: #4B0092;'>Purple</span> (Below Average) →
                         <span style='color: #1AFF1A;'>Green</span> (Above Average)
                         <br/>**Success Rate (SR):** Binary measure of how often a team keeps drives on schedule | **EPA/Play (EPA):** Measures how much a play impacts a team's scoring chance 
                         <br/>Note: Plays on this tab can be counted twice in the data if it is a part of a three play sequence of the same play type")
      ) %>%
      # Style the source note
      tab_style(
        style = list(
          cell_text(size = px(18), color = "#000", style = "italic"),
          cell_fill(color = "#F8F9FA")
        ),
        locations = cells_source_notes()
      ) %>%
      
      # Add some padding and adjust column widths
      cols_width(
        rank ~ px(45),
        rank_2 ~ px(45),
        wordmark ~ px(100),
        wordmark_2 ~ px(100),
        everything() ~ px(65)
      ) %>%
      cols_align(align = "center", columns = c(rank, rank_2, wordmark, wordmark_2,  
                                               SR, EPA, PP_SR, PP_EPA, PR_SR, PR_EPA, 
                                               RP_SR, RP_EPA, RR_SR, RR_EPA,
                                               SR_2, EPA_2, PP_SR_2, PP_EPA_2, PR_SR_2, PR_EPA_2, 
                                               RP_SR_2, RP_EPA_2, RR_SR_2, RR_EPA_2,
                                               plays, PP_plays, PR_plays, RP_plays, RR_plays,
                                               plays_2, PP_plays_2, PR_plays_2, RP_plays_2, RR_plays_2)) %>% 
      tab_style(
        style = cell_borders( sides = "top", color = "#E63946",
                              weight = px(3), style = "solid"),
        locations = cells_body(
          columns = everything(), rows = rank == "AVG")
      ) %>%
      
      # Dark gray background with white text
      tab_style(
        style = list(
          cell_fill(color = "#2C2C2C"),  # Dark gray
          cell_text(weight = "bold", color = "#FFFFFF", size = px(13))
        ),
        locations = cells_body( columns = everything(), rows = rank == "AVG")
      ) %>%
      
      # Remove data color gradient from league avg row
      tab_style(
        style = cell_fill(color = "#2C2C2C"),  # Override gradient
        locations = cells_body(
          columns = c(EPA, SR, PP_EPA, PP_SR, PR_EPA, PR_SR, 
                      RP_EPA, RP_SR, RR_EPA, RR_SR,
                      EPA_2, SR_2, PP_EPA_2, PP_SR_2, PR_EPA_2, PR_SR_2,
                      RP_EPA_2, RP_SR_2, RR_EPA_2, RR_SR_2,
                      plays, PP_plays, PR_plays, RP_plays, RR_plays,
                      plays_2, PP_plays_2, PR_plays_2, RP_plays_2, RR_plays_2),
          rows = rank == "AVG"
        )
      )    
  })
  
  # Action for Tab 2
  applied_overview_2 <- reactiveValues(
    tm = 'ARI',
    week = c(app_data$all_seq %>% pull(week) %>% min(na.rm = TRUE),
             if_else(app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE) > 18,
                     18,
                     app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE))),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5,
    season = most_recent_season(),
    fp = personnel_choices,
    sp = personnel_choices
  )
  
  observeEvent(input$apply_filters_2, {
    applied_overview_2$tm <- input$tm
    applied_overview_2$week <- input$week_2  
    applied_overview_2$wp <- input$wp_2      
    applied_overview_2$down <- or_all(input$down_2, 1:4)
    applied_overview_2$qtr <- or_all(input$qtr_2, 1:5)
    applied_overview_2$season <- or_all(input$season_2, c(2023,2024,2025))
    applied_overview_2$fp <- or_all(input$fp2, personnel_choices)
    applied_overview_2$sp <- or_all(input$sp2, personnel_choices)
  })
  
  output$tree <- renderPlot({
    
    tree_data <- app_data$pbp %>% 
      filter(between(week, as.numeric(min(applied_overview_2$week)), max(applied_overview_2$week)),
             between(wp, min(applied_overview_2$wp)/100, max(applied_overview_2$wp)/100),
             down %in% applied_overview_2$down,
             qtr %in% applied_overview_2$qtr,
             season %in% applied_overview_2$season,
             t_last_p %in% applied_overview_2$fp,
             offense_p %in% applied_overview_2$sp)
    
    subtitle <- paste0(paste(sort(applied_overview_2$season), collapse = ", "), " Season(s) • Weeks ", min(tree_data$week), "-", max(tree_data$week), 
                       " • Win Probability ", min(applied_overview_2$wp), "%-", max(applied_overview_2$wp), "%",
                       if(length(applied_overview_2$down) < 4) paste0(" • Downs: ", paste(applied_overview_2$down, collapse=", ")) else "",
                       if(length(applied_overview_2$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_2$qtr), collapse=", ")) else "",
                       if(length(applied_overview_2$fp) < length(personnel_choices)) paste0(" • 1st Play Personnel: ", paste(applied_overview_2$fp, collapse = ', ')) else "",
                       if(length(applied_overview_2$sp) < length(personnel_choices)) paste0(" • 2nd Play Personnel: ", paste(applied_overview_2$sp, collapse = ', ')) else "")
    
    off_freq_data <- calculate_sequence_frequencies(tree_data, side = 'Off')
    
    tree_plot <- create_sequence_matrix(off_freq_data, subtitle, team = applied_overview_2$tm, side = 'Off', color = input$epa)
    
    tree_plot
  })
  
  # Action for Tab 3
  applied_overview_3 <- reactiveValues(
    tm = 'ARI',
    week = c(app_data$all_seq %>% pull(week) %>% min(na.rm = TRUE),
             if_else(app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE) > 18,
                     18,
                     app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE))),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5,
    season = most_recent_season(),
    fp = personnel_choices,
    sp = personnel_choices
  )
  
  observeEvent(input$apply_filters_3, {
    applied_overview_3$tm <- input$t
    applied_overview_3$week <- input$week_3
    applied_overview_3$wp <- input$wp_3      
    applied_overview_3$down <- or_all(input$down_3, 1:4)
    applied_overview_3$qtr <- or_all(input$qtr_3, 1:5)
    applied_overview_3$season <- or_all(input$season_3, c(2023,2024,2025))
    applied_overview_3$fp <- or_all(input$fp3, personnel_choices)
    applied_overview_3$sp <- or_all(input$sp3, personnel_choices)
  })
  
  output$def_tend <- renderPlot({
    
    def_data <- app_data$pbp %>% 
      filter(between(week, as.numeric(min(applied_overview_3$week)), max(applied_overview_3$week)),
             between(wp, min(applied_overview_3$wp)/100, max(applied_overview_3$wp)/100),
             down %in% applied_overview_3$down,
             qtr %in% applied_overview_3$qtr,
             season %in% applied_overview_3$season,
             t_last_p %in% applied_overview_3$fp,
             offense_p %in% applied_overview_3$sp)
    
    subtitle <- paste0(paste(sort(applied_overview_3$season), collapse = ", "), " Season(s) • Weeks ", min(def_data$week), "-", max(def_data$week), 
                       " • Win Probability ", min(applied_overview_3$wp), "%-", max(applied_overview_3$wp), "%",
                       if(length(applied_overview_3$down) < 4) paste0(" • Downs: ", paste(applied_overview_3$down, collapse=", ")) else "",
                       if(length(applied_overview_3$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_3$qtr), collapse=", ")) else "",
                       if(length(applied_overview_3$fp) < length(personnel_choices)) paste0(" • 1st Play Personnel: ", paste(applied_overview_3$fp, collapse = ', ')) else "",
                       if(length(applied_overview_3$sp) < length(personnel_choices)) paste0(" • 2nd Play Personnel: ", paste(applied_overview_3$sp, collapse = ', ')) else "")
    
    def_freq_data <- calculate_sequence_frequencies(def_data,side = 'Def')
    
    def_plot <- create_sequence_matrix(def_freq_data, subtitle, team = applied_overview_3$tm, side = 'Def', color = input$epa_2)
    
    def_plot
  })
  
  ## Code for Tab 4  
  applied_overview_4 <- reactiveValues(
    tm = 'ARI',
    week = c(app_data$all_seq %>% pull(week) %>% min(na.rm = TRUE),
             if_else(app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE) > 18,
                     18,
                     app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE))),
    wp = c(5,95),
    dist = c('10+','10-7','6-4','3-1','GTG'),
    down = 1:4,
    qtr = 1:5,
    season = most_recent_season(),
    personnel = personnel_choices
  )
  
  observeEvent(input$apply_filters_4, {
    applied_overview_4$tm <- input$t
    applied_overview_4$week <- input$week_4
    applied_overview_4$wp <- input$wp_4  
    applied_overview_4$dist <- or_all(input$dist, c('10+','10-7','6-4','3-1','GTG'))
    applied_overview_4$down <- or_all(input$down_4, 1:4)
    applied_overview_4$qtr <- or_all(input$qtr_4, 1:5)
    applied_overview_4$season <- or_all(input$season_4, c(2023,2024,2025))
    applied_overview_4$personnel <- or_all(input$personnel, personnel_choices)
  })
  
  output$kpi_4 <- renderUI({
    base <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             season %in% applied_overview_4$season,
             penalty == 0,
             playType %in% c("Run","Pass"),
             (offense_p %in% applied_overview_4$personnel))
    
    team_stats <- base %>% 
      group_by(posteam) %>% 
      reframe(epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE), plays = n()) %>% 
      mutate(rank = dense_rank(desc(epa)))
    
    this_team <- team_stats %>% filter(posteam == applied_overview_4$tm)
    n_teams <- team_stats %>% pull(posteam) %>% n_distinct()
    
    team_colors <- teams_colors_logos %>% filter(team_abbr == applied_overview_4$tm)
    c1 <- team_colors %>% pull(team_color)
    c2 <- team_colors %>% pull(team_color2)
    
    layout_column_wrap(
      width = 1/4,
      value_box(title = "EPA / play", value = round(this_team$epa, 3), theme = value_box_theme(bg = c1, fg = "white")),
      value_box(title = "Success rate", value = scales::percent(this_team$sr, accuracy = 0.1), theme = value_box_theme(bg = c2, fg = "white")),
      value_box(title = "Total plays", value = this_team$plays, theme = value_box_theme(bg = c1, fg = "white")),
      value_box(title = "League rank", value = paste0(this_team$rank, " of ", n_teams), theme = value_box_theme(bg = c2, fg = "white"))
    )
  })
  
  output$pbp_table <- render_gt({
    play_table <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm,
             between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             season %in% applied_overview_4$season,
             penalty == 0,
             playType %in% c("Run","Pass"),
             (offense_p %in% applied_overview_4$personnel)) %>% 
      arrange(week, game_id, play_id)
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_4$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_4$season), collapse = ", "), " Season(s) • Weeks ", min(play_table$week), "-", max(play_table$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_4$dist) < 5) paste0(" • Distances: ", paste(applied_overview_4$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_4$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_4$personnel, collapse = ', ')) else "")
    play_table %>% 
      select(week, posteam_wordmark, defteam_wordmark,
             qtr, down,ydstogo, yrdln, yards_gained,playType, offense_p, epa,desc) %>%  
      gt() %>% 
      cols_label(qtr = 'Quarter',down = 'Down',ydstogo = 'To Go',yrdln = 'Yard Line',
                 yards_gained = 'Yards Gained',playType = 'Play Type', offense_p = 'Personnel', epa = 'EPA',
                 desc = 'Play Description', week = 'Week') %>% 
      cols_label(posteam_wordmark = 'Offense', defteam_wordmark = 'Defense') %>%
      tab_header(title = paste0(team_name, ' Play Table'), subtitle = subtitle) %>% 
      fmt_number(columns = epa) %>% 
      # Colored pill for Play Type, replacing the flat data_color fill
      text_transform(
        locations = cells_body(columns = playType),
        fn = function(x) {
          pill_color <- if_else(x == 'Pass', '#457B9D', '#2A9D8F')
          paste0("<span style='background:", pill_color, "22; color:", pill_color, 
                 "; padding:2px 10px; border-radius:10px; font-size:12px; font-weight:bold;'>", x, "</span>")
        }
      ) %>% 
      # Inline EPA bar alongside the value: a diverging bar centered at zero,
      # so a negative EPA visibly extends left of center (not just a shorter
      # bar in the same direction as a positive one). Capped at +/-3 EPA.
      text_transform(
        locations = cells_body(columns = epa),
        fn = function(x) {
          x <- gsub("\u2212", "-", x)  # gt formats negatives with a Unicode minus sign, not ASCII "-"
          val <- as.numeric(x)
          val <- if_else(is.na(val), 0, val)
          magnitude <- pmin(abs(val) / 3, 1) * 50
          bar_color <- if_else(val <= 0, '#4B0092','#1AFF1A')
          left_pos <- if_else(val >= 0, 50, 50 - magnitude)
          paste0(
            "<div style='display:flex; align-items:center; gap:6px;'>",
            "<div style='position:relative; width:40px; height:6px; background:#EAEAEA; border-radius:3px; overflow:hidden;'>",
            "<div style='position:absolute; left:50%; top:0; bottom:0; width:1px; background:#999;'></div>",
            "<div style='position:absolute; left:", left_pos, "%; top:0; height:100%; width:", magnitude, "%; background:", bar_color, ";'></div>",
            "</div>",
            "<span>", round(val, 2), "</span></div>"
          )
        }
      ) %>% 
      gt_img_rows(columns = posteam_wordmark) %>%
      gt_img_rows(columns = defteam_wordmark)
  })
  output$pass_chart <- renderPlot({
    pass_plays <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm,
             between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             season %in% applied_overview_4$season,
             pass == 1 & !is.na(pass_location),
             (offense_p %in% applied_overview_4$personnel))
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_4$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_4$season), collapse = ", "), " Season(s) • Weeks ", min(pass_plays$week), "-", max(pass_plays$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_4$dist) < 5) paste0(" • Distances: ", paste(applied_overview_4$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_4$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_4$personnel, collapse = ', ')) else "")
    
    league_base <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             season %in% applied_overview_4$season,
             pass == 1 & !is.na(pass_location),
             (offense_p %in% applied_overview_4$personnel))
    
    league_mid <- case_when(
      input$tile == 'EPA/Play' ~ mean(league_base$epa, na.rm = TRUE),
      input$tile == 'Success Rate' ~ mean(league_base$success, na.rm = TRUE),
      TRUE ~ 1/12
    )
    
    # League rank of this team's EPA/SR for each pass-location x depth-zone
    # cell, ranked against every team's value for that same cell (1 = best)
    league_zones <- league_base %>% 
      mutate(depth_zone = case_when(
        is_screen_pass ~ "Screen",
        air_yards < 10 ~ "< 10 Yrds",
        air_yards >= 10 & air_yards < 20 ~ "10-20 Yrds",
        air_yards >= 20 ~ "20+ Yrds",
        TRUE ~ NA_character_),
        pass_location = case_when(
          pass_location == 'right' ~ 'Right',
          pass_location == 'middle' ~ 'Middle',
          pass_location == 'left' ~ 'Left',
          TRUE ~ pass_location
        )
      ) %>% 
      mutate(pass_location = factor(pass_location, levels = c('Left','Middle','Right')),
             depth_zone = factor(depth_zone, levels = c('Screen',"< 10 Yrds","10-20 Yrds","20+ Yrds"))) %>% 
      group_by(posteam, pass_location, depth_zone, .drop = FALSE) %>% 
      reframe(epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE)) %>% 
      group_by(pass_location, depth_zone) %>% 
      mutate(epa_rank = dense_rank(-epa), sr_rank = dense_rank(-sr)) %>% 
      ungroup()
    
    n_teams_pass <- league_zones %>% pull(posteam) %>% n_distinct()
    
    team_ranks <- league_zones %>% 
      filter(posteam == applied_overview_4$tm) %>% 
      select(pass_location, depth_zone, epa_rank, sr_rank)
    
    pass <- pass_plays %>% 
      mutate(depth_zone = case_when(
        is_screen_pass ~ "Screen",
        air_yards < 10 #& air_yards >= 0 
        ~ "< 10 Yrds",
        air_yards >= 10 & air_yards < 20 ~ "10-20 Yrds",
        air_yards >= 20 ~ "20+ Yrds",
        TRUE ~ NA_character_),
        pass_location = case_when(
          pass_location == 'right' ~ 'Right',
          pass_location == 'middle' ~ 'Middle',
          pass_location == 'left' ~ 'Left',
          TRUE ~ pass_location
        )
      ) %>% 
      mutate(pass_location = factor(pass_location, levels = c('Left','Middle','Right')),
             depth_zone = factor(depth_zone, levels = c('Screen',"< 10 Yrds","10-20 Yrds","20+ Yrds"))) %>%  
      group_by(pass_location, depth_zone, .drop = FALSE) %>% 
      reframe(plays = n(),
              sr = mean(success),
              epa = mean(epa)) %>% 
      left_join(team_ranks, by = c('pass_location','depth_zone')) %>% 
      mutate(label = paste0('EPA: ', round(epa, 2), ' (', epa_rank, '/', n_teams_pass, ')',
                            '\nSR: ', percent(sr, accuracy = .1), ' (', sr_rank, '/', n_teams_pass, ')',
                            '\n', plays, ' plays'),
             freq = plays/sum(plays),
             fill_color = case_when(
               input$tile == 'EPA/Play' ~ epa,
               input$tile == 'Success Rate' ~ sr,
               input$tile == 'Frequency' ~ freq
             )) 
    
    total_plays <- sum(pass$plays)
    
    ggplot(pass, aes(x = pass_location, y = depth_zone, fill = fill_color)) +
      geom_tile() +
      geom_label(aes(label = label), fill = 'white', color = 'black', fontface = 'bold', size = 4.5, lineheight = 1.1) +
      # Vertical grid
      geom_segment(aes(x = 1.5, xend = 1.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = 2.5, xend = 2.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      # Horizontal Grid
      geom_segment(aes(x = .5, xend = 3.5, y = 1.5,yend = 1.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 2.5,yend = 2.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 3.5,yend = 3.5), size = 1.5, color = 'white') +
      scale_fill_gradient2(low = '#4B0092', mid = "#DDDDDD", high = '#1AFF1A', midpoint = league_mid
      ) +
      labs(
        title = paste0(team_name, ' Pass Plays | ',total_plays, ' plays'),
        subtitle = subtitle,
        caption = paste0("**Analysis:** @arieizen | **Data:** nflfastR | **Tile Color** = ", input$tile, " vs league average | Ranks (1 = best) are vs all teams for that cell"),
        x = 'Pass Location',
        y = 'Air Yards'
      ) +
      coord_cartesian(xlim = c(1.1,2.9), ylim = c(1.1,3.9)) +
      apply_theme()
    
    
  })
  output$run_chart <- renderPlot({
    rush_data <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm &
               between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)) &
               between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100) &
               down %in% applied_overview_4$down &
               qtr %in% applied_overview_4$qtr &
               dist %in% applied_overview_4$dist &
               season %in% applied_overview_4$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_4$personnel)) 
    rush <- rush_data %>% 
      group_by(run_location, run_gap, penalty) %>% 
      reframe(plays = n(),
              sr = mean(success),
              epa = mean(epa)) %>% 
      mutate(gap_side = case_when(
        run_location == 'left' & run_gap == 'end' ~ 'Left End',
        run_location == 'left' & run_gap == 'guard' ~ 'Left Guard',
        run_location == 'left' & run_gap == 'tackle' ~ 'Left Tackle',
        run_location == 'right' & run_gap == 'end' ~ 'Right End',
        run_location == 'right' & run_gap == 'guard' ~ 'Right Guard',
        run_location == 'right' & run_gap == 'tackle' ~ 'Right Tackle',
        run_location == 'middle' ~ 'Middle',
        is.na(run_gap) & is.na(run_location) ~ 'Other'
      )) %>% 
      filter(gap_side != 'Other' & penalty != 1) %>% 
      mutate(freq = plays/sum(plays))
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_4$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_4$season), collapse = ", "), " Season(s) • Weeks ", min(rush_data$week), "-", max(rush_data$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_4$dist) < 5) paste0(" • Distances: ", paste(applied_overview_4$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_4$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_4$personnel, collapse = ', ')) else "")
    
    avg_epa <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)) &
               between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100) &
               down %in% applied_overview_4$down &
               qtr %in% applied_overview_4$qtr &
               dist %in% applied_overview_4$dist &
               season %in% applied_overview_4$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_4$personnel)) %>% 
      pull(epa) %>% 
      mean(na.rm = TRUE)
    total_runs <- sum(rush$plays)
    
    # League rank of this team's EPA/SR for each run gap, ranked against
    # every team's value for that same gap (1 = best)
    league_rush <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)) &
               between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100) &
               down %in% applied_overview_4$down &
               qtr %in% applied_overview_4$qtr &
               dist %in% applied_overview_4$dist &
               season %in% applied_overview_4$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_4$personnel)) %>% 
      group_by(posteam, run_location, run_gap, penalty) %>% 
      reframe(sr = mean(success), epa = mean(epa)) %>% 
      mutate(gap_side = case_when(
        run_location == 'left' & run_gap == 'end' ~ 'Left End',
        run_location == 'left' & run_gap == 'guard' ~ 'Left Guard',
        run_location == 'left' & run_gap == 'tackle' ~ 'Left Tackle',
        run_location == 'right' & run_gap == 'end' ~ 'Right End',
        run_location == 'right' & run_gap == 'guard' ~ 'Right Guard',
        run_location == 'right' & run_gap == 'tackle' ~ 'Right Tackle',
        run_location == 'middle' ~ 'Middle',
        is.na(run_gap) & is.na(run_location) ~ 'Other'
      )) %>% 
      filter(gap_side != 'Other' & penalty != 1) %>% 
      group_by(gap_side) %>% 
      mutate(epa_rank = dense_rank(-epa), sr_rank = dense_rank(-sr)) %>% 
      ungroup()
    
    n_teams_run <- league_rush %>% pull(posteam) %>% n_distinct()
    
    team_gap_ranks <- league_rush %>% 
      filter(posteam == applied_overview_4$tm) %>% 
      select(gap_side, epa_rank, sr_rank)
    
    segments_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      # Top part of Line (From RB to angle change)
      x_start_top = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_start_top = rep(.99, 7),
      x_end_top = rep(3, 7),
      y_end_top = rep(0.981, 7),
      # Bottom part of line (Angle to LOS)
      x_start_bot = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_start_bot = rep(0.99, 7),
      x_end_bot = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_end_bot = rep(1.002, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    oline <- data.frame(
      position = c('LT','LG','C','RG','RT'),
      x = 1:5,
      y = rep(1, 5)
    )
    
    labels_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      # Label x-positions are spread wider than the arrow tips (below) for
      # the three closely-packed middle gaps (LG/C/RG are only 0.5 apart),
      # so the label boxes get breathing room even though the arrows still
      # point at the true gap locations.
      x = c(0.3, 1.5, 2.15, 3, 3.85, 4.5, 5.7),
      y = rep(c(1.008, 1.016), length.out = 7)
    ) %>%
      left_join(rush, by = 'gap_side') %>%
      left_join(team_gap_ranks, by = 'gap_side') %>%
      mutate(label_text = paste0(round(epa,2), " (", epa_rank, "/", n_teams_run, ")",
                                 "\n", percent(sr, accuracy = 0.1), " SR (", sr_rank, "/", n_teams_run, ")",
                                 "\n Carries: ",plays))
    
    mid_color = "#DDDDDD"
    low_color = '#4B0092'
    high_color = '#1AFF1A'
    
    play_totals <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      x = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y = rep(.979, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    ggplot() +
      geom_segment(aes(x = -1, y = 1, xend = 7, yend = 1), 
                   linewidth = 2, lineend = 'butt', linejoin = 'bevel', 
                   color = 'white') +
      geom_segment(data = segments_data, 
                   aes(x = x_start_bot, y = y_start_bot, xend = x_end_bot, yend = y_end_bot, 
                       color = epa, linewidth = freq, linetype = 'round'),
                   lineend = 'round', linejoin = 'round', 
                   arrow = arrow(type = 'closed',length = unit(0.3, 'inches'))) +
      geom_segment(data = segments_data,
                   aes(x = x_start_top, y = y_start_top, xend = x_end_top, yend = y_end_top,
                       color = epa, linewidth = freq, linetype = 'round'),
                   lineend = 'round', linejoin = 'round') +
      geom_point(aes(x = 3, y = .98), color = '#468944', size = 10) + 
      geom_shadowtext(data = oline, aes(x = x, y = y, label = position), 
                      fontface = 'bold', size = 10, bg.color = 'grey10') +
      geom_label(data = labels_data, 
                 aes(x = x, y = y, label = label_text),
                 color = 'black', fill = 'white', fontface = 'bold',
                 size = 5, lineheight = 0.9) +
      scale_color_gradient2(low = low_color, mid = mid_color, high = high_color,
                            midpoint = avg_epa,
                            name = "EPA/Play") +
      coord_cartesian(xlim = c(0, 6), ylim = c(0.98, 1.02)) +
      labs(
        title = paste0(team_name, ' Run Plays & QB Scrambles By Location | ', total_runs,' Plays'),
        subtitle = subtitle,
        caption = paste0("**Analysis:** @arieizen | **Data:** nflfastR | **Arrow color** = EPA/Play",
                         # | **Label** = EPA/Play (Success Rate)
                         " | **Line Width** = Frequency | Ranks (1 = best) are vs all teams for that gap"),
        x = '',
        y = ''
      ) +
      # theme_void() +
      apply_theme() +
      theme(
        axis.text = element_blank()
      )
  })
  
  ## Code for Tab 5 
  applied_overview_5 <- reactiveValues(
    tm = 'ARI',
    week = c(app_data$all_seq %>% pull(week) %>% min(na.rm = TRUE),
             if_else(app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE) > 18,
                     18,
                     app_data$all_seq %>% pull(week) %>% max(na.rm = TRUE))),
    wp = c(5,95),
    dist = c('10+','10-7','6-4','3-1','GTG'),
    down = 1:4,
    qtr = 1:5,
    season = most_recent_season(),
    personnel = personnel_choices
  )
  
  observeEvent(input$apply_filters_5, {
    applied_overview_5$tm <- input$t_2
    applied_overview_5$week <- input$week_5
    applied_overview_5$wp <- input$wp_5
    applied_overview_5$dist <- or_all(input$dist_2, c('10+','10-7','6-4','3-1','GTG'))
    applied_overview_5$down <- or_all(input$down_5, 1:4)
    applied_overview_5$qtr <- or_all(input$qtr_5, 1:5)
    applied_overview_5$season <- or_all(input$season_5, c(2023,2024,2025))
    applied_overview_5$personnel <- or_all(input$personnel_2, personnel_choices)
  })
  
  output$kpi_5 <- renderUI({
    base <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)),
             between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100),
             down %in% applied_overview_5$down,
             qtr %in% applied_overview_5$qtr,
             dist %in% applied_overview_5$dist,
             season %in% applied_overview_5$season,
             penalty == 0,
             playType %in% c("Run","Pass"),
             (offense_p %in% applied_overview_5$personnel))
    
    team_stats <- base %>% 
      group_by(defteam) %>% 
      reframe(epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE), plays = n()) %>% 
      mutate(rank = dense_rank(epa))
    print(team_stats |> filter(defteam == 'ARI'))
    this_team <- team_stats %>% filter(defteam == applied_overview_5$tm)
    print(this_team)
    n_teams <- team_stats %>% pull(defteam) %>% n_distinct()
    
    team_colors <- teams_colors_logos %>% filter(team_abbr == applied_overview_5$tm)
    c1 <- team_colors %>% pull(team_color)
    c2 <- team_colors %>% pull(team_color2)
    
    layout_column_wrap(
      width = 1/4,
      value_box(title = "EPA / play allowed", value = round(this_team$epa, 3), theme = value_box_theme(bg = c1, fg = "white")),
      value_box(title = "Success rate allowed", value = scales::percent(this_team$sr, accuracy = 0.1), theme = value_box_theme(bg = c2, fg = "white")),
      value_box(title = "Total plays", value = this_team$plays, theme = value_box_theme(bg = c1, fg = "white")),
      value_box(title = "League rank", value = paste0(this_team$rank, " of ", n_teams), theme = value_box_theme(bg = c2, fg = "white"))
    )
  })
  
  output$pbp_table_def <- render_gt({
    # print(applied_overview_5$dist)
    play_table <- app_data$full_data %>% 
      filter(defteam == applied_overview_5$tm,
             between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)),
             between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100),
             down %in% applied_overview_5$down,
             qtr %in% applied_overview_5$qtr,
             dist %in% applied_overview_5$dist,
             season %in% applied_overview_5$season,
             penalty == 0,
             playType %in% c("Run","Pass"),
             (offense_p %in% applied_overview_5$personnel)) %>% 
      arrange(week, game_id, play_id)
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_5$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_5$season), collapse = ", "), " Season(s) • Weeks ", min(play_table$week), "-", max(play_table$week), 
                       " • Win Probability ", min(applied_overview_5$wp), "%-", max(applied_overview_5$wp), "%",
                       if(length(applied_overview_5$down) < 4) paste0(" • Downs: ", paste(applied_overview_5$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_5$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_5$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_5$dist) < 5) paste0(" • Distances: ", paste(applied_overview_5$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_5$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_5$personnel, collapse = ', ')) else "")
    
    play_table %>% 
      select(week, posteam_wordmark, defteam_wordmark,
             qtr, down,ydstogo, yrdln, yards_gained,playType, offense_p, epa,desc) %>%  
      gt() %>% 
      cols_label(qtr = 'Quarter',down = 'Down',ydstogo = 'To Go',yrdln = 'Yard Line',
                 yards_gained = 'Yards Gained',playType = 'Play Type', offense_p = 'Personnel', epa = 'EPA',
                 desc = 'Play Description', week = 'Week') %>% 
      cols_label(posteam_wordmark = 'Offense', defteam_wordmark = 'Defense') %>%
      tab_header(title = paste0(team_name, ' Defensive Play Table'), subtitle = subtitle) %>% 
      fmt_number(columns = epa) %>% 
      # Colored pill for Play Type, replacing the flat data_color fill
      text_transform(
        locations = cells_body(columns = playType),
        fn = function(x) {
          pill_color <- if_else(x == 'Pass', '#457B9D', '#2A9D8F')
          paste0("<span style='background:", pill_color, "22; color:", pill_color, 
                 "; padding:2px 10px; border-radius:10px; font-size:12px; font-weight:bold;'>", x, "</span>")
        }
      ) %>% 
      # Inline EPA bar: same zero-centered diverging bar as the offensive
      # table, but color flipped since a lower EPA allowed is the good
      # outcome on defense
      text_transform(
        locations = cells_body(columns = epa),
        fn = function(x) {
          x <- gsub("\u2212", "-", x)  # gt formats negatives with a Unicode minus sign, not ASCII "-"
          val <- as.numeric(x)
          val <- if_else(is.na(val), 0, val)
          magnitude <- pmin(abs(val) / 3, 1) * 50
          bar_color <- if_else(val <= 0, '#4B0092','#1AFF1A')
          left_pos <- if_else(val >= 0, 50, 50 - magnitude)
          paste0(
            "<div style='display:flex; align-items:center; gap:6px;'>",
            "<div style='position:relative; width:40px; height:6px; background:#EAEAEA; border-radius:3px; overflow:hidden;'>",
            "<div style='position:absolute; left:50%; top:0; bottom:0; width:1px; background:#999;'></div>",
            "<div style='position:absolute; left:", left_pos, "%; top:0; height:100%; width:", magnitude, "%; background:", bar_color, ";'></div>",
            "</div>",
            "<span>", round(val, 2), "</span></div>"
          )
        }
      ) %>% 
      gt_img_rows(columns = posteam_wordmark) %>%
      gt_img_rows(columns = defteam_wordmark)
    
    
  })
  output$pass_chart_def <- renderPlot({
    pass_plays <- app_data$full_data %>% 
      filter(defteam == applied_overview_5$tm,
             between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)),
             between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100),
             down %in% applied_overview_5$down,
             qtr %in% applied_overview_5$qtr,
             dist %in% applied_overview_5$dist,
             season %in% applied_overview_5$season,
             pass == 1 & !is.na(pass_location),
             (offense_p %in% applied_overview_5$personnel))
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_5$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_5$season), collapse = ", "), " Season(s) • Weeks ", min(pass_plays$week), "-", max(pass_plays$week), 
                       " • Win Probability ", min(applied_overview_5$wp), "%-", max(applied_overview_5$wp), "%",
                       if(length(applied_overview_5$down) < 4) paste0(" • Downs: ", paste(applied_overview_5$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_5$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_5$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_5$dist) < 5) paste0(" • Distances: ", paste(applied_overview_5$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_5$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_5$personnel, collapse = ', ')) else "")
    
    league_base <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)),
             between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100),
             down %in% applied_overview_5$down,
             qtr %in% applied_overview_5$qtr,
             dist %in% applied_overview_5$dist,
             season %in% applied_overview_5$season,
             pass == 1 & !is.na(pass_location),
             (offense_p %in% applied_overview_5$personnel))
    
    league_mid <- case_when(
      input$tile_2 == 'EPA/Play' ~ mean(league_base$epa, na.rm = TRUE),
      input$tile_2 == 'Success Rate' ~ mean(league_base$success, na.rm = TRUE),
      TRUE ~ -1/12
    )
    
    # League rank of this defense's EPA/SR allowed for each pass-location x
    # depth-zone cell, ranked against every defense's value for that same
    # cell (1 = best, i.e. lowest EPA/SR allowed)
    league_zones <- league_base %>% 
      mutate(depth_zone = case_when(
        is_screen_pass ~ "Screen",
        air_yards < 10 ~ "< 10 Yrds",
        air_yards >= 10 & air_yards < 20 ~ "10-20 Yrds",
        air_yards >= 20 ~ "20+ Yrds",
        TRUE ~ NA_character_),
        pass_location = case_when(
          pass_location == 'right' ~ 'Right',
          pass_location == 'middle' ~ 'Middle',
          pass_location == 'left' ~ 'Left',
          TRUE ~ pass_location
        )
      ) %>% 
      mutate(pass_location = factor(pass_location, levels = c('Left','Middle','Right')),
             depth_zone = factor(depth_zone, levels = c('Screen',"< 10 Yrds","10-20 Yrds","20+ Yrds"))) %>% 
      group_by(defteam, pass_location, depth_zone, .drop = FALSE) %>% 
      reframe(epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE)) %>% 
      group_by(pass_location, depth_zone) %>% 
      mutate(epa_rank = dense_rank(epa), sr_rank = dense_rank(sr)) %>% 
      ungroup()
    
    n_teams_pass <- league_zones %>% pull(defteam) %>% n_distinct()
    
    team_ranks <- league_zones %>% 
      filter(defteam == applied_overview_5$tm) %>% 
      select(pass_location, depth_zone, epa_rank, sr_rank)
    
    pass <- pass_plays %>% 
      mutate(depth_zone = case_when(
        is_screen_pass ~ "Screen",
        air_yards < 10 #& air_yards >= 0 
        ~ "< 10 Yrds",
        air_yards >= 10 & air_yards < 20 ~ "10-20 Yrds",
        air_yards >= 20 ~ "20+ Yrds",
        TRUE ~ NA_character_),
        pass_location = case_when(
          pass_location == 'right' ~ 'Right',
          pass_location == 'middle' ~ 'Middle',
          pass_location == 'left' ~ 'Left',
          TRUE ~ pass_location
        )
      ) %>% 
      mutate(pass_location = factor(pass_location, levels = c('Left','Middle','Right')),
             depth_zone = factor(depth_zone, levels = c('Screen',"< 10 Yrds","10-20 Yrds","20+ Yrds"))) %>% 
      group_by(pass_location, depth_zone, .drop = FALSE) %>% 
      reframe(plays = n(),
              sr = mean(success),
              epa = mean(epa)) %>% 
      left_join(team_ranks, by = c('pass_location','depth_zone')) %>% 
      mutate(label = paste0('EPA: ', round(epa, 2), ' (', epa_rank, '/', n_teams_pass, ')',
                            '\nSR: ', percent(sr, accuracy = .1), ' (', sr_rank, '/', n_teams_pass, ')',
                            '\n', plays, ' plays'),
             freq = plays/sum(plays),
             fill_color = case_when(
               input$tile_2 == 'EPA/Play' ~ epa,
               input$tile_2 == 'Success Rate' ~ sr,
               input$tile_2 == 'Frequency' ~ -freq
             )) 
    total_plays <- sum(pass$plays)
    
    ggplot(pass, aes(x = pass_location, y = depth_zone, fill = fill_color)) +
      geom_tile() +
      geom_label(aes(label = label), fill = 'white', color = 'black', fontface = 'bold', size = 4.5, lineheight = 1.1) +
      # Vertical grid
      geom_segment(aes(x = 1.5, xend = 1.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = 2.5, xend = 2.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      # Horizontal Grid
      geom_segment(aes(x = .5, xend = 3.5, y = 1.5,yend = 1.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 2.5,yend = 2.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 3.5,yend = 3.5), size = 1.5, color = 'white') +
      scale_fill_gradient2(low = '#1AFF1A', mid = "#DDDDDD", high = '#4B0092', midpoint = league_mid) +
      labs(
        title = paste0(team_name, ' Defensive Pass Plays | ',total_plays, ' plays'),
        subtitle = subtitle,
        caption = paste0("**Analysis:** @arieizen | **Data:** nflfastR | **Tile Color** = ", input$tile_2, " vs league average | Ranks (1 = best) are vs all teams for that cell"),
        x = 'Pass Location',
        y = 'Air Yards',
        x = '',
        y = ''
      ) +
      coord_cartesian(xlim = c(1.1,2.9), ylim = c(1.1,3.9)) +
      apply_theme()
  })
  output$run_chart_def <- renderPlot({
    rush_data <- app_data$full_data %>% 
      filter(defteam == applied_overview_5$tm &
               between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)) &
               between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100) &
               down %in% applied_overview_5$down &
               qtr %in% applied_overview_5$qtr &
               dist %in% applied_overview_5$dist &
               season %in% applied_overview_5$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_5$personnel)) 
    rush <- rush_data %>% 
      group_by(run_location, run_gap, penalty) %>% 
      reframe(plays = n(),
              sr = mean(success),
              epa = mean(epa)) %>% 
      mutate(gap_side = case_when(
        run_location == 'left' & run_gap == 'end' ~ 'Left End',
        run_location == 'left' & run_gap == 'guard' ~ 'Left Guard',
        run_location == 'left' & run_gap == 'tackle' ~ 'Left Tackle',
        run_location == 'right' & run_gap == 'end' ~ 'Right End',
        run_location == 'right' & run_gap == 'guard' ~ 'Right Guard',
        run_location == 'right' & run_gap == 'tackle' ~ 'Right Tackle',
        run_location == 'middle' ~ 'Middle',
        is.na(run_gap) & is.na(run_location) ~ 'Other'
      )) %>% 
      filter(gap_side != 'Other' & penalty != 1) %>% 
      mutate(freq = plays/sum(plays))
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_5$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0(paste(sort(applied_overview_5$season), collapse = ", "), " Season(s) • Weeks ", min(rush_data$week), "-", max(rush_data$week), 
                       " • Win Probability ", min(applied_overview_5$wp), "%-", max(applied_overview_5$wp), "%",
                       if(length(applied_overview_5$down) < 4) paste0(" • Downs: ", paste(applied_overview_5$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_5$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_5$qtr), collapse=", ")) else " • All Qtrs ",
                       if(length(applied_overview_5$dist) < 5) paste0(" • Distances: ", paste(applied_overview_5$dist, collapse=", ")) else " • All Distances",
                       if(length(applied_overview_5$personnel) < length(personnel_choices)) paste0(" • Personnel: ", paste(applied_overview_5$personnel, collapse = ', ')) else "")
    
    avg_epa <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)) &
               between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100) &
               down %in% applied_overview_5$down &
               qtr %in% applied_overview_5$qtr &
               dist %in% applied_overview_5$dist &
               season %in% applied_overview_5$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_5$personnel)) %>% 
      pull(epa) %>% 
      mean(na.rm = TRUE)
    total_runs <- sum(rush$plays)
    
    # League rank of this defense's EPA/SR allowed for each run gap, ranked
    # against every defense's value for that same gap (1 = best, i.e.
    # lowest EPA/SR allowed)
    league_rush <- app_data$full_data %>% 
      filter(between(week, as.numeric(min(applied_overview_5$week)), max(applied_overview_5$week)) &
               between(wp, min(applied_overview_5$wp)/100, max(applied_overview_5$wp)/100) &
               down %in% applied_overview_5$down &
               qtr %in% applied_overview_5$qtr &
               dist %in% applied_overview_5$dist &
               season %in% applied_overview_5$season &
               (rush == 1| qb_scramble == 1) &
               (offense_p %in% applied_overview_5$personnel)) %>% 
      group_by(defteam, run_location, run_gap, penalty) %>% 
      reframe(sr = mean(success), epa = mean(epa)) %>% 
      mutate(gap_side = case_when(
        run_location == 'left' & run_gap == 'end' ~ 'Left End',
        run_location == 'left' & run_gap == 'guard' ~ 'Left Guard',
        run_location == 'left' & run_gap == 'tackle' ~ 'Left Tackle',
        run_location == 'right' & run_gap == 'end' ~ 'Right End',
        run_location == 'right' & run_gap == 'guard' ~ 'Right Guard',
        run_location == 'right' & run_gap == 'tackle' ~ 'Right Tackle',
        run_location == 'middle' ~ 'Middle',
        is.na(run_gap) & is.na(run_location) ~ 'Other'
      )) %>% 
      filter(gap_side != 'Other' & penalty != 1) %>% 
      group_by(gap_side) %>% 
      mutate(epa_rank = dense_rank(epa), sr_rank = dense_rank(sr)) %>% 
      ungroup()
    
    n_teams_run <- league_rush %>% pull(defteam) %>% n_distinct()
    
    team_gap_ranks <- league_rush %>% 
      filter(defteam == applied_overview_5$tm) %>% 
      select(gap_side, epa_rank, sr_rank)
    
    segments_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      # Top part of Line (From RB to angle change)
      x_start_top = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_start_top = rep(.99, 7),
      x_end_top = rep(3, 7),
      y_end_top = rep(0.981, 7),
      # Bottom part of line (Angle to LOS)
      x_start_bot = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_start_bot = rep(0.99, 7),
      x_end_bot = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_end_bot = rep(1.002, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    oline <- data.frame(
      position = c('LT','LG','C','RG','RT'),
      x = 1:5,
      y = rep(1, 5)
    )
    
    labels_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      # Label x-positions are spread wider than the arrow tips (below) for
      # the three closely-packed middle gaps (LG/C/RG are only 0.5 apart),
      # so the label boxes get breathing room even though the arrows still
      # point at the true gap locations.
      x = c(0.3, 1.5, 2.15, 3, 3.85, 4.5, 5.7),
      y = rep(c(1.008, 1.016), length.out = 7)
    ) %>%
      left_join(rush, by = 'gap_side') %>%
      left_join(team_gap_ranks, by = 'gap_side') %>%
      mutate(label_text = paste0(round(epa,2), " (", epa_rank, "/", n_teams_run, ")",
                                 "\n", percent(sr, accuracy = 0.1), " SR (", sr_rank, "/", n_teams_run, ")",
                                 "\n Carries: ",plays))
    
    
    play_totals <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      x = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y = rep(.979, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    
    mid_color = "#DDDDDD"
    high_color = '#4B0092'
    low_color = '#1AFF1A'
    
    ggplot() +
      geom_segment(aes(x = -1, y = 1, xend = 7, yend = 1), 
                   linewidth = 2, lineend = 'butt', linejoin = 'bevel', 
                   color = 'white') +
      geom_segment(data = segments_data, 
                   aes(x = x_start_bot, y = y_start_bot, xend = x_end_bot, yend = y_end_bot, 
                       color = epa, linewidth = freq, linetype = 'round'),
                   lineend = 'round', linejoin = 'round', 
                   arrow = arrow(type = 'closed',length = unit(0.3, 'inches'))) +
      geom_segment(data = segments_data,
                   aes(x = x_start_top, y = y_start_top, xend = x_end_top, yend = y_end_top,
                       color = epa, linewidth = freq, linetype = 'round'),
                   lineend = 'round', linejoin = 'round') +
      geom_point(aes(x = 3, y = .98), color = '#468944', size = 10) + 
      geom_shadowtext(data = oline, aes(x = x, y = y, label = position), 
                      fontface = 'bold', size = 10, bg.color = 'grey10') +
      geom_label(data = labels_data, 
                 aes(x = x, y = y, label = label_text), 
                 color = 'black', fill = 'white', fontface = 'bold',
                 size = 5, lineheight = 0.9) +
      scale_color_gradient2(low = low_color, mid = mid_color, high = high_color,
                            midpoint = avg_epa,
                            name = "EPA/Play") +
      coord_cartesian(xlim = c(0, 6), ylim = c(0.98, 1.02)) +
      
      labs(
        title = paste0(team_name, ' Defensive Run Plays & QB Scrambles By Location | ', total_runs,' Plays'),
        subtitle = subtitle,
        caption = paste0("**Analysis:** @arieizen | **Data:** nflfastR | **Arrow color** = EPA/Play",
                         # | **Label** = EPA/Play (Success Rate)
                         " | **Line Width** = Frequency | Ranks (1 = best) are vs all teams for that gap"),
        x = '',
        y = ''
      ) +
      # theme_void() +
      apply_theme() +
      theme(
        axis.text = element_blank()
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
