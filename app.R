library(shiny)
library(scales)
library(bslib)
library(tidyverse)
library(nflverse)
library(gt)
library(gtExtras)
library(rsconnect)
library(DT)

## Functions for the Code
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
            success_rate = mean(success)) %>% 
    merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3, team_wordmark,team_logo_wikipedia),
          by.x = 'posteam',by.y = 'team_abbr') %>% 
    unique()
  
  whole_totals <- pbp_data %>% 
    group_by(posteam) %>% 
    mutate(total_epa = mean(epa),
           total_sr = mean(success)) 
  
  
  # print(seq_chart)
  new_table <- c()
  for (k in 1:length(unique(seq_chart$posteam))) {
    tms <- seq_chart %>% pull(posteam) %>% unique()
    tm <- tms[k]
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
    
    # Get Pass-Pass Metrics
    PP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Pass-Run Metrics
    PR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Run-Pass Metrics
    RP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Run-Run Metrics
    RR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    team_row <- data.frame(
      posteam = tm,
      primary, 
      secondary,
      tertiary,
      wordmark,
      logo,
      EPA = if (length(EPA) == 0) 0 else EPA,
      SR = if (length(SR) == 0) 0 else SR,
      PP_EPA = if (length(PP_EPA) == 0) 0 else PP_EPA,
      PP_SR = if (length(PP_SR) == 0) 0 else PP_SR,
      PR_EPA = if (length(PR_EPA) == 0) 0 else PR_EPA,
      PR_SR = if (length(PR_SR) == 0) 0 else PR_SR,
      RP_EPA = if (length(RP_EPA) == 0) 0 else RP_EPA,
      RP_SR = if (length(RP_SR) == 0) 0 else RP_SR,
      RR_EPA = if (length(RR_EPA) == 0) 0 else RR_EPA,
      RR_SR = if (length(RR_SR) == 0) 0 else RR_SR
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
  
  
  # Get Pass-Pass Metrics
  SR <- lg_avg %>% 
    pull(SR) %>%
    unique()
  
  EPA <- lg_avg %>% 
    pull(EPA) %>% 
    round(3) %>%
    unique()
  
  # Get Pass-Pass Metrics
  PP_SR <- seq_avg %>% 
    filter(seq_group == 'Pass-Pass') %>% 
    pull(SR) 
  
  PP_EPA <- seq_avg %>% 
    filter(seq_group == 'Pass-Pass') %>% 
    pull(EPA) %>% 
    round(3)
  
  # Get Pass-Run Metrics
  PR_SR <- seq_avg %>% 
    filter(seq_group == 'Pass-Run') %>% 
    pull(SR) 
  
  PR_EPA <- seq_avg %>% 
    filter(seq_group == 'Pass-Run') %>% 
    pull(EPA) %>% 
    round(3)
  
  # Get Run-Pass Metrics
  RP_SR <- seq_avg %>% 
    filter(seq_group == 'Run-Pass') %>% 
    pull(SR) 
  
  RP_EPA <- seq_avg %>% 
    filter(seq_group == 'Run-Pass') %>% 
    pull(EPA) %>% 
    round(3)
  
  # Get Run-Run Metrics
  RR_SR <- seq_avg %>% 
    filter(seq_group == 'Run-Run') %>% 
    pull(SR) 
  
  RR_EPA <- seq_avg %>% 
    filter(seq_group == 'Run-Run') %>% 
    pull(EPA) %>% 
    round(3)
  
  team_row <- data.frame(
    posteam = 'NFL',
    primary = NA, 
    secondary = NA,
    tertiary = NA,
    wordmark = "https://raw.githubusercontent.com/nflverse/nflfastR-data/master/NFL.png",
    # logo = NA,
    EPA = if (length(EPA) == 0) 0 else EPA,
    SR = if (length(SR) == 0) 0 else SR,
    PP_EPA = if (length(PP_EPA) == 0) 0 else PP_EPA,
    PP_SR = if (length(PP_SR) == 0) 0 else PP_SR,
    PR_EPA = if (length(PR_EPA) == 0) 0 else PR_EPA,
    PR_SR = if (length(PR_SR) == 0) 0 else PR_SR,
    RP_EPA = if (length(RP_EPA) == 0) 0 else RP_EPA,
    RP_SR = if (length(RP_SR) == 0) 0 else RP_SR,
    RR_EPA = if (length(RR_EPA) == 0) 0 else RR_EPA,
    RR_SR = if (length(RR_SR) == 0) 0 else RR_SR,
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
    PP_EPA_2 = if (length(PP_EPA) == 0) 0 else PP_EPA,
    PP_SR_2 = if (length(PP_SR) == 0) 0 else PP_SR,
    PR_EPA_2 = if (length(PR_EPA) == 0) 0 else PR_EPA,
    PR_SR_2 = if (length(PR_SR) == 0) 0 else PR_SR,
    RP_EPA_2 = if (length(RP_EPA) == 0) 0 else RP_EPA,
    RP_SR_2 = if (length(RP_SR) == 0) 0 else RP_SR,
    RR_EPA_2 = if (length(RR_EPA) == 0) 0 else RR_EPA,
    RR_SR_2 = if (length(RR_SR) == 0) 0 else RR_SR
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
      mutate(first_epa_rk = dense_rank(-first_epa_play),
             first_SR_rk = dense_rank(-first_SR),
             second_epa_rk = dense_rank(-epa_play),
             second_SR_rk = dense_rank(-SR)) %>% 
      merge(logos, by.x = 'defteam',by.y = 'team_abbr')
  }
  
  return(frequency_data)
}

create_off_decision_trees <- function(data, subtitle, off_team = 'DET') {
  
  freq_data <- data %>% 
    filter(posteam == off_team)
  
  logo_data <- data.frame(
    x = 1.5,
    y = 1.5,
    team = off_team
  )
  
  team_name <- freq_data %>% pull(team_name) %>% unique()
  primary <- freq_data %>% pull(team_color) %>% unique()
  secondary <- freq_data %>% pull(team_color2) %>% unique()
  third <- freq_data %>% pull(team_color3) %>% unique()
  
  # Process data for all teams
  tree_data <- freq_data %>%
    mutate(
      first_play = t_last_play,
      second_play = playType,
      x_root = ifelse(first_play == "Pass", 0.5, 2.5),
      x_end = case_when(
        first_play == "Pass" & second_play == "Pass" ~ 0,
        first_play == "Pass" & second_play == "Run" ~ 1,
        first_play == "Run" & second_play == "Pass" ~ 2,
        first_play == "Run" & second_play == "Run" ~ 3,
      ),
      y_root = 2,
      y_end = 1,
      # Compact labels for space efficiency
      edge_label = paste0(round(frequency * 100), "%"),
      first_epa_diff = first_epa_play - first_LA_epa_play,
      epa_diff = epa_play - second_LA_epa_play,
      # Create compact node labels
      root_label = paste0(first_play, "\nEPA: ", round(first_epa_play, 2)," (",first_epa_rk,")", 
                          "\nSR: ", round(first_SR * 100), "%", " (",first_SR_rk,")"),
      end_label = paste0(second_play, "\nEPA: ", round(epa_play, 2), " (",second_epa_rk,")",
                         "\nSR: ", round(SR * 100), "%", " (",second_SR_rk,")")
    )
  
  # Create the plot with facets
  p <- ggplot(tree_data) +
    # Background 
    annotate("rect", xmin = -1.75, xmax = 4.75, ymin = 0, ymax = 2.6,fill = third, alpha = .5) +
    # Caption
    annotate('text', label = 'bold("@arieizen | data: nflfastR")', x = 3.55, y = .45, size = 6.5, parse = TRUE) +
    # Team Wordmark
    # geom_nfl_wordmarks( data = logo_data, aes(x = x, y = y, team_abbr = team), width = 0.75, alpha = 0.7) +
    # Frequency Connectors
    geom_segment( aes(x = x_root, y = y_root, xend = x_end, yend = y_end, linewidth = 5, color = frequency)) +
    # Frequency Label
    geom_label(
      aes(x = (x_root + x_end) / 2, y = (y_root + y_end) / 2, label = edge_label),
      size = 6.5, fontface = "bold", color = "black", fill = 'white') +
    # Root nodes (Pass/Run starting points)
    geom_label(
      data = tree_data %>% distinct(posteam, first_play, x_root, y_root, 
                                    first_epa_play, first_SR, first_epa_diff, root_label),
      aes(x = x_root, y = y_root, fill = first_epa_diff, label = root_label),
      size = 6.5, label.padding = unit(0.3, "lines"), 
      label.r = unit(0.2, "lines"), fontface = "bold"
    ) +
    # Outcome nodes (second play results)
    geom_label(
      aes(x = x_end, y = y_end, fill = epa_diff, label = end_label),
      size = 6.5,label.padding = unit(0.3, "lines"),
      label.r = unit(0.2, "lines"), fontface = "bold"
    ) +
    # Color scales
    scale_color_gradient2(
      low = '#3B4CC0', high = '#B40426', mid = "#DDDDDD", 
      midpoint = 0.5, name = "Frequency"
    ) +
    scale_fill_gradient2(
      low = '#3B4CC0', high = '#B40426', mid = "#DDDDDD", 
      name = "EPA vs League Avg"
    ) +
    
    # Labels and theming
    labs(
      title = paste0(team_name, ' Offensive Play Calling Tendancies'),
      subtitle = subtitle,
      # caption = "@arieizen | data: nflfastR"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 24,face = "bold",hjust = 0.5,color = primary),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 14,face = "bold"),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.spacing = unit(0.1, "lines"),
      panel.border = element_rect(colour = secondary, fill=NA, linewidth=2.5)
    ) +
    coord_cartesian(xlim = c(-0.75, 3.75),ylim = c(0.5, 2.5),clip = "off")
  
  return(p)
}

create_def_decision_trees <- function(data, subtitle, def_team = 'DET') {
  
  freq_data <- data %>% 
    filter(defteam == def_team)
  
  logo_data <- data.frame(
    x = 1.5,
    y = 1.5,
    team = def_team
  )
  
  team_name <- freq_data %>% pull(team_name) %>% unique()
  primary <- freq_data %>% pull(team_color) %>% unique()
  secondary <- freq_data %>% pull(team_color2) %>% unique()
  third <- freq_data %>% pull(team_color3) %>% unique()
  
  # Process data for all teams
  tree_data <- freq_data %>%
    mutate(
      first_play = t_last_play,
      second_play = playType,
      x_root = ifelse(first_play == "Pass", 0.5, 2.5),
      x_end = case_when(
        first_play == "Pass" & second_play == "Pass" ~ 0,
        first_play == "Pass" & second_play == "Run" ~ 1,
        first_play == "Run" & second_play == "Pass" ~ 2,
        first_play == "Run" & second_play == "Run" ~ 3,
      ),
      y_root = 2,
      y_end = 1,
      # Compact labels for space efficiency
      edge_label = paste0(round(frequency * 100), "%"),
      first_epa_diff = first_epa_play - first_LA_epa_play,
      epa_diff = epa_play - second_LA_epa_play,
      # Create compact node labels
      root_label = paste0(first_play, "\nEPA: ", round(first_epa_play, 2)," (",first_epa_rk,")", 
                          "\nSR: ", round(first_SR * 100), "%", " (",first_SR_rk,")"),
      end_label = paste0(second_play, "\nEPA: ", round(epa_play, 2), " (",second_epa_rk,")",
                         "\nSR: ", round(SR * 100), "%", " (",second_SR_rk,")")
    )
  
  # Create the plot with facets
  p <- ggplot(tree_data) +
    # Background 
    annotate("rect", xmin = -1.75, xmax = 4.75, ymin = 0, ymax = 2.6,fill = third, alpha = .25) +
    # Caption
    annotate('text', label = 'bold("@arieizen | data: nflfastR")', x = 3.55, y = .45, size = 6.5, parse = TRUE) +
    # Team Wordmark
    geom_nfl_wordmarks( data = logo_data, aes(x = x, y = y, team_abbr = team), width = 0.75, alpha = 0.7) +
    # Frequency Connectors
    geom_segment( aes(x = x_root, y = y_root, xend = x_end, yend = y_end, linewidth = 5, color = frequency)) +
    # Frequency Label
    geom_label(
      aes(x = (x_root + x_end) / 2, y = (y_root + y_end) / 2, label = edge_label),
      size = 6.5, fontface = "bold", color = "black", fill = 'white') +
    # Root nodes (Pass/Run starting points)
    geom_label(
      data = tree_data %>% distinct(defteam, first_play, x_root, y_root, 
                                    first_epa_play, first_SR, first_epa_diff, root_label),
      aes(x = x_root, y = y_root, fill = first_epa_diff, label = root_label),
      size = 6.5, label.padding = unit(0.3, "lines"), 
      label.r = unit(0.2, "lines"), fontface = "bold"
    ) +
    # Outcome nodes (second play results)
    geom_label(
      aes(x = x_end, y = y_end, fill = epa_diff, label = end_label),
      size = 6.5,label.padding = unit(0.3, "lines"),
      label.r = unit(0.2, "lines"), fontface = "bold"
    ) +
    # Color scales
    scale_color_gradient2(
      low = '#3B4CC0', high = '#B40426', mid = "#DDDDDD", 
      midpoint = 0.5, name = "Frequency"
    ) +
    scale_fill_gradient2(
      high = '#3B4CC0', low = '#B40426', mid = "#DDDDDD", 
      name = "EPA vs League Avg"
    ) +
    
    # Labels and theming
    labs(
      title = paste0(team_name, ' Defensive Play Calling Tendancies'),
      subtitle = subtitle,
      # caption = "@arieizen | data: nflfastR"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 24,face = "bold",hjust = 0.5,color = primary),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 14,face = "bold"),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.spacing = unit(0.1, "lines"),
      panel.border = element_rect(colour = secondary, fill=NA, linewidth=2.5)
    ) +
    coord_cartesian(xlim = c(-0.75, 3.75),ylim = c(0.5, 2.5),clip = "off")
  
  return(p)
}

load_app_data <- function(){
  list(all_seq = read_csv('All Seq.csv', show_col_types = F),
       pbp = read_csv('NFL pbp.csv', show_col_types = F) %>% 
         mutate(Run = if_else(playType == 'Run',1,0)),
       full_data = read_csv('Full pbp.csv', show_col_types = F)
  )
}
# Define UI for application that draws a histogram
icon <- div(
  style = "position: absolute; top: 10px; right: 20px; 
           background-color: #E63946; color: white; 
           padding: 8px 15px; border-radius: 5px; 
           font-weight: bold; font-size: 12px; 
           box-shadow: 2px 2px 5px rgba(0,0,0,0.3);",
  "By: @AriEizen | Data: nflfastR/Wikipedia | Inspo: @reinhardNFL"
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
                               sliderInput("wp", "Win %:", 
                                           min = 0, max = 100, value = c(5,95))
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Down:", style = "font-size: 14px;"),
                                   checkboxGroupInput('down', NULL, 
                                                      choices = 1:4, 
                                                      selected = 1:4, inline = TRUE)
                               )
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Quarter:", style = "font-size: 14px;"),
                                   checkboxGroupInput('qtr', NULL, 
                                                      choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                                      selected = 1:5, inline = TRUE)
                               )
                        ),
                        column(2,
                               selectInput('order', 'Sort By:',
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
                                           selected = 'SR')
                        ),
                        column(2,
                               actionButton("apply_filters", 
                                            "Apply",
                                            class = "btn-primary",
                                            icon = icon("filter"),
                                            style = "width: 100%; height: 38px; margin-top: 25px; font-size: 15px; font-weight: bold;")
                        )
                      )
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
  # TAB #2: Offensive Play Calling Analysis
  #############################################################################
  tabPanel("Offensive Play Calling Analysis",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(2,
                               selectInput('tm', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(2,
                               sliderInput("week_2", "Week:", 
                                           min = 1, max = 22, value = c(1,18))
                        ),
                        column(2,
                               sliderInput("wp_2", "Win %:", 
                                           min = 0, max = 100, value = c(5,95))
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Down:", style = "font-size: 14px;"),
                                   checkboxGroupInput('down_2', NULL, 
                                                      choices = 1:4, 
                                                      selected = 1:4, inline = TRUE)
                               )
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Quarter:", style = "font-size: 14px;"),
                                   checkboxGroupInput('qtr_2', NULL, 
                                                      choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                                      selected = 1:5, inline = TRUE)
                               )
                        ),
                        column(2,
                               actionButton("apply_filters_2", 
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
             column(12, plotOutput('tree', height = "85vh"))
           )
  ),
  #############################################################################
  # TAB #3: Defensive Play Calling Analysis
  #############################################################################
  tabPanel("Defensive Play Calling Analysis",
           fluidRow(
             column(12,
                    wellPanel(
                      style = "background-color: #FFFFFF; border: 2px solid #E63946; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      h4("Filter Settings", style = "color: #E63946; margin-top: 0px; margin-bottom: 15px; font-size: 18px;"),
                      fluidRow(
                        column(2,
                               selectInput('t', 'Team', 
                                           choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                           selected = 'ARI')
                        ),
                        column(2,
                               sliderInput("week_3", "Week:", 
                                           min = 1, max = 22, value = c(1,18))
                        ),
                        column(2,
                               sliderInput("wp_3", "Win %:", 
                                           min = 0, max = 100, value = c(5,95))
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Down:", style = "font-size: 14px;"),
                                   checkboxGroupInput('down_3', NULL, 
                                                      choices = 1:4, 
                                                      selected = 1:4, inline = TRUE)
                               )
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Quarter:", style = "font-size: 14px;"),
                                   checkboxGroupInput('qtr_3', NULL, 
                                                      choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                                      selected = 1:5, inline = TRUE)
                               )
                        ),
                        column(2,
                               actionButton("apply_filters_3", 
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
             column(12, plotOutput('def_tend', height = "85vh"))
           )
  ),
  #############################################################################
  # TAB #4: Down and Distance Tendencies
  #############################################################################
  tabPanel("Play Tendencies",
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
                        column(2,
                               sliderInput("week_4", "Week:", 
                                           min = 1, max = 22, value = c(1,18))
                        ),
                        column(2,
                               sliderInput("wp_4", "Win %:", 
                                           min = 0, max = 100, value = c(5,95))
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Down:", style = "font-size: 14px;"),
                                   checkboxGroupInput('down_4', NULL, 
                                                      choices = 1:4, 
                                                      selected = 1:4, inline = TRUE)
                               )
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Distance:", style = "font-size: 14px;"),
                                   checkboxGroupInput('dist', NULL, 
                                                      choices = c('10+','10-7','6-4','3-1','GTG', '2PC'), 
                                                      selected = c('10+','10-7','6-4','3-1','GTG', '2PC'), inline = TRUE)
                               )
                        ),
                        column(2,
                               div(style = "margin-top: 5px;",
                                   strong("Quarter:", style = "font-size: 14px;"),
                                   checkboxGroupInput('qtr_4', NULL, 
                                                      choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5), 
                                                      selected = 1:5, inline = TRUE)
                               )
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
           fluidRow(column(6,
                           # div(style = "display: flex; flex-direction: column; height: 100%;",
                               plotOutput('pass_chart'#, height = "50%"
                                          ),
                               plotOutput('run_chart'#, height = "50%"
                                          )
                           # )
                           ),
                    column(6,gt_output('pbp_table')))
           )
  )
  #############################################################################

server <- function(input, output) {
  
  app_data <- load_app_data()
  
  # Action for Tab 1
  applied_overview <- reactiveValues(
    # week = seq(from = app_data$all_seq %>% pull(week) %>% min(),
    #            to = app_data$all_seq %>% pull(week) %>% max(),
    #            by = 1),
    week = c(app_data$all_seq %>% pull(week) %>% min(),
             app_data$all_seq %>% pull(week) %>% max()),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5
  )

  observeEvent(input$apply_filters, {
    applied_overview$week <- input$week
    applied_overview$wp <- input$wp
    applied_overview$down <- input$down
    applied_overview$qtr <- input$qtr
  })

  output$overview <- render_gt({
    
    t <- app_data$all_seq %>% 
      filter(between(week,as.numeric(min(applied_overview$week)),max(applied_overview$week)),
             between(wp,min(applied_overview$wp)/100,max(applied_overview$wp)/100),
             down %in% applied_overview$down,
             qtr %in% applied_overview$qtr)
    
    pbp <- app_data$pbp %>% 
      filter(between(week,as.numeric(min(applied_overview$week)),max(applied_overview$week)),
             between(wp,min(applied_overview$wp)/100,max(applied_overview$wp)/100),
             down %in% applied_overview$down,
             qtr %in% applied_overview$qtr)

    lg_avg <- calculate_league_averages(t,pbp)
    
    play_table <- seq_table(t, pbp) %>% 
      select(-logo) %>% 
      arrange(desc(.data[[input$order]])) %>%
      unique() %>% 
      mutate(rank = as.character(row_number()))
    
    table_split <- split_data_for_display(play_table) %>% 
      bind_rows(lg_avg) %>% 
      relocate(rank_2, .before = wordmark_2)
    
    tab_subtitle <- paste0("2025 Season • Weeks ", min(pbp$week), "-", max(pbp$week), 
                          " • Win Probability ", min(applied_overview$wp), "%-", max(applied_overview$wp), "%",
                          if(length(applied_overview$down) < 4) paste0(" • Downs: ", paste(applied_overview$down, collapse=", ")) else "",
                          if(length(applied_overview$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview$qtr), collapse=", ")) else "")
    
    # Option 1:
    # low_color = "#8E44AD"
    # high_color = "#27AE60"
    # Option 2:
    low_color = '#3B4CC0'
    high_color = '#B40426'
    mid_color = "#DDDDDD"

    table_split %>% 
      gt() %>% 
      # Move rank columns to start
      cols_move_to_start(rank) %>% 
      # Hide color and team abbreviation columns
      cols_hide(c(primary, secondary, tertiary, posteam,
                  primary_2, secondary_2, tertiary_2, posteam_2)) %>% 
      
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
      tab_spanner('Overall Stats', columns = c(SR, EPA)) %>%
      tab_spanner('Pass-Pass', columns = c(PP_SR, PP_EPA)) %>%
      tab_spanner('Pass-Run', columns = c(PR_SR, PR_EPA)) %>%
      tab_spanner('Run-Pass', columns = c(RP_SR, RP_EPA)) %>%
      tab_spanner('Run-Run', columns = c(RR_SR, RR_EPA)) %>%
      
      ## RIGHT COLUMN SPANNERS
      tab_spanner('Overall Stats ', columns = c(SR_2, EPA_2)) %>%
      tab_spanner('Pass-Pass ', columns = c(PP_SR_2, PP_EPA_2)) %>%
      tab_spanner('Pass-Run ', columns = c(PR_SR_2, PR_EPA_2)) %>%
      tab_spanner('Run-Pass ', columns = c(RP_SR_2, RP_EPA_2)) %>%
      tab_spanner('Run-Run ', columns = c(RR_SR_2, RR_EPA_2)) %>%
      
      # Column labels
      cols_label(
        # Left side
        PP_SR = 'SR', PP_EPA = 'EPA',
        PR_SR = 'SR', PR_EPA = 'EPA',
        RR_SR = 'SR', RR_EPA = 'EPA',
        RP_SR = 'SR', RP_EPA = 'EPA',
        SR = 'SR', EPA = 'EPA',
        wordmark = 'Team', rank = 'Rank',
        # Right side
        PP_SR_2 = 'SR', PP_EPA_2 = 'EPA',
        PR_SR_2 = 'SR', PR_EPA_2 = 'EPA',
        RR_SR_2 = 'SR', RR_EPA_2 = 'EPA',
        RP_SR_2 = 'SR', RP_EPA_2 = 'EPA',
        SR_2 = 'SR', EPA_2 = 'EPA',
        wordmark_2 = 'Team', rank_2 = 'Rank'
      ) %>% 
      
      # Format percentages
      fmt_percent(columns = c(SR, PP_SR, PR_SR, RP_SR, RR_SR, 
                              SR_2, PP_SR_2, PR_SR_2, RP_SR_2, RR_SR_2), 
                  decimals = 1) %>% 
      
      # COLOR CODING: Purple (bad) to Green (good) gradient
      # Left side color coding
      # Should I use rank for color or difference from lg average?
      data_color(columns = EPA, palette = c(low_color,mid_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR, palette = c(low_color,mid_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>% 
      data_color(columns = PP_EPA, palette = c(low_color,mid_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR, palette = c(low_color,mid_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_EPA, palette = c(low_color,mid_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR, palette = c(low_color,mid_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_EPA, palette = c(low_color,mid_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR, palette = c(low_color,mid_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_EPA, palette = c(low_color,mid_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR, palette = c(low_color,mid_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      
      # Right side color coding
      data_color(columns = EPA_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>%
      data_color(columns = PP_EPA_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_EPA_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_EPA_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_EPA_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR_2, palette = c(low_color,mid_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      
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
        source_note = md("**Analysis:** @AriEizen | **Data:** nflfastR | **Color Scale:** Blue (Poor Performance) → Red (Strong Performance)")
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
                                               RP_SR_2, RP_EPA_2, RR_SR_2, RR_EPA_2)) %>% 
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
                      RP_EPA_2, RP_SR_2, RR_EPA_2, RR_SR_2),
          rows = rank == "AVG"
        )
      )
    
  })
  
  # Offensive Play Calling Tendancies
  
  # Action for Tab 2
  applied_overview_2 <- reactiveValues(
    tm = 'ARI',
    week = c(1, 18),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5
  )
  
  observeEvent(input$apply_filters_2, {
    applied_overview_2$tm <- input$tm
    applied_overview_2$week <- input$week_2  
    applied_overview_2$wp <- input$wp_2      
    applied_overview_2$down <- input$down_2  
    applied_overview_2$qtr <- input$qtr_2 
  })
  
  output$tree <- renderPlot({
    
    tree_data <- app_data$pbp %>% 
      filter(between(week, as.numeric(min(applied_overview_2$week)), max(applied_overview_2$week)),
             between(wp, min(applied_overview_2$wp)/100, max(applied_overview_2$wp)/100),
             down %in% applied_overview_2$down,
             qtr %in% applied_overview_2$qtr)
    
    subtitle <- paste0("2025 Season • Weeks ", min(tree_data$week), "-", max(tree_data$week), 
                       " • Win Probability ", min(applied_overview_2$wp), "%-", max(applied_overview_2$wp), "%",
                       if(length(applied_overview_2$down) < 4) paste0(" • Downs: ", paste(applied_overview_2$down, collapse=", ")) else "",
                       if(length(applied_overview_2$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_2$qtr), collapse=", ")) else "")
    
    off_freq_data <- calculate_sequence_frequencies(tree_data, side = 'Off')
    tree_plot <- create_off_decision_trees(off_freq_data, subtitle, off_team = applied_overview_2$tm)
    
    tree_plot
  })

  # Defensive Play Calling Tendancies
  
  # Action for Tab 3
  applied_overview_3 <- reactiveValues(
    tm = 'ARI',
    week = c(1, 18),
    wp = c(5,95),
    down = 1:4,
    qtr = 1:5
  )
  
  observeEvent(input$apply_filters_3, {
    applied_overview_3$tm <- input$t
    applied_overview_3$week <- input$week_3
    applied_overview_3$wp <- input$wp_3      
    applied_overview_3$down <- input$down_3  
    applied_overview_3$qtr <- input$qtr_3
  })
  
  output$def_tend <- renderPlot({
    
    def_data <- app_data$pbp %>% 
      filter(between(week, as.numeric(min(applied_overview_3$week)), max(applied_overview_3$week)),
             between(wp, min(applied_overview_3$wp)/100, max(applied_overview_3$wp)/100),
             down %in% applied_overview_3$down,
             qtr %in% applied_overview_3$qtr)
    
    subtitle <- paste0("2025 Season • Weeks ", min(def_data$week), "-", max(def_data$week), 
                       " • Win Probability ", min(applied_overview_3$wp), "%-", max(applied_overview_3$wp), "%",
                       if(length(applied_overview_3$down) < 4) paste0(" • Downs: ", paste(applied_overview_3$down, collapse=", ")) else "",
                       if(length(applied_overview_3$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_3$qtr), collapse=", ")) else "")
    
  def_freq_data <- calculate_sequence_frequencies(def_data,side = 'Def')
  def_plot <- create_def_decision_trees(def_freq_data,subtitle, def_team = applied_overview_3$tm)
  
  def_plot
  })
  
  applied_overview_4 <- reactiveValues(
    tm = 'ARI',
    week = c(1, 18),
    wp = c(5,95),
    dist = c('10+','10-7','6-4','3-1','GTG', '2PC'),
    down = 1:4,
    qtr = 1:5
  )
  
  observeEvent(input$apply_filters_4, {
    applied_overview_4$tm <- input$t
    applied_overview_4$week <- input$week_4
    applied_overview_4$wp <- input$wp_4  
    applied_overview_4$dist <- input$dist  
    applied_overview_4$down <- input$down_4  
    applied_overview_4$qtr <- input$qtr_4
  })
  
  output$pbp_table <- render_gt({
    # print(applied_overview_4$dist)
    play_table <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm,
             between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             penalty == 0,
             playType %in% c("Run","Pass")) %>% 
      arrange(week, game_id, play_id)
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_4$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0("2025 Season • Weeks ", min(play_table$week), "-", max(play_table$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else "")
    
    # title_line <- 
    # print(play_table %>% head())
    play_table %>% 
      select(week, 
             posteam_wordmark, defteam_wordmark,
             # posteam_logo_1,defteam_logo_1,
             qtr, down,ydstogo, yrdln, yards_gained,playType, epa,desc) %>%  
      # head(20) %>% 
      gt() %>% 
      # opt_interactive(
      #   use_pagination = FALSE,
      #   use_search = TRUE,
      #   use_filters = TRUE,
      #   use_resizers = TRUE,
      #   use_highlight = TRUE,
      #   page_size_default = 20
      # ) %>% 
      cols_label(#posteam_logo_1 = 'Offense', defteam_logo_1 = 'Defense', 
                 qtr = 'Quarter',down = 'Down',ydstogo = 'To Go',yrdln = 'Yard Line',
                 yards_gained = 'Yards Gained',playType = 'Play Type', epa = 'EPA',
                 desc = 'Play Description', week = 'Week') %>% 
      cols_label(posteam_wordmark = 'Offense', defteam_wordmark = 'Defense') %>%
      tab_header(title = paste0(team_name, ' Play Table'), subtitle = subtitle) %>% 
      fmt_number(columns = epa) %>% 
      data_color(columns = epa, domain = c(-3,3), palette = c("#8E44AD", 'green')) %>%
      data_color(playType, palette = c('red','blue')) %>% 
      gt_img_rows(columns = posteam_wordmark) %>%
      gt_img_rows(columns = defteam_wordmark) 
      # opt_interactive(
      #   use_pagination = FALSE,
      #   use_search = TRUE,
      #   use_filters = TRUE,
      #   # use_resizers = TRUE,
      #   use_compact_mode = TRUE,
      #   use_highlight = TRUE,
      #   page_size_default = 20
      # )
      # gt_img_rows(columns = posteam_logo_1) %>%
      # gt_img_rows(columns = defteam_logo_1)
    
  })
  output$pass_chart <- renderPlot({
    pass_plays <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm,
             between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             pass == 1 & !is.na(pass_location))
    
    team_name <- teams_colors_logos %>% 
      filter(team_abbr == applied_overview_4$tm) %>% 
      pull(team_name)
    
    subtitle <- paste0("2025 Season • Weeks ", min(pass_plays$week), "-", max(pass_plays$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else "")
    
    pass <- pass_plays %>% 
      # Pass Location: Right, Middle, Left | Pass Length: Short, Middle
      mutate(depth_zone = case_when(
        is_screen_pass ~ "Screen",
        air_yards < 10 #& air_yards >= 0 
        ~ "Short (< 10)",
        air_yards >= 10 & air_yards < 20 ~ "Medium (10-20)",
        air_yards >= 20 ~ "Deep (20+)",
        TRUE ~ NA_character_),
        pass_location = case_when(
          pass_location == 'right' ~ 'Right',
          pass_location == 'middle' ~ 'Middle',
          pass_location == 'left' ~ 'Left',
          TRUE ~ pass_location
        )
      ) %>% 
      mutate(pass_location = factor(pass_location, levels = c('Left','Middle','Right')),
             depth_zone = factor(depth_zone, levels = c('Screen',"Short (< 10)","Medium (10-20)","Deep (20+)"))) %>% 
      group_by(pass_location, depth_zone, .drop = FALSE) %>% 
      reframe(plays = n(),
              sr = mean(success),
              epa = mean(epa)) %>% 
      mutate(label = paste0('EPA: ', round(epa, 2), ' | SR: ',percent(sr, accuracy = .1), '\n', plays, ' plays')) #%>% 
    # group_by(posteam) %>%
    # mutate(freq = plays/sum(plays))
    
    total_plays <- sum(pass$plays)
    
    ggplot(pass, aes(x = pass_location, y = depth_zone, fill = epa)) +
      geom_tile() +
      # geom_text(aes(label = label)) +
      # Vertical grid
      geom_segment(aes(x = 1.5, xend = 1.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = 2.5, xend = 2.5, y = 0.5,yend = 4.5), size = 1.5, color = 'white') +
      # Horizontal Grid
      geom_segment(aes(x = .5, xend = 3.5, y = 1.5,yend = 1.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 2.5,yend = 2.5), size = 1.5, color = 'white') +
      geom_segment(aes(x = .5, xend = 3.5, y = 3.5,yend = 3.5), size = 1.5, color = 'white') +
      scale_fill_continuous(low = "#8E44AD", high = 'green'#"#27AE60"
      ) +
      labs(
        title = paste0(team_name, ' Pass Plays | ',total_plays, ' plays'),
        subtitle = subtitle,
        caption = "Data: nflverse/nflfastR | @arieizen"
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#468944", color = NA),
        panel.background = element_rect(fill = "#468944", color = NA),
        panel.border = element_rect(colour = 'white', fill = NA, linewidth = 2.5),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text = element_text(color = 'white', face = 'bold', size = 10),
        legend.position = "none",
        # legend.direction = "horizontal",
        # legend.background = element_rect(fill = alpha("white", 0.9), color = "grey30"),
        # legend.title = element_text(face = "bold", size = 9),
        # legend.text = element_text(size = 8),
        # legend.key.height = unit(0.4, "cm"),
        # legend.key.width = unit(2, "cm"),
        # legend.margin = margin(8,8,8,8),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = 'white', 
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = 'white',
                                     margin = margin(b = 10)),
        plot.caption = element_text(face = "bold", size = 12, color = 'white',#hjust = 0.5, 
                                    margin = margin(t = 10, b = 5)),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      coord_cartesian(xlim = c(1.1,2.9), ylim = c(1.1,3.9)) #+
    # facet_wrap(~posteam, ncol = 8, nrow = 4)
    
    
  })
  output$run_chart <- renderPlot({
    rush_data <- app_data$full_data %>% 
      filter(posteam == applied_overview_4$tm,
             between(week, as.numeric(min(applied_overview_4$week)), max(applied_overview_4$week)),
             between(wp, min(applied_overview_4$wp)/100, max(applied_overview_4$wp)/100),
             down %in% applied_overview_4$down,
             qtr %in% applied_overview_4$qtr,
             dist %in% applied_overview_4$dist,
             rush == 1) 
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
    
    subtitle <- paste0("2025 Season • Weeks ", min(rush_data$week), "-", max(rush_data$week), 
                       " • Win Probability ", min(applied_overview_4$wp), "%-", max(applied_overview_4$wp), "%",
                       if(length(applied_overview_4$down) < 4) paste0(" • Downs: ", paste(applied_overview_4$down, collapse=", ")) else " • All Downs ",
                       if(length(applied_overview_4$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", applied_overview_4$qtr), collapse=", ")) else "")
    
    avg_epa <- mean(rush$epa)
    total_runs <- sum(rush$plays)
    # Create segment coordinates data frame
    segments_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      x_start = rep(3, 7),
      y_start = rep(0.98, 7),
      x_end = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      y_end = rep(1.002, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    # O-line positions
    oline <- data.frame(
      position = c('LT','LG','C','RG','RT'),
      x = 1:5,
      y = rep(1, 5)
    )
    
    # Create label coordinates data frame
    labels_data <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      x = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      # x = c(.85, 2.2, 2.55, 3, 3.4, 3.8, 5.1),
      # y = c(0.995, .99,.995,.99,.995,.99,.995)
      y = rep(1.0035, 7)
    ) %>%
      left_join(rush, by = 'gap_side') %>%
      mutate(label_text = paste0(round(epa,2),#percent(freq, accuracy = 0.1), 
                                 "\n(", percent(sr, accuracy = 0.1), " SR)"))
    
    # Create label coordinates data frame
    play_totals <- data.frame(
      gap_side = c('Left End', 'Left Tackle', 'Left Guard', 'Middle', 
                   'Right Guard', 'Right Tackle', 'Right End'),
      x = c(0.5, 1.5, 2.5, 3, 3.5, 4.5, 5.5),
      # x = c(.85, 2.2, 2.55, 3, 3.4, 3.8, 5.1),
      # y = c(0.995, .99,.995,.99,.995,.99,.995)
      y = rep(.979, 7)
    ) %>%
      left_join(rush, by = 'gap_side')
    
    # Create plot
    ggplot() +
      # Line of Scrimmage
      geom_segment(aes(x = -1, y = 1, xend = 7, yend = 1), 
                   linewidth = 2, lineend = 'butt', linejoin = 'bevel', 
                   color = 'white') +
      
      # Run direction arrows (consolidated into one geom_segment call)
      geom_segment(data = segments_data,
                   aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                       color = epa, linewidth = freq),
                   # linewidth = 3,
                   lineend = 'round', linejoin = 'round', 
                   arrow = arrow(type = 'closed',length = unit(0.3, 'inches'))) +
      
      # Dot to make lines look more round
      geom_point(aes(x = 3, y = .98), color = '#468944', size = 10) + 
      
      # O-line position labels
      geom_shadowtext(data = oline, aes(x = x, y = y, label = position), 
                      fontface = 'bold', size = 10, bg.color = 'grey10') +
      
      # Frequency and SR labels
      geom_label(data = labels_data, 
                 aes(x = x, y = y, label = label_text),
                 size = 4, lineheight = 0.9) +
      
      # Total Plays Labels
      geom_text(data = play_totals, 
                aes(x = x, y = y, label = plays),
                size = 10, lineheight = 0.9) +
      
      # Color scale: diverging around average SR
      scale_color_gradient2(low = "red", mid = "yellow", high = "#27AE60",
                            midpoint = avg_epa,
                            # breaks = c(min(rush_stats$sr), max(rush_stats$sr)),
                            # labels = percent_format(accuracy = 1),
                            name = "EPA/Play") +
      coord_cartesian(xlim = c(0, 6), ylim = c(0.975, 1.01)) +
      
      labs(
        title = paste0(team_name, ' Run Plays By Location | ', total_runs,' Plays'),
        subtitle = subtitle,
        # title = "NFL Run Location Tendencies",
        # subtitle = "Arrow color = EPA/Play | Label = Frequency (Success Rate)",
        caption = "Arrow color = EPA/Play | Label = EPA/Play (Success Rate) | Line Width = Frequency | Data: nflverse/nflfastR | @arieizen"
      ) +
      
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#468944", color = NA),
        panel.background = element_rect(fill = "#468944", color = NA),
        panel.border = element_rect(colour = 'white', fill = NA, linewidth = 2.5),
        # legend.position = c(0.15, 0.85),
        # legend.background = element_rect(fill = alpha("white", 0.9), color = "grey30"),
        # legend.title = element_text(face = "bold", size = 9),
        # legend.text = element_text(size = 8),
        # legend.key.height = unit(0.4, "cm"),
        # legend.key.width = unit(0.6, "cm"),
        legend.position = "none",
        # legend.direction = "horizontal",
        # legend.background = element_rect(fill = alpha("white", 0.9), color = "grey30"),
        # legend.title = element_text(face = "bold", size = 9),
        # legend.text = element_text(size = 8),
        # legend.key.height = unit(0.4, "cm"),
        # legend.key.width = unit(2, "cm"),
        # legend.margin = margin(8,8,8,8),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = 'white',
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = 'white',
                                     margin = margin(b = 10)),
        plot.caption = element_text(face = "bold", size = 12, color = 'white',#hjust = 0.5, 
                                    margin = margin(t = 10, b = 5)),
        plot.margin = margin(10, 10, 10, 10)
      ) #+
      # scale_y_reverse() + 
      # scale_x_reverse()
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
