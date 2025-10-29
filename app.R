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
    annotate("rect", xmin = -1.75, xmax = 4.75, ymin = 0, ymax = 2.6,fill = third, alpha = .25) +
    # Caption
    annotate('text', label = 'bold("@arieizen | data: nflfastR")', x = 3.65, y = .45, size = 6.5, parse = TRUE) +
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

load_app_data <- function(){
  list(all_seq = read_csv('All Seq.csv', show_col_types = F),
       pbp = read_csv('NFL pbp.csv', show_col_types = F) %>% 
         mutate(Run = if_else(playType == 'Run',1,0))
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
  title = "NFL Draft Trends & Insights",
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
  ),
  tabPanel("Main Dashboard",
    fluidRow(
      column(2,sliderInput("week", "Week Number:", min = 1, max = 22, value = c(1,18))),
      column(2,sliderInput("wp", "Team Win %:", min = 0, max = 100, value = c(20,80))),
      column(1,checkboxGroupInput('down', 'Down:', choices = 1:4, selected = 1:4, inline = T)),
      column(1,checkboxGroupInput('qtr', 'Quarter:', choices = c(1,2,3,4,'OT' = 5), selected = 1:5, inline = T)),
      column(6,selectInput('order', 'Order Data:',
                   choices = c('Overall Success Rate (SR)' = 'SR', 'Overall EPA/Play' = 'EPA',
                               'Pass-Pass SR' = 'PP_SR', 'Pass-Pass EPA' = 'PP_EPA',
                               'Pass-Run SR' = 'PR_SR', 'Pass-Run EPA' = 'PR_EPA',
                               'Run-Pass SR' = 'RP_SR', 'Run-Pass EPA' = 'RP_EPA',
                               'Run-Run SR' = 'RR_SR', 'Run-Run EPA' = 'RR_EPA'),
                   selected = 'SR'#,
                   # inline = T
                   ))
      
    ),

        # Show a plot of the generated distribution
        fluidRow(
           # DTOutput("raw_data")
          column(12,gt_output('overview'))
        )
    ),
  tabPanel("Play Calling Analysis",
           fluidRow(
             column(2,selectInput('tm', 'Select Team', 
                                  choices = unique(teams_colors_logos$team_abbr)[c(-19,-27,-30,-33)],
                                  selected = 'ARI')),
             column(2,sliderInput("week_2", "Week Number:", min = 1, max = 22, value = c(1,18))),
             column(2,sliderInput("wp_2", "Team Win %:", min = 0, max = 100, value = c(20,80))),
             column(1,checkboxGroupInput('down_2', 'Down:', choices = 1:4, selected = 1:4, inline = T)),
             column(1,checkboxGroupInput('qtr_2', 'Quarter:', choices = c(1,2,3,4,'OT' = 5), selected = 1:5, inline = T))
           ),
           fluidRow(
          column(12,plotOutput('tree', height = "85vh"))
        )
      )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  app_data <- load_app_data()
  # print(app_data %>% head() %>% as.data.frame())
  # Stats Overview GT
  output$overview <- render_gt({
    
    t <- app_data$all_seq %>% 
      filter(between(week,as.numeric(min(input$week)),max(input$week)),
             between(wp,min(input$wp)/100,max(input$wp)/100),
             down %in% input$down,
             qtr %in% input$qtr)
    
    pbp <- app_data$pbp %>% 
      filter(between(week,as.numeric(min(input$week)),max(input$week)),
             between(wp,min(input$wp)/100,max(input$wp)/100),
             down %in% input$down,
             qtr %in% input$qtr)
    
    lg_avg <- calculate_league_averages(t,p)
    
    play_table <- seq_table(t, pbp) %>% 
      select(-logo) %>% 
      arrange(desc(.data[[input$order]])) %>%
      unique() %>% 
      mutate(rank = as.character(row_number()))
    
    table_split <- split_data_for_display(play_table) %>% 
      bind_rows(lg_avg) %>% 
      relocate(rank_2, .before = wordmark_2)
    
    tab_subtitle <- paste0("2025 Season • Weeks ", min(pbp$week), "-", max(pbp$week), 
                          " • Win Probability ", min(input$wp), "%-", max(input$wp), "%",
                          if(length(input$down) < 4) paste0(" • Downs: ", paste(input$down, collapse=", ")) else "",
                          if(length(input$qtr) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", input$qtr), collapse=", ")) else "")
    
    # Option 1:
    low_color = "#8E44AD"
    high_color = "#27AE60"
    # Option 2:
    # low_color = '#3B4CC0'
    # high_color = '#B40426'
    # mid_color = "#DDDDDD"

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
      data_color(columns = EPA, palette = c(low_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR, palette = c(low_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>% 
      data_color(columns = PP_EPA, palette = c(low_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR, palette = c(low_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_EPA, palette = c(low_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR, palette = c(low_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_EPA, palette = c(low_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR, palette = c(low_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_EPA, palette = c(low_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR, palette = c(low_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      
      # Right side color coding
      data_color(columns = EPA_2, palette = c(low_color, high_color), domain = range(play_table$EPA, na.rm = TRUE)) %>% 
      data_color(columns = SR_2, palette = c(low_color, high_color), domain = range(play_table$SR, na.rm = TRUE)) %>%
      data_color(columns = PP_EPA_2, palette = c(low_color, high_color), domain = range(play_table$PP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PP_SR_2, palette = c(low_color, high_color), domain = range(play_table$PP_SR, na.rm = TRUE)) %>% 
      data_color(columns = PR_EPA_2, palette = c(low_color, high_color), domain = range(play_table$PR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = PR_SR_2, palette = c(low_color, high_color), domain = range(play_table$PR_SR, na.rm = TRUE)) %>% 
      data_color(columns = RP_EPA_2, palette = c(low_color, high_color), domain = range(play_table$RP_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RP_SR_2, palette = c(low_color, high_color), domain = range(play_table$RP_SR, na.rm = TRUE)) %>% 
      data_color(columns = RR_EPA_2, palette = c(low_color, high_color), domain = range(play_table$RR_EPA, na.rm = TRUE)) %>% 
      data_color(columns = RR_SR_2, palette = c(low_color, high_color), domain = range(play_table$RR_SR, na.rm = TRUE)) %>% 
      
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
  
  # Play Calling Tendancies
  output$tree <- renderPlot({
    
    tree_data <- app_data$pbp %>% 
      filter(between(week,as.numeric(min(input$week_2)),max(input$week_2)),
             between(wp,min(input$wp_2)/100,max(input$wp_2)/100),
             down %in% input$down_2,
             qtr %in% input$qtr_2)
    
    subtitle <- paste0("2025 Season • Weeks ", min(tree_data$week), "-", max(tree_data$week), 
                       " • Win Probability ", min(input$wp_2), "%-", max(input$wp_2), "%",
                       if(length(input$down_2) < 4) paste0(" • Downs: ", paste(input$down_2, collapse=", ")) else "",
                       if(length(input$qtr_2) < 5) paste0(" • Qtrs: ", paste(gsub("5", "OT", input$qtr_2), collapse=", ")) else "")
    
    off_freq_data <- calculate_sequence_frequencies(tree_data,side = 'Off')
    tree_plot <- create_off_decision_trees(off_freq_data,subtitle, off_team = input$tm)
    
    tree_plot
  })
  
  # Table on Drill In Page
################################################################################  
  # # Should show epa/play and SR for both first play and second play of selected
  # output$raw_data <- renderDT({
  #   
  #   seq_table <- function(){
  #     t <- read_csv('All Seq.csv') %>% 
  #       filter(between(week,as.numeric(min(input$week_2)),max(input$week_2)),
  #              between(wp,min(input$wp_2)/100,max(input$wp_2)/100),
  #              down %in% input$down_2,
  #              qtr %in% input$qtr_2,
  #              ((playType == input$first & t_next_play == input$second) |
  #              (t_last_play == input$first & playType == input$second)))
  #     
  #     seq_chart <- t %>% 
  #       group_by(posteam,seq_group) %>% 
  #       reframe(epa_per_play = mean(epa),
  #               success_rate = mean(success)) %>% 
  #       merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3, team_wordmark,team_logo_wikipedia),
  #             by.x = 'posteam',by.y = 'team_abbr') %>% 
  #       unique()
  #     
  #     whole_totals <- t %>% 
  #       group_by(posteam) %>% 
  #       mutate(total_epa = mean(epa),
  #              total_sr = mean(success)) 
  #     
  #     
  #     # print(seq_chart)
  #     new_table <- c()
  #     for (k in 1:32) {
  #       tms <- seq_chart %>% pull(posteam) %>% unique()
  #       tm <- tms[k]
  #       primary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color) %>% unique() 
  #       secondary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color2) %>% unique() 
  #       tertiary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color3) %>% unique()
  #       wordmark <- seq_chart %>% filter(posteam == tm) %>% pull(team_wordmark) %>% unique()
  #       logo <- seq_chart %>% filter(posteam == tm) %>% pull(team_logo_wikipedia) %>% unique()
  #       
  #       
  #       # Get Pass-Pass Metrics
  #       SR <- whole_totals %>% 
  #         filter(posteam == tm) %>% 
  #         pull(total_sr) #%>% 
  #       # percent(accuracy = 0.1)
  #       EPA <- whole_totals %>% 
  #         filter(posteam == tm) %>% 
  #         pull(total_epa) %>% 
  #         round(3)
  #       
  #       # Get Pass-Pass Metrics
  #       PP_SR <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
  #         pull(success_rate) #%>% 
  #       # percent(accuracy = 0.1)
  #       PP_EPA <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
  #         pull(epa_per_play) %>% 
  #         round(3)
  #       
  #       # Get Pass-Run Metrics
  #       PR_SR <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Pass-Run') %>% 
  #         pull(success_rate) #%>% 
  #       # percent(accuracy = 0.1)
  #       PR_EPA <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Pass-Run') %>% 
  #         pull(epa_per_play) %>% 
  #         round(3)
  #       
  #       # Get Run-Pass Metrics
  #       RP_SR <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Run-Pass') %>% 
  #         pull(success_rate) #%>% 
  #       # percent(accuracy = 0.1)
  #       RP_EPA <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Run-Pass') %>% 
  #         pull(epa_per_play) %>% 
  #         round(3)
  #       
  #       # Get Run-Run Metrics
  #       RR_SR <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Run-Run') %>% 
  #         pull(success_rate) #%>% 
  #       # percent(accuracy = 0.1)
  #       RR_EPA <- seq_chart %>% 
  #         filter(posteam == tm & seq_group == 'Run-Run') %>% 
  #         pull(epa_per_play) %>% 
  #         round(3)
  #       
  #       team_row <- data.frame(
  #         posteam = tm,
  #         primary, 
  #         secondary,
  #         tertiary,
  #         wordmark,
  #         logo,
  #         EPA,
  #         SR,
  #         PP_EPA,
  #         PP_SR,
  #         PR_EPA,
  #         PR_SR,
  #         RP_EPA,
  #         RP_SR,
  #         RR_EPA,
  #         RR_SR
  #       )
  #       
  #       new_table <- bind_rows(new_table, team_row)
  #     }
  #     
  #     return(new_table)
  #   }
  #   
  #   dt <- seq_table() %>% 
  #     rename(
  #       'Pass-Pass EPA/Play'= PP_EPA,
  #       'Pass-Pass Success Rate'= PP_SR,
  #       'Pass-Run EPA/Play'= PR_EPA,
  #       'Pass-Run SR'= PR_SR,
  #       'Run-Pass EPA/Play'= RP_EPA,
  #       'Run-Pass SR'= RP_SR,
  #       'Run-Run EPA/Play'= RR_EPA,
  #       'Run-Run SR'= RR_SR,
  #     ) %>% 
  #     select(-posteam,-primary,-secondary,-tertiary,-wordmark) %>% 
  #     gt() %>% 
  #     
  #   
  #   return(dt)
  #   
  # 
  # },escape = FALSE, options = list(pageLength = 10)) 
################################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)
