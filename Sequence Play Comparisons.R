# rm(list = ls())
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
# setwd('~/Extracurricular/NFL Data/NFL Play Sequencing/')
##### Personnel Classifications
classify_personnel <- function(offense_personnel) {
  
  # Special teams personnel (punt, FG, kickoff) never lists a QB — the
  # punter/kicker takes the snap instead — so absence of QB is a reliable
  # signal to bucket these (and any other non-offensive-skill package,
  # e.g. all-defensive personnel lined up as a "hands team") as 'Other'
  # rather than misreading them as some flavor of "00p".
  has_qb <- str_detect(offense_personnel, "\\bQB\\b")
  
  rb_count <- str_extract(offense_personnel, "\\d+(?=\\s*RB)") %>%
    as.integer() %>% replace_na(0L)
  
  # A fullback lines up in the same backfield slot as a traditional
  # running back for personnel-grouping purposes (standard NFL
  # convention: "1 FB, 1 TE" is 11 personnel, same as "1 RB, 1 TE"),
  # so FB count folds into the same "backs" tally as RB.
  fb_count <- str_extract(offense_personnel, "\\d+(?=\\s*FB)") %>%
    as.integer() %>% replace_na(0L)
  
  te_count <- str_extract(offense_personnel, "\\d+(?=\\s*TE)") %>%
    as.integer() %>% replace_na(0L)
  
  backs <- rb_count + fb_count
  code <- paste0(backs, te_count, "p")
  
  # Only the 10 standard groupings are recognized; anything else (2-QB
  # trick formations, 0/4+ TE jumbo packages, 3-RB goal-line sets, etc.)
  # is genuinely rare and falls to 'Other' rather than cluttering the
  # filter with one-off codes.
  valid_groupings <- c('00p','01p','10p','11p','12p','13p',
                       '20p','21p','22p','23p')
  
  case_when(
    !has_qb ~ "Other",
    code %in% valid_groupings ~ code,
    TRUE ~ "Other"
  )
}
#####
###############################################################################
# Which seasons to pull. Normally just the current season, appending only the
# games we don't have yet. Setting REBUILD_SEASONS (e.g. "2025,2026") throws
# away everything stored for those seasons and rebuilds them from scratch --
# used to repair bad data (run from the workflow's "Run workflow" button).
rebuild_seasons <- as.integer(strsplit(Sys.getenv('REBUILD_SEASONS'), ',')[[1]])
rebuild_seasons <- rebuild_seasons[!is.na(rebuild_seasons)]
seasons <- if (length(rebuild_seasons) > 0) rebuild_seasons else most_recent_season()
message("Seasons: ", paste(seasons, collapse = ", "),
        if (length(rebuild_seasons) > 0) " (full rebuild)" else " (new games only)")

# guess_max = Inf: read_csv otherwise guesses each column's type from the
# first 1000 rows only. Rows from a new season (no personnel yet) sit at the
# top of the files, so offense_p got guessed as logical and every older
# "11p"-style value was silently read in as NA. season is re-derived from
# game_id so it can never come back blank. The personnel columns are forced
# to character because when they're entirely NA (e.g. a season before its
# participation data is published) they'd otherwise read as logical and
# refuse to bind_rows with the character values from new games.
read_data <- function(path, ...) {
  read_csv(path, show_col_types = F, guess_max = Inf, ...) |>
    mutate(season = as.integer(substr(game_id, 1, 4)),
           across(any_of(c('offense_p', 't_last_p', 't_next_p', 'offense_formation', 'offense_personnel')), as.character))
}

old_seq <- read_data('All Seq.csv')
old_epa <- read_data('NFL pbp.csv')
# Full pbp is stored gzipped -- as a plain CSV it's over GitHub's 100 MB limit
old_play <- read_data('Full pbp.csv.gz', col_types = cols(time = col_character()))

if (length(rebuild_seasons) > 0) {
  old_seq <- old_seq |> filter(!(season %in% rebuild_seasons))
  old_epa <- old_epa |> filter(!(season %in% rebuild_seasons))
  old_play <- old_play |> filter(!(season %in% rebuild_seasons))
}

#### 7/31/2026: The change I want to try and make is that only bring in the new weeks data
### What about looking at game IDs -- WORKED
existing_ids <- unique(old_epa$game_id)

# Loads one season at a time so a season that hasn't been published yet
# (e.g. participation for the current season returns a 404) doesn't wipe out
# the seasons that do exist.
load_safely <- function(loader, seasons) {
  map(seasons, \(s) tryCatch(suppressWarnings(loader(seasons = s)), error = function(e) NULL)) |>
    bind_rows()
}

raw_participation <- load_safely(load_participation, seasons)
raw_charting <- load_safely(load_ftn_charting, seasons)

# Personnel (participation) and FTN charting lag the play-by-play by days or
# weeks, so games stored before they were published have offense_p /
# is_screen_pass all NA. Once that data shows up, pull those games again.
missing_personnel <- old_epa |> group_by(game_id) |> filter(all(is.na(offense_p))) |> pull(game_id) |> unique()
missing_charting <- old_play |> group_by(game_id) |> filter(all(is.na(is_screen_pass))) |> pull(game_id) |> unique()
refresh_ids <- union(intersect(missing_personnel, raw_participation$nflverse_game_id),
                     intersect(missing_charting, raw_charting$nflverse_game_id))
if (length(refresh_ids) > 0) message("Refreshing ", length(refresh_ids), " games that now have personnel/charting data")
existing_ids <- setdiff(existing_ids, refresh_ids)

new_pbp <- load_pbp(seasons) |>
  filter(!(game_id %in% existing_ids))

if (nrow(new_pbp) == 0) {
  message("No new games since the last update -- data files left unchanged.")
} else {
  message("Adding ", n_distinct(new_pbp$game_id), " games")

  ##### Personnel Classifications
  if (nrow(raw_participation) == 0) {
    message("No participation data available yet for ", paste(seasons, collapse = ", "), " -- personnel will be filled in on a later run.")
    formations <- tibble(
      game_id = character(),
      play_id = numeric(),
      offense_formation = character(),
      offense_personnel = character(),
      offense_p = character()
    )
  } else {
    formations <- raw_participation |>
      filter(!(nflverse_game_id %in% existing_ids)) |>
      mutate(offense_p = classify_personnel(offense_personnel)) |>
      select(nflverse_game_id, play_id, offense_formation, offense_personnel, offense_p) |>
      rename(game_id = nflverse_game_id)
  }

  play_by_play <- new_pbp |>
    clean_pbp() %>%
    select(game_id,week,season,season_type,posteam,defteam,drive,play_id,qtr,down,ydstogo,
           goal_to_go,wp,play_type,play_type_nfl,desc,epa,success,yards_gained,
           pass,rush,pass_attempt,incomplete_pass,qb_scramble,rusher_player_id,
           penalty,penalty_team,penalty_yards,touchdown,field_goal_attempt,
           field_goal_result,extra_point_result,two_point_conv_result,
           safety,sack,interception,fumble,fumble_lost,fourth_down_failed,
           punt_attempt,punt_blocked) |>
    # Bring in formation data
    merge(formations, by = c('game_id','play_id'), all.x = T)


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
    select(game_id,week,season,season_type,posteam,defteam,qtr,down,drive_no, play_no,
           play_type,playType,desc, epa, success, wp, offense_p) %>%
    arrange(game_id,posteam,drive_no,play_no) %>%
    group_by(game_id,posteam,drive_no) %>%
    mutate(t_last_play = lag(playType),
           t_next_play = lead(playType),
           t_last_down = lag(down),
           t_next_down = lead(down),
           t_last_p = lag(offense_p),
           t_next_p = lead(offense_p),
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


  if (nrow(raw_charting) == 0) {
    message("No charting data available yet for ", paste(seasons, collapse = ", "), " -- will be filled in on a later run.")
    charting_data <- tibble(
      nflverse_game_id = character(),
      nflverse_play_id = numeric(),
      is_catchable_ball = logical(),
      is_screen_pass = logical()
    )
  } else {
    charting_data <- raw_charting %>%
      filter(!(nflverse_game_id %in% existing_ids)) |>
      select(-week, -season)
  }


  play_data <- new_pbp %>%
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
    ),
    line = as.numeric(line)
    ) |>
    merge(formations,
          by = c('game_id','play_id'),
          all.x = T)

  # Append New Data to the games we already had
  all_seq <- all_seq |> bind_rows(old_seq |> filter(game_id %in% existing_ids)) |> unique()
  seq_epa <- seq_epa |> bind_rows(old_epa |> filter(game_id %in% existing_ids)) |> unique()
  play_data <- play_data |> bind_rows(old_play |> filter(game_id %in% existing_ids)) |> unique()

  all_seq %>% write_csv('All Seq.csv')
  seq_epa %>% write_csv('NFL pbp.csv')
  play_data %>% write_csv('Full pbp.csv.gz')

  # Error checking to ensure each file was created successfully
  outputs <- list('All Seq.csv' = all_seq, 'NFL pbp.csv' = seq_epa, 'Full pbp.csv.gz' = play_data)
  for (f in names(outputs)) {
    if (!file.exists(f)) stop("❌ Failed to create ", f)
    cat("✅", f, "created successfully at", as.character(Sys.time()), "\n")
    cat("📊 File size:", round(file.size(f) / 1024, 2), "KB\n")
    cat("📈 Rows in dataset:", nrow(outputs[[f]]), "\n")
  }
}
