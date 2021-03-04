library(data.table)
library(tidynhl)

clock_seconds <- function(clock) {
  
  sapply(strsplit(clock, ":"), function(x) {60L * as.integer(x[1]) + as.integer(x[2])})
  
}

# Load data
schedules <- tidy_schedules(paste0(2010:2019, 2011:2020), playoffs = FALSE, keep_id = TRUE)

# Remove 26 outdoor games
schedules <- schedules[game_outdoor == FALSE]

events <- tidy_games_events(schedules[, game_id], time_elapsed = FALSE, keep_id = TRUE)
faceoffs <- tidy_games_faceoffs(schedules[, game_id], time_elapsed = FALSE, keep_id = TRUE)
goals <- tidy_games_goals(schedules[, game_id], time_elapsed = FALSE, keep_id = TRUE)

data <- events[event_type %in% c("faceoff", "goal") & period_type != "shootout"]

# Merge differents dimensions
cols <- c("faceoff_x", "faceoff_y", "winner_team_id", "loser_team_id")
data[faceoffs, (cols) := mget(cols), on = .(game_id, event_id)]

cols <- c("goal_strength", "for_team_id")
data[goals, (cols) := mget(cols), on = .(game_id, event_id)]

cols <- c("game_datetime", "season_years", "away_id", "home_id", "away_abbreviation",
          "home_abbreviation")
data[schedules, (cols) := mget(cols), on = .(game_id)]

# Duplicate rows
home <- copy(data)
home[, `:=`(
  team_id = home_id,
  team_abbreviation = home_abbreviation,
  opponent_id = away_id,
  opponent_abbreviation = away_abbreviation,
  team_status = "home"
)][, `:=`(
  away_id = NULL,
  away_abbreviation = NULL,
  home_id = NULL,
  home_abbreviation = NULL
)]

away <- copy(data)
away[, `:=`(
  team_id = away_id,
  team_abbreviation = away_abbreviation,
  opponent_id = home_id,
  opponent_abbreviation = home_abbreviation,
  team_status = "away"
)][, `:=`(
  away_id = NULL,
  away_abbreviation = NULL,
  home_id = NULL,
  home_abbreviation = NULL
)]

data <- rbindlist(list(home, away))

# Create faceoff features
data[team_id == winner_team_id, faceoff_win := TRUE]
data[team_id == loser_team_id, faceoff_win := FALSE]

data[team_status == "away" & faceoff_x > 25, faceoff_zone := "offense"]
data[team_status == "away" & faceoff_x > 0 & faceoff_x < 25, faceoff_zone := "blue_offense"]
data[team_status == "away" & faceoff_x == 0, faceoff_zone := "red"]
data[team_status == "away" & faceoff_x < 0 & faceoff_x > -25, faceoff_zone := "blue_defense"]
data[team_status == "away" & faceoff_x < -25, faceoff_zone := "defense"]
data[team_status == "away" & faceoff_y < 0, faceoff_side := "right"]
data[team_status == "away" & faceoff_y == 0, faceoff_side := "center"]
data[team_status == "away" & faceoff_y > 0, faceoff_side := "left"]

data[team_status == "home" & faceoff_x > 25, faceoff_zone := "defense"]
data[team_status == "home" & faceoff_x > 0 & faceoff_x < 25, faceoff_zone := "blue_defense"]
data[team_status == "home" & faceoff_x == 0, faceoff_zone := "red"]
data[team_status == "home" & faceoff_x < 0 & faceoff_x > -25, faceoff_zone := "blue_offense"]
data[team_status == "home" & faceoff_x < -25, faceoff_zone := "offense"]
data[team_status == "home" & faceoff_y < 0, faceoff_side := "left"]
data[team_status == "home" & faceoff_y == 0, faceoff_side := "center"]
data[team_status == "home" & faceoff_y > 0, faceoff_side := "right"]

data[, `:=`(
  winner_team_id = NULL,
  loser_team_id = NULL,
  faceoff_x = NULL,
  faceoff_y = NULL
)]

setorder(data, game_datetime, game_id, event_id, team_status)

# Create targets
data[, `:=`(
  next_event_type = rep(c(event_type[seq(1, .N, 2)][-1L], NA_character_), each = 2L),
  next_for_team_id = rep(c(for_team_id[seq(1, .N, 2)][-1L], NA_character_), each = 2L),
  next_clock = rep(c(period_time_remaining[seq(1, .N, 2)][-1L], NA_character_), each = 2L),
  result_goal_strength = rep(c(goal_strength[seq(1, .N, 2)][-1L], NA_character_), each = 2L)
), .(game_id, period_id)]
data[event_type == "faceoff", `:=`(
  result_goal_for = FALSE,
  result_goal_against = FALSE
)]
data[next_event_type == "goal" & next_for_team_id == team_id, result_goal_for := TRUE]
data[next_event_type == "goal" & next_for_team_id == opponent_id, result_goal_against := TRUE]

data[, `:=`(
  for_team_id = NULL,
  next_event_type = NULL,
  next_for_team_id = NULL,
  goal_strength = NULL
)]

# Final cleaning
sequences <- data[event_type == "faceoff"]
sequences[, event_type := NULL]
setnames(sequences, "period_time_remaining", "clock_begin")
sequences[, clock_end := rep(c(clock_begin[seq(1, .N, 2)][-1L], "00:00"), each = 2L),
          .(game_id, period_id)]
sequences[period_id == 4L & (result_goal_for == TRUE | result_goal_against == TRUE),
          clock_end := next_clock]
sequences[, next_clock := NULL]
sequences[, length_seconds := clock_seconds(clock_begin) - clock_seconds(clock_end)]

sequences[, `:=`(
  game_id = NULL,
  event_id = NULL,
  period_id = NULL,
  team_id = NULL,
  opponent_id = NULL
)]
setcolorder(sequences, c(
  "season_years", "game_datetime", "team_abbreviation", "opponent_abbreviation", "team_status",
  "period_label", "period_type", "clock_begin", "clock_end", "length_seconds", "faceoff_win",
  "faceoff_zone", "faceoff_side", "result_goal_for", "result_goal_against", "result_goal_strength"
))

fwrite(sequences, "data/nhl.csvy", yaml = TRUE)
