library(data.table)

if (!file.exists("data/data.rds")) {
  source("data-raw/fetch-data.R", local = new.env())
}
data <- readRDS("data/data.rds")
setnames(data, "team", "event_team")

clock_seconds <- function(clock) {
  
  sapply(strsplit(clock, ":"), function(x) {60L * as.integer(x[1]) + as.integer(x[2])})
  
}

# Keep meaningful events and columns
sequences <- data[dataset == "scouting" & event %in% c("Faceoff Win", "Goal")]
sequences[, c("dataset", "x_coordinate_2", "y_coordinate_2", "player", "player_2") := NULL]

# Create team/opponent features as seen by the Erie Otters
sequences[, home := home_team == "Erie Otters"]
sequences[home == TRUE, `:=`(
  team = home_team,
  opponent = away_team,
  team_skaters_nb = home_team_skaters,
  opponent_skaters_nb = away_team_skaters,
  team_score = home_team_goals,
  opponent_score = away_team_goals
)]
sequences[home == FALSE, `:=`(
  team = away_team,
  opponent = home_team,
  team_skaters_nb = away_team_skaters,
  opponent_skaters_nb = home_team_skaters,
  team_score = away_team_goals,
  opponent_score = home_team_goals
)]
sequences[, c("home_team", "away_team", "home_team_skaters", "away_team_skaters",
              "home_team_goals", "away_team_goals") := NULL]

# Create faceoff features
sequences[event == "Faceoff Win" & event_team == "Erie Otters", faceoff_win := TRUE]
sequences[event == "Faceoff Win" & event_team != "Erie Otters", faceoff_win := FALSE]
sequences[faceoff_win == FALSE, `:=`(
  x_coordinate = 200L - x_coordinate,
  y_coordinate = 85L - y_coordinate
)]
sequences[event == "Faceoff Win" & x_coordinate == 31L, faceoff_zone := "defense"]
sequences[event == "Faceoff Win" & x_coordinate == 80L, faceoff_zone := "blue_defense"]
sequences[event == "Faceoff Win" & x_coordinate == 100L, faceoff_zone := "red"]
sequences[event == "Faceoff Win" & x_coordinate == 120L, faceoff_zone := "blue_offense"]
sequences[event == "Faceoff Win" & x_coordinate == 169L, faceoff_zone := "offense"]
sequences[event == "Faceoff Win" & y_coordinate %in% 20:21, faceoff_side := "left"]
sequences[event == "Faceoff Win" & y_coordinate %in% 42:43, faceoff_side := "center"]
sequences[event == "Faceoff Win" & y_coordinate %in% 64:65, faceoff_side := "right"]
sequences[event == "Faceoff Win", faceoff_win_grip := detail_1]
sequences[, c("x_coordinate", "y_coordinate", "detail_1", "detail_2", "detail_3",
              "detail_4") := NULL]

# Create targets
sequences[, `:=`(
  next_event_type = c(event[-1L], "End of Period"),
  next_event_team = c(event_team[-1L], NA_character_),
  next_even_clock = c(clock[-1L], NA_character_)
), .(game_date, period)]
sequences[event == "Faceoff Win", `:=`(
  result_goal_for = FALSE,
  result_goal_against = FALSE
)]
sequences[next_event_type == "Goal" & next_event_team == team, result_goal_for := TRUE]
sequences[next_event_type == "Goal" & next_event_team == opponent, result_goal_against := TRUE]
sequences[, c("event_team", "next_event_type", "next_event_team") := NULL]

# Final cleaning
sequences <- sequences[event == "Faceoff Win"]
setnames(sequences, "clock", "clock_begin")
sequences[, "event" := NULL]
sequences[, clock_end := c(clock_begin[-1L], "0:00"), .(game_date, period)]
sequences[period == 4L & (result_goal_for == TRUE | result_goal_against == TRUE),
          clock_end := next_even_clock]
sequences[, next_even_clock := NULL]
sequences[, length_seconds := clock_seconds(clock_begin) - clock_seconds(clock_end)]
setcolorder(sequences, c("game_date", "home", "period", "clock_begin", "clock_end",
                         "length_seconds"))

sequences[]
