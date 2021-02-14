library(data.table)

if (!file.exists("data/data.rds")) {
  source("data-raw/fetch-data.R", local = new.env())
}
data <- readRDS("data/data.rds")
data <- unique(data)

penalties <- data[event == "Penalty Taken", .(dataset, game_date, home_team, away_team, period,
                                              clock, home_team_goals, away_team_goals, team,
                                              detail_1)]

penalties <- penalties[!(period == 3L & clock == "0:00" & home_team_goals != away_team_goals)]
penalties <- penalties[!(detail_1 %in% c("Game Misconduct", "Misconduct", "Fighting"))]

penalties[, `:=`(
  simultaneous_penalty_nb = .N,
  home_nb = sum(team == home_team),
  away_nb = sum(team == away_team)
), .(dataset, game_date, home_team, away_team, period, clock)]

penalties <- penalties[home_nb != away_nb]

penalties[, index := c(1:.N), .(dataset, game_date, home_team, away_team, period, clock, team)]
penalties[, keep := index == max(index), .(dataset, game_date, home_team, away_team, period, clock)]
penalties <- penalties[keep == TRUE]

penalties[, `:=`(
  home_prior_penalty_nb = cumsum(team == home_team) - (team == home_team),
  away_prior_penalty_nb = cumsum(team == away_team) - (team == away_team)
), .(dataset, game_date, home_team, away_team)]

penalties[, `:=`(
  home_lead = home_team_goals - away_team_goals,
  prior_penalty_nb = home_prior_penalty_nb + away_prior_penalty_nb,
  home_prior_penalty_score = home_prior_penalty_nb - away_prior_penalty_nb
), .(dataset, game_date, home_team, away_team)]

penalties[, home_penalty := team == home_team]

drop <- c("game_date", "home_team", "away_team", "home_team_goals", "away_team_goals", "team",
          "detail_1", "simultaneous_penalty_nb", "home_nb", "away_nb", "index",
          "home_prior_penalty_nb", "away_prior_penalty_nb")
penalties[, (drop) := NULL]

penalties[, .(
  nb = .N,
  pr_next_penalty_home = sum(home_penalty == TRUE) / .N
), home_prior_penalty_score][order(home_prior_penalty_score)][nb > 5L]
