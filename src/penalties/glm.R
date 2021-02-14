
#################
# Attach packages ---------------------------------------------------------
#################

library(data.table)


###########
# Load data ---------------------------------------------------------------
###########

if (!file.exists("data/data.rds")) {
  source("data-raw/fetch-data.R", local = new.env())
}
data <- readRDS("data/data.rds")
data <- unique(data)


##################
# Pre-process data --------------------------------------------------------
##################
# edit from JayP's script

penalties <- data[event == "Penalty Taken" & dataset == "scouting",
                  .(game_date, home_team, away_team, period,
                    clock, home_team_goals, away_team_goals,
                    team, detail_1)]

penalties <- penalties[!(period == 3L & clock == "0:00" & home_team_goals != away_team_goals)]
penalties <- penalties[!(detail_1 %in% c("Game Misconduct", "Misconduct", "Fighting"))]

penalties[, `:=`(
  simultaneous_penalty_nb = .N,
  erie_home = (home_team == "Erie Otters"), 
  home_nb = sum(team == home_team),
  away_nb = sum(team == away_team)
), .(game_date, home_team, away_team, period, clock)]

# penalties <- penalties[home_nb != away_nb]

penalties[, index := c(1:.N), .(game_date, home_team, away_team, period, clock, team)]
penalties[, keep := index == max(index), .(game_date, home_team, away_team, period, clock)]
penalties <- penalties[keep == TRUE]

penalties[, `:=`(
  home_prior_penalty_nb = cumsum(team == home_team) - (team == home_team),
  away_prior_penalty_nb = cumsum(team == away_team) - (team == away_team)
), .(game_date, home_team, away_team)]

penalties[, `:=`(
  home_lead = home_team_goals - away_team_goals,
  prior_penalty_nb = home_prior_penalty_nb + away_prior_penalty_nb,
  home_prior_penalty_score = home_prior_penalty_nb - away_prior_penalty_nb
), .(game_date, home_team, away_team)]

penalties[, home_penalty := team == home_team]

# drop <- c("game_date", "home_team", "away_team", "home_team_goals", "away_team_goals", "team",
#           "detail_1", "simultaneous_penalty_nb", "home_nb", "away_nb", "index",
#           "home_prior_penalty_nb", "away_prior_penalty_nb")
# penalties[, (drop) := NULL]

penalties[, .(
  nb = .N,
  pr_next_penalty_home = sum(home_penalty == TRUE) / .N
), home_prior_penalty_score][order(home_prior_penalty_score)][nb > 5L]

penalties[, erie_penalty := (home_penalty & erie_home) | (!home_penalty & !erie_home)]
penalties[, erie_penalty_score := (1 - 2*(!erie_home))*home_prior_penalty_score]
penalties[, erie_score := (1 - 2*(!erie_home))*(home_team_goals - away_team_goals)]


###########
# Fit model ---------------------------------------------------------------
###########

# basic model
glm_fit <- glm(erie_penalty ~ erie_penalty_score,
               family = binomial(),
               data = penalties)
summary(glm_fit)
hist(glm_fit$residuals)

# basic model
glm_fit <- glm(erie_penalty ~ erie_penalty_score + erie_home + erie_score,
               family = binomial(),
               data = penalties)
summary(glm_fit)


pen2 <- penalties[, .(mean_erie_penalty = mean(erie_penalty)), .(erie_home, erie_score)]
table(penalties[, c("erie_home", "erie_score")])
