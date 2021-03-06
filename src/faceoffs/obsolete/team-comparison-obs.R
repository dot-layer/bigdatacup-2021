

# To perform this analysis, we needed to construct several new objects.
# For each team:
#   1. a table containing the offensive and defensive binned data
#   2. a table containing the results of gam models

# Those are created with the script src/faceoffs/table-creator-teams.R
# They are saved under the names
#   src/faceoffs/objects/bin_teams.csv
#   src/faceoffs/objects/res_teams.csv


###########
# Load data ---------------------------------------------------------------
###########

seas <- 20
filenames <- paste0("data/sequences_exp_",seas,".csv")

bin_width <- 20

# function to avoid overloading the memory
readReduce <- function(filename){
  sequences_exp_temp <- fread(filename)
  sequences_exp_temp <- sequences_exp_temp[faceoff_zone == "offense"]
  sequences_exp_temp[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
  sequences_exp_temp[, `:=`(N = .N, goal_for_scored = sum(goal_for_scored)), .(bin, faceoff_won, team_abbreviation, season_years2)]
  sequences_exp_temp[, prop := goal_for_scored/N]
  sequences_exp_temp
}

sequences_exp <- readReduce(filenames[1])
for(filename in filenames[-1]){
  sequences_exp <- rbind(sequences_exp, readReduce(filename))
}



#### THIS IS CRAP HERE. I NEED TO WORK THIS OUT

sequences_exp <- sequences_exp[bin == 10 & faceoff_won]
sequences_exp[season_years2 == 20][order(prop)]

y1 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "BUF"]$prop
N1 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "BUF"]$N

y2 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "NSH"]$prop
N2 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "NSH"]$N

# Number of goals
y2*N1 - y1*N1

# 185 GF ARI in 2019-2020
(y2*N1 - y1*N1)/195
