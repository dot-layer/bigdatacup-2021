# For running this code, you need the relevant data objects in data/, that is
# data/sequences_exp_yy.csv
# where yy is the number that appears in 20xx-yy
# as recorded in the data/nhl-2.csvy data (in local only).
# See the variable season_years

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
