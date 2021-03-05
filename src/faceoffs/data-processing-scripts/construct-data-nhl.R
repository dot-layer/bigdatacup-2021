##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
source("src/faceoffs/functions/expandSequenceNHL.R")


##################
# Process raw data --------------------------------------------------------
##################

seas <- 16:20
max_time <- 100

for(k in seq_along(seas)){
  
  cat("\n", "season ", seas[k], " of ", seas, "\n")
  
  if("sequences" %in% ls()) rm(sequences)
  if("sequences_exp" %in% ls()) rm(sequences_exp)
  sequences <- fread("data/nhl-2.csvy", header = T)
  
  # Choose seasons
  sequences <- sequences[as.integer(substr(season_years,6,7)) %in% seas[k]]
  
  # clean a couple things. a bit useless
  sequences[, faceoff_win := factor(faceoff_win)]
  sequences[, faceoff_zone := factor(faceoff_zone)]
  sequences[, period_type := factor(period_type)]
  sequences[, clock_begin := as.ITime(clock_begin, format = "%M:%S")]
  sequences[, clock_end := as.ITime(clock_end, format = "%M:%S")]
  
  # run the function expandSequenceNHL to get the expaded sequence
  sequences_exp <- rbindlist(lapply(1:nrow(sequences), function(k){
    if(k %% 5000 == 0) cat(paste0(round(k/nrow(sequences)*100,2),"% -- "))
    expandSequenceNHL(sequences[k], seq_id = k, max_time = max_time)
  }))
  fwrite(sequences_exp, paste0("data/sequences_exp_",seas[k],".csv"))
}
