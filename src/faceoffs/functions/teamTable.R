teamTable <- function(team, seas = 16:20){
  ###########
  # Load data ---------------------------------------------------------------
  ###########
  
  filenames <- paste0("data/sequences_exp_",seas,".csv")
  
  # function to avoid overloading the memory
  readReduce <- function(filename){
    sequences_exp_temp <- fread(filename)
    sequences_exp_temp[team_abbreviation == team]
  }
  
  sequences_exp <- readReduce(filenames[1])
  for(filename in filenames[-1]){
    sequences_exp <- rbind(sequences_exp, readReduce(filename))
  }
  sequences_exp[faceoff_zone %in% c("offense", "defense")]

  
  # bins offense
  bin_width <- 20
  sqo <- sequences_exp[faceoff_zone == "offense", .(goal_for_scored = sum(goal_for_scored), N = .N), .(time_since_faceoff, faceoff_won)]
  sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
  sqo <- sqo[, .(y = sum(goal_for_scored)/sum(N)*100, N = sum(N)), .(bin, faceoff_won)]
  sqo$team_abbreviation <- team
  
  # bins defense
  bin_width <- 20
  sqd <- sequences_exp[faceoff_zone == "defense", .(goal_against_scored = sum(goal_against_scored), N = .N), .(time_since_faceoff, faceoff_won)]
  sqd[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
  sqd <- sqd[, .(y = sum(goal_against_scored)/sum(N)*100, N = sum(N)), .(bin, faceoff_won)]
  sqd$team_abbreviation <- team
  
  ######################
  # Fit model -- offense ----------------------------------------------------
  ######################
  sequences_exp[, time_since_faceoff2 := time_since_faceoff] 
  sequences_exp[, time_since_faceoff2 := log(time_since_faceoff+1)] 

  sequences_exp[, time_since_faceoff_F := time_since_faceoff2*(faceoff_won == F)] 
  sequences_exp[, time_since_faceoff_T := time_since_faceoff2*(faceoff_won == T)] 
  
  mod <- gam(formula = goal_for_scored ~ s(time_since_faceoff_F, df=6) + s(time_since_faceoff_T, df=6),
             weights = 1 + 100*(sequences_exp[faceoff_zone == "offense"]$time_since_faceoff == 0),
             family = binomial(),
             data = sequences_exp[faceoff_zone == "offense"])
  
  # plot(mod)
  # predict(mod, newdata = sequences_exp_off[1:10,], type = "response")
  
  
  res <- predict(mod, type = "response", se.fit = T)
  res_table <- data.table(team_abbreviation = team,
                          faceoff_won = sequences_exp[faceoff_zone == "offense"]$faceoff_won,
                          time_since_faceoff = sequences_exp[faceoff_zone == "offense"]$time_since_faceoff,
                          mean = res$fit,
                          se = c(res$se.fit))
  res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
  
  rt0 <- res_table[,.(N = .N), .(faceoff_won, time_since_faceoff)]
  rt0[, prop := N/N[1]]
  res_table <- unique(res_table, by = c("faceoff_won", "time_since_faceoff"))
  res_table <- merge.data.table(res_table, rt0, by = c("faceoff_won", "time_since_faceoff"))
  
  res_table <- res_table[order(faceoff_won, time_since_faceoff)]
  res_table[,`:=`(mean2 = mean*prop,
                  lower2 = lower*prop,
                  upper2 = upper*prop)]
  
  res_table_off <- res_table


  
  ######################
  # Fit model -- defense ----------------------------------------------------
  ######################

  mod <- gam(formula = goal_against_scored ~ s(time_since_faceoff_F, df=6) + s(time_since_faceoff_T, df=6),
             weights = 1 + 100*(sequences_exp[faceoff_zone == "defense"]$time_since_faceoff == 0),
             family = binomial(),
             data = sequences_exp[faceoff_zone == "defense"])
  
  # plot(mod)
  # predict(mod, newdata = sequences_exp_off[1:10,], type = "response")
  
  
  res <- predict(mod, type = "response", se.fit = T)
  res_table <- data.table(team_abbreviation = team,
                          faceoff_won = sequences_exp[faceoff_zone == "defense"]$faceoff_won,
                          time_since_faceoff = sequences_exp[faceoff_zone == "defense"]$time_since_faceoff,
                          mean = res$fit,
                          se = c(res$se.fit))
  res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
  
  rt0 <- res_table[,.(N = .N), .(faceoff_won, time_since_faceoff)]
  rt0[, prop := N/N[1]]
  res_table <- unique(res_table, by = c("faceoff_won", "time_since_faceoff"))
  res_table <- merge.data.table(res_table, rt0, by = c("faceoff_won", "time_since_faceoff"))
  
  res_table <- res_table[order(faceoff_won, time_since_faceoff)]
  res_table[,`:=`(mean2 = mean*prop,
                  lower2 = lower*prop,
                  upper2 = upper*prop)]
  
  res_table_def <- res_table

  list(OZ_bin = sqo, DZ_bin = sqd,
       OZ_table = res_table_off, DZ_table = res_table_def)
}
