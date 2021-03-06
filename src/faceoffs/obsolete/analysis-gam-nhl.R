
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(gam)
library(ggplot2)


##################
# Process raw data --------------------------------------------------------
##################

sequences <- fread("data/nhl2.csvy", header = T)

range(which(as.integer(substr(sequences$season_years,6,7)) %in% c(16,17,18)))
sequences <- sequences[as.integer(substr(season_years,6,7)) %in% c(16,17,18)]
# sequences <- sequences[1:10000]
  
sequences[]
sequences[, faceoff_win := factor(faceoff_win, levels = c(FALSE,TRUE))]
sequences[, period_type := as.factor(period_type)]
sequences[, clock_begin := as.ITime(clock_begin, format = "%M:%S")]
sequences[, clock_end := as.ITime(clock_end, format = "%M:%S")]

max_time <- 60
source("src/faceoffs/functions/expandSequenceNHL.R")
sequences_exp <- rbindlist(lapply(1:nrow(sequences), function(k){
  if(k %% 5000 == 0) cat(paste0(round(k/nrow(sequences)*100,2),"% -- "))
  expandSequenceNHL(sequences[k], seq_id = k, max_time = max_time)
}))
fwrite(sequences_exp, "data/sequences_exp_16-18.csv")

# IMPORTANT -- FALSE, TRUE
levels(sequences_exp$faceoff_won)


# NOW TAKEN CARE OF IN LAPPLY ABOVE
# max_time <- 60 
# sequences_exp_off <- sequences_exp[faceoff_zone == "offense" & time_since_faceoff <= max_time]
# sequences_exp_def <- sequences_exp[faceoff_zone == "defense" & time_since_faceoff <= max_time]
sequences_exp_off <- sequences_exp[faceoff_zone == "offense"]
sequences_exp_def <- sequences_exp[faceoff_zone == "defense"]


############
# Bar charts --------------------------------------------------------------
############

sequences_exp_off[, .(y = mean(goal_for_scored)*100), .(time_since_faceoff, faceoff_won)]
sqo <- sequences_exp_off
sqo[, bin := as.integer(ceiling(time_since_faceoff/10)*10)]
sqo <- sqo[, .(y = mean(goal_for_scored)*100), .(bin, faceoff_won)]


gg <- ggplot(sqo,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-2,max_time+2), ylim = c(0,.3)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15")
gg
ggsave("report/figures/bar_off_nhl.png", gg, "png", width=4.5, height=3, units="in")


sequences_exp_def[, .(y = mean(goal_against_scored)*100), .(time_since_faceoff, faceoff_won)]
sqd <- sequences_exp_def
sqd[, bin := as.integer(ceiling(time_since_faceoff/10)*10)]
sqd <- sqd[, .(y = mean(goal_against_scored)*100), .(bin, faceoff_won)]

gg <- ggplot(sqd,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal against rate following defensive faceoff") +
  theme_light() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-2,max_time+2), ylim = c(0,.3)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15")
gg
ggsave("report/figures/bar_def_nhl.png", gg, "png", width=4.5, height=3, units="in")


######################
# Fit model -- offense ----------------------------------------------------
######################
# sequences_exp_off[, time_since_faceoff_F := log(time_since_faceoff + 1)*(faceoff_won == F)] 
# sequences_exp_off[, time_since_faceoff_T := log(time_since_faceoff + 1)*(faceoff_won == T)] 
sequences_exp_off[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)] 
sequences_exp_off[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)] 

mod_off <- gam(formula = goal_for_scored ~
                 # s(time_since_faceoff_F, df=5) + s(time_since_faceoff_T, df=5),
                 s(time_since_faceoff_F, df=10) + s(time_since_faceoff_T, df=10),
               # weights = 1 + 10*(sequences_exp_off$time_since_faceoff == 0),
               family = binomial(),
               data = sequences_exp_off)

plot(mod_off)

predict(mod_off, newdata = sequences_exp_off[1:10,], type = "response")

####################################
# Get effect of variables -- offense --------------------------------------
####################################
res <- predict(mod_off, type = "response", se.fit = T)
res_table <- data.table(faceoff_won = sequences_exp_off$faceoff_won,
                        time_since_faceoff = sequences_exp_off$time_since_faceoff,
                        mean = res$fit,
                        se = c(res$se.fit))
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)

gg
ggsave("report/figures/curve_off.png", gg, "png", width=4.5, height=3, units="in")


# sequences_exp_off[, .(count = sum(goal_for_scored)), .(time_since_faceoff, faceoff_won)]
sequences[length_seconds < 30, .(goal_for = sum(result_goal_for), goal_against = sum(result_goal_against)), .(faceoff_zone)]


######################
# Fit model -- defense ----------------------------------------------------
######################
# sequences_exp_def[, time_since_faceoff_F := log(time_since_faceoff + 1)*(faceoff_won == F)] 
# sequences_exp_def[, time_since_faceoff_T := log(time_since_faceoff + 1)*(faceoff_won == T)] 
sequences_exp_def[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)] 
sequences_exp_def[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)] 

mod_def <- gam(formula = goal_against_scored ~ 
                 # s(time_since_faceoff_F, df=5) + s(time_since_faceoff_T, df=5),
                 s(time_since_faceoff_F, df=5) + s(time_since_faceoff_T, df=10),
               weights = 1 + 10*(sequences_exp_def$time_since_faceoff == 0),
               family = binomial(),
               data = sequences_exp_def)

plot(mod_def)

predict(mod_def, newdata = sequences_exp_def[1:10,], type = "response")

####################################
# Get effect of variables -- defense --------------------------------------
####################################
res <- predict(mod_def, type = "response", se.fit = T)
res_table <- data.table(faceoff_won = sequences_exp_def$faceoff_won,
                        time_since_faceoff = sequences_exp_def$time_since_faceoff,
                        mean = res$fit,
                        se = c(res$se.fit))
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal against rate following defensive faceoff") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)

gg
ggsave("report/figures/curve_def.png", gg, "png", width=4.5, height=3, units="in")

# sequences_exp_def[, .(count = sum(goal_against_scored)), .(time_since_faceoff, faceoff_won)]
sequences[length_seconds < 30, .(goal_for = sum(result_goal_for), goal_against = sum(result_goal_against)), .(faceoff_zone)]

