
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(gam)
library(ggplot2)


##################
# Process raw data --------------------------------------------------------
##################

source("src/faceoffs/create-features.R")
sequences[]
sequences[, clock_begin := as.ITime(clock_begin, format = "%M:%S")]
sequences[, clock_end := as.ITime(clock_end, format = "%M:%S")]

source("src/faceoffs/functions/expandSequence.R")
sequences_exp <- rbindlist(lapply(1:nrow(sequences), function(k){
  expandSequence(sequences[k,], seq_id = k)
}))

# number of sequences that reach 60, 120 and 200 seconds
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 30]
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 60]
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 120]
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 200]

max_time <- 30
sequences_exp_off <- sequences_exp[faceoff_zone == "offense" & time_since_faceoff <= max_time]
sequences_exp_def <- sequences_exp[faceoff_zone == "defense" & time_since_faceoff <= max_time]


############
# Bar charts --------------------------------------------------------------
############

gg <- ggplot(sequences_exp_off[, .(y = mean(goal_for_scored)*100), .(time_since_faceoff, faceoff_won)],
       aes(x = time_since_faceoff, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = c(.875,.825), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("yellow", "green")) +
  coord_cartesian(xlim = c(0,max_time+1), ylim = c(0,1.2)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15")
gg
ggsave("report/figures/bar_off.png", gg, "png", width=4.5, height=3, units="in")

gg <- ggplot(sequences_exp_def[, .(y = mean(goal_against_scored)*100), .(time_since_faceoff, faceoff_won)],
       aes(x = time_since_faceoff, y = y, fill = faceoff_won)) +
  ggtitle("Goal against rate following defensive faceoff") +
  theme_light() +
  theme(legend.position = c(.875,.825), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "orange")) +
  coord_cartesian(xlim = c(0,max_time+1), ylim = c(0,1.2)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15")
gg
ggsave("report/figures/bar_def.png", gg, "png", width=4.5, height=3, units="in")


######################
# Fit model -- offense ----------------------------------------------------
######################
sequences_exp_off[, time_since_faceoff_F := log(time_since_faceoff + 1)*(faceoff_won == F)] 
sequences_exp_off[, time_since_faceoff_T := log(time_since_faceoff + 1)*(faceoff_won == T)] 

# span = .5
mod1 <- gam(formula = goal_for_scored ~ lo(time_since_faceoff_F, span=.5, degree=1) +
              lo(time_since_faceoff_T, span=.5, degree=1),
            weights = 1 + 100*(sequences_exp_off$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_off)
# plot(mod1)

# span = .75
mod2 <- gam(formula = goal_for_scored ~ lo(time_since_faceoff_F, span=.75, degree=1) +
              lo(time_since_faceoff_T, span=.75, degree=1),
            weights = 1 + 100*(sequences_exp_off$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_off)
# plot(mod2)

# span = 1
mod3 <- gam(formula = goal_for_scored ~ lo(time_since_faceoff_F, span=1, degree=1) +
              lo(time_since_faceoff_T, span=1, degree=1),
            weights = 1 + 100*(sequences_exp_off$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_off)
# plot(mod3)

####################################
# Get effect of variables -- offense --------------------------------------
####################################
ress <- lapply(list(mod1,mod2,mod3), function(mod){
  
  # create data tables for prediction
  res_F <- data.table(time_since_faceoff_F = log(rep(1:(max_time+1),times = 1)),
                      time_since_faceoff_T = rep(0,times = max_time + 1),
                      faceoff_won = as.factor(rep(FALSE, max_time + 1)))
  res_F <- cbind(res_F,
                 predict(mod,
                         newdata = res_F,
                         type = "response"),
                 predict(mod,
                         newdata = res_F,
                         type = "terms"))
  res_F[, time_since_faceoff_T := NULL]
  res_F[, 5] <- NULL
  names(res_F) <- c("time_since_faceoff", "faceoff_won", "p_mean", "f_mean")
  
  res_T <- data.table(time_since_faceoff_F = rep(0,times = max_time + 1),
                      time_since_faceoff_T = log(rep(1:(max_time + 1),times = 1)),
                      faceoff_won = as.factor(rep(TRUE, max_time + 1)))
  res_T <- cbind(res_T,
                 predict(mod,
                         newdata = res_T,
                         type = "response"),
                 predict(mod,
                         newdata = res_T,
                         type = "terms"))
  res_T[, time_since_faceoff_F := NULL]
  res_T[, 4] <- NULL
  names(res_T) <- c("time_since_faceoff", "faceoff_won", "p_mean", "f_mean")
  
  rbind(res_F,res_T)
})

gg <- ggplot(mapping=aes(x=exp(time_since_faceoff) - 1, y=p_mean*100, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.position = c(.85,.85), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("yellow", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.5)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line(data=ress[[2]]) +
  geom_line(data=ress[[1]], alpha = .3) +
  geom_line(data=ress[[3]], alpha = .3)
gg
ggsave("report/figures/curve_off.png", gg, "png", width=4.5, height=3, units="in")


# sequences_exp_off[, .(count = sum(goal_for_scored)), .(time_since_faceoff, faceoff_won)]
sequences[length_seconds < 30, .(goal_for = sum(result_goal_for), goal_against = sum(result_goal_against)), .(faceoff_zone)]


######################
# Fit model -- defense ----------------------------------------------------
######################
sequences_exp_def[, time_since_faceoff_F := log(time_since_faceoff + 1)*(faceoff_won == F)] 
sequences_exp_def[, time_since_faceoff_T := log(time_since_faceoff + 1)*(faceoff_won == T)] 

# span = .5
mod1 <- gam(formula = goal_against_scored ~ lo(time_since_faceoff_F, span=.5, degree=1) +
              lo(time_since_faceoff_T, span=.5, degree=1),
            weights = 1 + 100*(sequences_exp_def$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_def)
# plot(mod1)

# span = .75
mod2 <- gam(formula = goal_against_scored ~ lo(time_since_faceoff_F, span=.75, degree=1) +
              lo(time_since_faceoff_T, span=.75, degree=1),
            weights = 1 + 100*(sequences_exp_def$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_def)
# plot(mod2)

# span = 1
mod3 <- gam(formula = goal_against_scored ~ lo(time_since_faceoff_F, span=1, degree=1) +
              lo(time_since_faceoff_T, span=1, degree=1),
            weights = 1 + 100*(sequences_exp_def$time_since_faceoff == 0),
            family = binomial(),
            data = sequences_exp_def)
# plot(mod3)

####################################
# Get effect of variables -- offense --------------------------------------
####################################
ress <- lapply(list(mod1,mod2,mod3), function(mod){
  
  # create data tables for prediction
  res_F <- data.table(time_since_faceoff_F = log(rep(1:(max_time+1),times = 1)),
                      time_since_faceoff_T = rep(0,times = max_time + 1),
                      faceoff_won = as.factor(rep(FALSE, max_time + 1)))
  res_F <- cbind(res_F,
                 predict(mod,
                         newdata = res_F,
                         type = "response"),
                 predict(mod,
                         newdata = res_F,
                         type = "terms"))
  res_F[, time_since_faceoff_T := NULL]
  res_F[, 5] <- NULL
  names(res_F) <- c("time_since_faceoff", "faceoff_won", "p_mean", "f_mean")
  
  res_T <- data.table(time_since_faceoff_F = rep(0,times = max_time + 1),
                      time_since_faceoff_T = log(rep(1:(max_time + 1),times = 1)),
                      faceoff_won = as.factor(rep(TRUE, max_time + 1)))
  res_T <- cbind(res_T,
                 predict(mod,
                         newdata = res_T,
                         type = "response"),
                 predict(mod,
                         newdata = res_T,
                         type = "terms"))
  res_T[, time_since_faceoff_F := NULL]
  res_T[, 4] <- NULL
  names(res_T) <- c("time_since_faceoff", "faceoff_won", "p_mean", "f_mean")
  
  rbind(res_F,res_T)
})

gg <- ggplot(mapping=aes(x=exp(time_since_faceoff) - 1, y=p_mean*100, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal against rate following defensive faceoff") +
  theme(legend.position = c(.85,.85), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "orange")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.5)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line(data=ress[[2]]) +
  geom_line(data=ress[[3]], alpha = .3) +
  geom_line(data=ress[[2]], alpha = .3)
gg
ggsave("report/figures/curve_def.png", gg, "png", width=4.5, height=3, units="in")

# sequences_exp_def[, .(count = sum(goal_against_scored)), .(time_since_faceoff, faceoff_won)]
sequences[length_seconds < 30, .(goal_for = sum(result_goal_for), goal_against = sum(result_goal_against)), .(faceoff_zone)]

