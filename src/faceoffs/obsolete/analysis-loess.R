
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
  expandSequence(sequences[k,])
}))

# number of sequences that reach 60, 120 and 200 seconds
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 60]
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 120]
sequences_exp[faceoff_zone == "offense" & time_since_faceoff == 200]

max_time <- 60
sequences_exp <- sequences_exp[faceoff_zone == "offense" & time_since_faceoff <= max_time]


###########
# Fit model ---------------------------------------------------------------
###########
sequences_exp[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)] 
sequences_exp[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)] 

mod <- gam(formula = goal_scored ~ lo(time_since_faceoff_F, span=.35, degree=1) +
             lo(time_since_faceoff_T, span=.35, degree=1),
           # k = 15, fx = T),
           # k = length(kn), fx = T),
           # bs = "cr",
           # m = 1),
           # knots = list(x = kn),
           family = binomial(),
           data = sequences_exp[faceoff_zone == "offense"])
# plot(mod)


#########################
# Get effect of variables -------------------------------------------------
#########################

max_time <- 60
res_F <- data.table(time_since_faceoff_F = rep(0:max_time,times = 1),
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
                    time_since_faceoff_T = rep(0:max_time,times = 1),
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

res <- rbind(res_F,res_T)
ggplot(res, aes(x=time_since_faceoff,
                y=p_mean,
                col=faceoff_won)) +
  theme_light() +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.005)) +
  xlab("time since faceoff") +
  ylab("goal probability") +
  geom_line()
