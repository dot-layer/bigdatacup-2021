
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


###########
# Fit model ---------------------------------------------------------------
###########

# manual placing of knots
# kn <- c(seq(0,100,length.out=20),
#         seq(100,200,length.out=10)[-1],
#         seq(200,600,length.out=10)[-1])
# kn <- c(seq(0,30,5),30,45,seq(60,540,60))
kn <- c(1,5,10,15,30,45,60,120,360)

# Generalized additive model
# mod <- gam(formula = goal_scored ~ lo(time_since_faceoff, by = faceoff_won),
#                                      # k = 15, fx = T),
#                                      # k = length(kn), fx = T),
#            # bs = "cr",
#            # m = 1),
#            knots = list(x = kn),
#            family = binomial(),
#            data = sequences_exp[faceoff_zone == "offense"])
# 

# mod <- gam(formula = goal_scored ~ lo(time_since_faceoff, span=.5, degree=1),
#            # k = 15, fx = T),
#            # k = length(kn), fx = T),
#            # bs = "cr",
#            # m = 1),
#            knots = list(x = kn),
#            family = binomial(),
#            data = sequences_exp[faceoff_zone == "offense"])


sequences_exp[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)] 
sequences_exp[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)] 

mod <- gam(formula = goal_scored ~ lo(time_since_faceoff_F, span=.1, degree=1) +
             lo(time_since_faceoff_T, span=.1, degree=1),
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

max_time <- 300
res_F <- data.table(time_since_faceoff_F = rep(1:max_time,times = 1),
                    time_since_faceoff_T = rep(0,times = max_time),
                    faceoff_won = as.factor(rep(FALSE, max_time)))
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

res_T <- data.table(time_since_faceoff_F = rep(0,times = max_time),
                    time_since_faceoff_T = rep(1:max_time,times = 1),
                    faceoff_won = as.factor(rep(TRUE, max_time)))
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
  coord_cartesian(xlim = c(0,120), ylim = c(0,.005)) +
  xlab("time since faceoff") +
  ylab("goal probability") +
  geom_line()

# # Effects directly. Not sure I can read this properly
# ggplot(res, aes(x=time_since_fo,
#                 y=f_mean, ymin = f_lower, ymax = f_upper,
#                 col=faceoff_won)) +
#   geom_line() +
#   geom_ribbon(aes(fill=faceoff_won), alpha=0.25, col=NA)
