
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
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
mod <- gam(formula = goal_scored ~ s(time_since_faceoff, by = faceoff_won,
                                     # k = 15, fx = T),
                                     k = length(kn), fx = T),
                                     # bs = "cr",
                                     # m = 1),
           knots = list(x = kn),
           family = binomial(),
           data = sequences_exp[faceoff_zone == "offense"])

# mod <- gam(formula = goal_scored ~ faceoff_won +
#              s(time_since_faceoff, #k = 3, fx = T),
#                                    k = length(kn)),
#            knots = list(x = kn),
#            family = binomial(),
#            data = sequences_exp[faceoff_zone == "offense"])

# plot(mod)


#########################
# Get effect of variables -------------------------------------------------
#########################

max_time <- 300
res <- data.table(time_since_faceoff = rep(1:max_time,times = 1),
                  faceoff_won = as.factor(rep(c(FALSE,TRUE), each = max_time)))

res <- cbind(res,
             do.call("cbind", predict(mod,
                     newdata = res,
                     type = "response",
                     se.fit = T)),
             rowSums(predict(mod,
                             newdata = res,
                             type = "terms")),
             rowSums(predict(mod,
                             newdata = res,
                             type = "terms", se.fit = T)$se.fit)
             )
names(res)[3:6] <- c("p_mean", "p_se", "f_mean", "f_se")
res[, `:=`(p_lower = p_mean - 1.96*p_se,  p_upper = p_mean + 1.96*p_se)]
res[, `:=`(f_lower = f_mean - 1.96*f_se,  f_upper = f_mean + 1.96*f_se)]


ggplot(res, aes(x=time_since_faceoff,
                y=p_mean, ymin = p_lower, ymax = p_upper,
                col=faceoff_won)) +
  theme_light() +
  coord_cartesian(xlim = c(0,200), ylim = c(0,.005)) +
  xlab("time since faceoff") +
  ylab("goal probability") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.25, col=NA)


# # Effects directly. Not sure I can read this properly
# ggplot(res, aes(x=time_since_fo,
#                 y=f_mean, ymin = f_lower, ymax = f_upper,
#                 col=faceoff_won)) +
#   geom_line() +
#   geom_ribbon(aes(fill=faceoff_won), alpha=0.25, col=NA)
