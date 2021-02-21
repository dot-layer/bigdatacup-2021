
###########
# Fit model ---------------------------------------------------------------
###########
sequences2 <- sequences[faceoff_zone == "offense"]
mod <- gam(formula = result_goal_for ~ 0 + faceoff_win,
           # weight = sequences2$length_seconds,
           family = binomial(),
           data = sequences2)

summary(mod)
mod$coefficients
# plot(mod)
