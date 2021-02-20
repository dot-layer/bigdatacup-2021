
###########
# Fit model ---------------------------------------------------------------
###########
sequences2 <- sequences[faceoff_zone == "offense"]
mod <- gam(formula = result_goal_for ~ 0 + faceoff_win,
           # weight = sequences2$length_seconds,
           family = binomial(),
           data = sequences2)

summary(mod)

# plot(mod)


#########################
# Get effect of variables -------------------------------------------------
#########################

# max_time <- 300
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

res <- rbind(res_F,res_T)
ggplot(res, aes(x=exp(time_since_faceoff) - 1,
                y=p_mean,
                col=faceoff_won)) +
  theme_light() +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.005)) +
  xlab("time since faceoff") +
  ylab("goal probability (within a second)") +
  geom_line() +
  geom_col(data = sequences_exp[, .(y = mean(goal_scored)/5), .(time_since_faceoff, faceoff_won)],
           aes(x = time_since_faceoff, y = y, fill = faceoff_won),
           alpha = .4, position = "identity")

# sequences_exp[, .(count = sum(goal_scored)), .(time_since_faceoff, faceoff_won)]
sequences[length_seconds < 30, .(goal_for = sum(result_goal_for), goal_against = sum(result_goal_against)), .(faceoff_zone)]
