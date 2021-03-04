
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
library(ggplot2)


###########
# Fake data ---------------------------------------------------------------
###########

# S sequences
S <- 1500

# Generate sequences
seq_data <- rbindlist(lapply(1:S, function(s){
  
  # create sequence
  dt <- data.table(seq_id = s,
                   time_since_fo = seq(rpois(1,10)*4),
                   faceoff_won = sample(0:1,1),
                   goal_scored = 0)
  
  # formula for prob that goal is scored
  dt[, goal_prob := .001*(faceoff_won*pmax(35 - 2*time_since_fo, 10) +
                         (1 - faceoff_won)*pmin(time_since_fo/2, 10))]
  
  # generate goal (or no goal)
  dt[, goal_scored := sapply(goal_prob, function(JayP){
    sample(0:1, 1, prob = c(1 - JayP, JayP))})]
  
  # truncate sequence if a goal was scored
  if(any(dt$goal_scored == 1)) dt <- dt[1:which(goal_scored == 1)[1],]
  
  return(dt)
}))

# factor faceoff_won
seq_data[, faceoff_won := as.factor(faceoff_won)]

# peak at data
head(seq_data)
seq_data[seq_id == sample(max(seq_id),1)]
seq_data[, sum(goal_scored)]
hist(seq_data$time_since_fo)

###########
# Fit model ---------------------------------------------------------------
###########

# Generalized additive model
mod <- gam(formula = goal_scored ~ s(time_since_fo, by = faceoff_won,
                                     k = 10, fx = T),
           # knots = list(x = c(1,6,11,16,21,22,31,41,51)),
           family = binomial(),
           data = seq_data)
# plot(mod)


#########################
# Get effect of variables -------------------------------------------------
#########################

max_time <- 45
res <- data.table(time_since_fo = rep(1:max_time,times = 1),
                  faceoff_won = as.factor(rep(0:1, each = max_time)))

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


ggplot(res, aes(x=time_since_fo,
                y=p_mean, ymin = p_lower, ymax = p_upper,
                col=faceoff_won)) +
  theme_light() +
  xlab("time since faceoff") +
  ylab("goal probability") +
  geom_line() +
  geom_hline(lty=2, yintercept = .01) +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.25, col=NA)


# # Effects directly. Not sure I can read this properly
# ggplot(res, aes(x=time_since_fo,
#                 y=f_mean, ymin = f_lower, ymax = f_upper,
#                 col=faceoff_won)) +
#   geom_line() +
#   geom_ribbon(aes(fill=faceoff_won), alpha=0.25, col=NA)
