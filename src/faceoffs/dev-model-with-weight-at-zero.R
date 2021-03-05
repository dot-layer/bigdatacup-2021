
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
library(ggplot2)


###########
# Load data ---------------------------------------------------------------
###########

if("sequences_exp" %in% ls()) rm(sequences_exp)

# which years to load -- 20 means 2019-20
# watch out, no more than 3 years fit in memory
seas <- 20
filenames <- paste0("data/sequences_exp_",seas,".csv")
sequences_exp <- rbindlist(lapply(filenames, fread))

# keep a particular zone
# offensive and defensive results will be the same
FO_zone <- c("offense")
sequences_exp <- sequences_exp[faceoff_zone %in% FO_zone]

# max_time variable for later purposes
max_time <- 60


############
# Bar charts --------------------------------------------------------------
############

bin_width <- 2
sqo <- sequences_exp[time_since_faceoff < max_time, .(goal_for_scored = sum(goal_for_scored), N = .N), .(time_since_faceoff, faceoff_won)]
sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
sqo <- sqo[, .(y = sum(goal_for_scored)/sum(N)*100), .(bin, faceoff_won)]

gg <- ggplot(sqo,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time+1), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15")
gg
ggsave("report/figures/bar_off_nhl.png", gg, "png", width=4.5, height=3, units="in")


######################
# Fit model -- offense ----------------------------------------------------
######################
# sequences_exp[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)]
# sequences_exp[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)]
# mod <- gam(formula = goal_for_scored ~
#              s(time_since_faceoff_F, k = 8, pc = 0) + s(time_since_faceoff_T, k = 8, pc = 0),
#            # weights = 1 + 10*(sequences_exp$time_since_faceoff == 0),
#            family = binomial(),
#            data = sequences_exp)

sequences_exp[, faceoff_won := factor(faceoff_won, levels = c(FALSE,TRUE))]
mod <- gam(formula = goal_for_scored ~ s(time_since_faceoff, by = faceoff_won, pc = 0),
           # formula = goal_for_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, k = 8, pc = 0),
           # drop.intercept = T,
           weights = 1 + 100*(sequences_exp$time_since_faceoff == 0),
           family = binomial(),
           data = sequences_exp)
saveRDS(mod, "src/faceoffs/objects/mod_off_nhl.rds")

# diagnostics
plot(mod)
gam.check(mod)


#########################
# Get effect of variables -------------------------------------------------
#########################

t_max <- max(sequences_exp$time_since_faceoff)
res_table <- data.table(faceoff_won = rep(c(FALSE,TRUE), each = t_max+1),
                        time_since_faceoff = 0:t_max,
                        time_since_faceoff_F = c(0:t_max, rep(0, t_max+1)),
                        time_since_faceoff_T = c(rep(0, t_max+1), 0:t_max))
# res_table <- data.table(faceoff_won = rep(c(FALSE,TRUE), each = t_max+1),
#                         time_since_faceoff = 0:t_max)

res <- predict(mod, newdata = res_table, type = "response", se.fit = T)
res_table[, `:=`(mean = res$fit, se = c(res$se.fit))]
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.4)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)
gg

ggsave("report/figures/curve_off_nhl.png", gg, "png", width=4.5, height=3, units="in")
