
##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(gam)
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
sequences_exp <- sequences_exp[goal_scored >= 0,]

# max_time variable for later purposes
max_time <- 60


############
# Bar charts --------------------------------------------------------------
############

bin_width <- 2
sqo <- sequences_exp[time_since_faceoff < max_time, .(goal_scored = sum(goal_scored), N = .N), .(time_since_faceoff, faceoff_won)]
sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
sqo <- sqo[, .(y = sum(goal_scored)/sum(N)*100), .(bin, faceoff_won)]

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
sequences_exp[, time_since_faceoff_F := time_since_faceoff*(faceoff_won == F)] 
sequences_exp[, time_since_faceoff_T := time_since_faceoff*(faceoff_won == T)] 

mod <- gam(formula = goal_scored ~
            s(time_since_faceoff_F, df=8) + s(time_since_faceoff_T, df=8),
           weights = 1 + 100*(sequences_exp$time_since_faceoff == 0),
           family = binomial(),
           data = sequences_exp)

# plot(mod)
# predict(mod, newdata = sequences_exp_off[1:10,], type = "response")


#########################
# Get effect of variables -------------------------------------------------
#########################
res <- predict(mod, type = "response", se.fit = T)
res_table <- data.table(faceoff_won = sequences_exp$faceoff_won,
                        time_since_faceoff = sequences_exp$time_since_faceoff,
                        mean = res$fit,
                        se = c(res$se.fit))
res_table <- unique(res_table, by = c("faceoff_won", "time_since_faceoff"))
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)

gg
ggsave("report/figures/curve_off_nlh.png", gg, "png", width=4.5, height=3, units="in")
