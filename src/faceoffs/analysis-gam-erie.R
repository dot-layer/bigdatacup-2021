# Material related to the binning and modeling of the Erie data

# DO YOU REALLY WANT TO SAVE THE FIGURES?
save_figures <- T
# save_figures <- F

##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
library(ggplot2)


##################
# Process raw data --------------------------------------------------------
##################

source("src/faceoffs/data-processing-scripts/create-features.R")
sequences[]
sequences[, clock_begin := as.ITime(clock_begin, format = "%M:%S")]
sequences[, clock_end := as.ITime(clock_end, format = "%M:%S")]

source("src/faceoffs/functions/expandSequence.R")
sequences_exp <- rbindlist(lapply(1:nrow(sequences), function(k){
  expandSequence(sequences[k,], seq_id = k)
}))

# number of sequences that reach 60, 120 and 200 seconds
sequences_exp[faceoff_zone == "offense" & time_since_faceoff %in% c(30,60,90,120,180),
              .N, .(time_since_faceoff)]

# for graphical purposes
max_time <- 60

# for convenience, because we can
sequences_exp_off <- sequences_exp[faceoff_zone == "offense"]
sequences_exp_def <- sequences_exp[faceoff_zone == "defense"]


############
# Bar charts --------------------------------------------------------------
############

# we want bins of 5 seconds
binwidth <- 5

#### OFFENSE
sequences_exp_off[, bin := as.integer(floor(time_since_faceoff/binwidth)*binwidth) + binwidth/2]

gg <- ggplot(sequences_exp_off[time_since_faceoff < max_time,
                               .(y = mean(goal_for_scored)*100), .(bin, faceoff_won)],
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoffs") +
  theme_light(base_size = 7) +
  theme(legend.position = c(.875,.875), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank(),
        legend.key.size = unit(.75,"line")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goal for rate (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15", size=.25)
gg

if(save_figures) ggsave("report/figures/bar_off.png", gg, "png", width=3, height=2, units="in", dpi = 320)


#### DEFENSE
sequences_exp_def[, bin := as.integer(floor(time_since_faceoff/binwidth)*binwidth) + binwidth/2]

gg <- ggplot(sequences_exp_def[time_since_faceoff < max_time,
                               .(y = mean(goal_against_scored)*100), .(bin, faceoff_won)],
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal against rate following defensive faceoffs") +
  theme_light(base_size = 7) +
  theme(legend.position = c(.875,.875), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank(),
        legend.key.size = unit(.75,"line")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goal against rate (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15", size=.25)
gg

if(save_figures) ggsave("report/figures/bar_def.png", gg, "png", width=3, height=2, units="in")


######################
# Fit model -- offense ----------------------------------------------------
######################

mod_off <- gam(formula = goal_for_scored ~ s(log(time_since_faceoff+1.5), by = faceoff_won, pc = 0),
               family = binomial(),
               data = sequences_exp_off)

#### diagnostics
# plot(mod_off)
# gam.check(mod_off)

#### collecting results
res_table <- data.table(faceoff_won = factor(rep(c(FALSE,TRUE), each = max_time + 1)),
                        time_since_faceoff = 0:max_time)
res <- predict(mod_off, newdata = res_table, type = "response", se.fit = T)
res_table[,`:=`(mean = res$fit, se = c(res$se.fit))]
res_table[,`:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

#### plot
gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light(base_size = 7) +
  ggtitle("Goal for rate following offensive faceoffs") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("model-based goal for rate (%)") +
  geom_line(size=.35) +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)

gg
if(save_figures) ggsave("report/figures/curve_off.png", gg, "png", width=3, height=2, units="in")


######################
# Fit model -- defense ----------------------------------------------------
######################

mod_def <- gam(formula = goal_against_scored ~ s(log(time_since_faceoff + 1.5), by = faceoff_won, pc = 0),
               weight = 1 + 2*(sequences_exp_def$time_since_faceoff==2),
               family = binomial(),
               data = sequences_exp_def)

#### diagnostics
# plot(mod_off)
# gam.check(mod_off)

#### collecting results
res_table <- data.table(faceoff_won = factor(rep(c(FALSE,TRUE), each = max_time + 1)),
                        time_since_faceoff = 0:max_time)
res <- predict(mod_def, newdata = res_table, type = "response", se.fit = T)
res_table[,`:=`(mean = res$fit, se = c(res$se.fit))]
res_table[,`:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

#### plot
gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light(base_size = 7) +
  ggtitle("Goal against rate following defensive faceoffs") +
  theme(legend.position = c(.85,.8), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("model-based goal against rate (%)") +
  geom_line(size=.35) +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA)

gg
if(save_figures) ggsave("report/figures/curve_def.png", gg, "png", width=3, height=2, units="in")

