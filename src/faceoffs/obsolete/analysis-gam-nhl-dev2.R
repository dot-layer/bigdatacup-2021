
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
seas <- 16:20
filenames <- paste0("data/sequences_exp_",seas,".csv")

# function to avoid overloading the memory
readReduce <- function(filename){
  sequences_exp_temp <- fread(filename)
  sequences_exp_temp[period_type == "overtime"]
}

sequences_exp <- readReduce(filenames[1])
for(filename in filenames[-1]){
  sequences_exp <- rbind(sequences_exp, readReduce(filename))
}  

sequences_exp[time_since_faceoff == 0,]

sequences_exp[, faceoff_zone2 := faceoff_zone]
sequences_exp[!(faceoff_zone2 %in% c("offense", "defense")), faceoff_zone2 := "neutral"]

# max_time variable for later purposes
max_time <- 60


############
# Bar charts --------------------------------------------------------------
############
bin_width <- 10
sqo <- sequences_exp[time_since_faceoff < max_time, .(goal_for_scored = sum(goal_for_scored), N = .N), .(time_since_faceoff, faceoff_zone2, faceoff_won)]
sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
sqo <- sqo[, .(y = sum(goal_for_scored)/sum(N)*100, N = sum(N)), .(bin, faceoff_zone2, faceoff_won)]
sqo[order(faceoff_zone2,faceoff_won, bin)]
sqo[, prop := N/N[1], .(faceoff_zone2, faceoff_won)]
sqo[, y2 := y*prop]

gg <- ggplot(sqo,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following faceoff") +
  theme_light() +
  theme(legend.position = c(.5,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time), ylim = c(0,.75)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge", col="gray15") +
  facet_wrap(~faceoff_zone2)
gg
ggsave("report/figures/bar_ot_nhl.png", gg, "png", width=4.5+4.5/2, height=3, units="in")


######################
# Fit model -- offense ----------------------------------------------------
######################
# sequences_exp[, time_since_faceoff2 := time_since_faceoff] 
sequences_exp[, time_since_faceoff2 := log(time_since_faceoff + 1)] 

sequences_exp[, time_since_faceoff_F_D := time_since_faceoff2*(faceoff_won == F)*(faceoff_zone2 == "defense")] 
sequences_exp[, time_since_faceoff_T_D := time_since_faceoff2*(faceoff_won == T)*(faceoff_zone2 == "defense")] 
sequences_exp[, time_since_faceoff_F_N := time_since_faceoff2*(faceoff_won == F)*(faceoff_zone2 == "neutral")] 
sequences_exp[, time_since_faceoff_T_N := time_since_faceoff2*(faceoff_won == T)*(faceoff_zone2 == "neutral")] 
sequences_exp[, time_since_faceoff_F_O := time_since_faceoff2*(faceoff_won == F)*(faceoff_zone2 == "offense")] 
sequences_exp[, time_since_faceoff_T_O := time_since_faceoff2*(faceoff_won == T)*(faceoff_zone2 == "offense")] 

df <- 5
mod_off <- gam(formula = goal_for_scored ~
                 s(time_since_faceoff_F_D, df=df) + s(time_since_faceoff_T_D, df=df) +
                 s(time_since_faceoff_F_N, df=df) + s(time_since_faceoff_T_N, df=df) +
                 s(time_since_faceoff_F_O, df=df) + s(time_since_faceoff_T_O, df=df),
           # weights = 1 + 10*(sequences_exp$time_since_faceoff == 0),
           family = binomial(),
           data = sequences_exp)

# mod_def <- gam(formula = goal_against_scored ~ s(time_since_faceoff_F_D, df=df) + s(time_since_faceoff_T_D, df=df) +
#                  s(time_since_faceoff_F_N, df=df) + s(time_since_faceoff_T_N, df=df) +
#                  s(time_since_faceoff_F_O, df=df) + s(time_since_faceoff_T_O, df=df),
#                # weights = 1 + 10*(sequences_exp$time_since_faceoff == 0),
#                family = binomial(),
#                data = sequences_exp)

# plot(mod)
# predict(mod, newdata = sequences_exp_off[1:10,], type = "response")


res <- predict(mod_off, type = "response", se.fit = T)
res_table <- data.table(faceoff_won = sequences_exp$faceoff_won,
                        faceoff_zone2 = sequences_exp$faceoff_zone2,
                        time_since_faceoff = sequences_exp$time_since_faceoff,
                        mean = res$fit,
                        se = c(res$se.fit))

rt0 <- res_table[,.(N = .N), .(faceoff_won, faceoff_zone2, time_since_faceoff)]
rt0[, prop := N/N[1]]
res_table <- unique(res_table, by = c("faceoff_won", "faceoff_zone2", "time_since_faceoff"))
res_table <- merge.data.table(res_table, rt0, by = c("faceoff_won", "faceoff_zone2", "time_since_faceoff"))

res_table <- unique(res_table, by = c("faceoff_won", "faceoff_zone2", "time_since_faceoff"))
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
res_table <- res_table[order(faceoff_won, faceoff_zone2, time_since_faceoff)]
res_table[,`:=`(mean2 = mean*prop,
                lower2 = lower*prop,
                upper2 = upper*prop)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean, col=faceoff_won,
                            ymin = 100*lower, ymax = 100*upper)) +
  theme_light() +
  ggtitle("Goal for rate following faceoff") +
  theme(legend.position = c(.5,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time+15), ylim = c(0,.5)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~faceoff_zone2)
gg
ggsave("report/figures/curve_ot_nlh.png", gg, "png", width=4.5, height=3, units="in")

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean2, col=faceoff_won,
                            ymin = 100*lower2, ymax = 100*upper2)) +
  theme_light() +
  ggtitle("Goal for rate following faceoff") +
  theme(legend.position = c(.35,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.25)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~faceoff_zone2)
gg



#########################
# Get effect of variables -------------------------------------------------
#########################
res_tables <- lapply(list(mod_off, mod_def), function(mod){
  res <- predict(mod, type = "response", se.fit = T)
  res_table <- data.table(faceoff_won = sequences_exp$faceoff_won,
                          faceoff_zone2 = sequences_exp$faceoff_zone2,
                          time_since_faceoff = sequences_exp$time_since_faceoff,
                          mean = res$fit,
                          se = c(res$se.fit))
  res_table <- unique(res_table, by = c("faceoff_won", "faceoff_zone2", "time_since_faceoff"))
  res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
  res_table[order(faceoff_won, faceoff_zone2, time_since_faceoff)]
})

res_table3 <- res_tables[[1]][,1:4]
res_table3$mean <- res_tables[[1]]$mean - res_tables[[2]]$mean

gg <- ggplot() +
  theme_light() +
  ggtitle("Goal for rate following faceoff") +
  theme(legend.position = c(.5,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(-.4,.4)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line(data = res_tables[[1]], aes(x=time_since_faceoff, y=100*mean, col=faceoff_won), alpha = .25) +
  geom_ribbon(data = res_tables[[1]], aes(x=time_since_faceoff, y=100*mean, 
                                    ymin = 100*lower, ymax = 100*upper,
                                    fill=faceoff_won), alpha=0.05, col=NA) +
  geom_line(data = res_tables[[2]], aes(x=time_since_faceoff, y=-100*mean, col=faceoff_won), alpha = .25) +
  geom_ribbon(data = res_tables[[2]], aes(x=time_since_faceoff, y=-100*mean, 
                                    ymin = -100*lower, ymax = -100*upper,
                                    fill=faceoff_won), alpha=0.05, col=NA) +
  geom_line(data = res_table3, aes(x=time_since_faceoff, y=100*mean, col=faceoff_won), linetype = 2) +
  facet_wrap(~faceoff_zone2)

gg
# ggsave("report/figures/curve_ot_nlh.png", gg, "png", width=4.5, height=3, units="in")




#### Whats going on with neutral
bin_width <- 10
sqo <- sequences_exp[, .(goal_for_scored = sum(goal_for_scored), N = .N), .(time_since_faceoff, faceoff_zone2, faceoff_won)]
sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
sqo <- sqo[, .(y = sum(goal_for_scored)/sum(N)*100, N = sum(N)), .(bin, faceoff_zone2, faceoff_won)]
sqo[order(faceoff_zone2,faceoff_won, bin)]

df <- 5
mod <- gam(formula = goal_for_scored ~
             s(time_since_faceoff_F_N, df=df) + s(time_since_faceoff_T_N, df=df),
           family = binomial(),
           data = sequences_exp[faceoff_zone2 == "neutral"])

res <- predict(mod, type = "response", se.fit = T)
res_table <- data.table(faceoff_won = sequences_exp[faceoff_zone2 == "neutral"]$faceoff_won,
                        faceoff_zone2 = sequences_exp[faceoff_zone2 == "neutral"]$faceoff_zone2,
                        time_since_faceoff = sequences_exp[faceoff_zone2 == "neutral"]$time_since_faceoff,
                        mean = res$fit,
                        se = c(res$se.fit))
res_table <- unique(res_table, by = c("faceoff_won", "faceoff_zone2", "time_since_faceoff"))
res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]

gg <- ggplot(res_table, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following faceoff") +
  theme(legend.position = c(.5,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,75), ylim = c(0,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~faceoff_zone2)

gg
