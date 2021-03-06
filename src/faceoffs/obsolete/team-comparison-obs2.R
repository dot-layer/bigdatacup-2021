# I STILL NEED SOME OF THIS


##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(gam)
library(ggplot2)


###########
# Load data ---------------------------------------------------------------
###########


# function to avoid overloading the memory
readGet <- function(seas){
  sequences_exp_temp <- fread(paste0("data/sequences_exp_",seas,".csv"))
  unique(sequences_exp_temp$team_abbreviation)
}

team_abb <- readGet(20)
source("src/faceoffs/functions/teamTable.R")
table_list <- lapply(team_abb, function(team){
  cat(team, " -- ")
  teamTable(team)
  })
saveRDS(table_list, "data/table_list_dev.rds")


bin_off <- rbindlist(lapply(table_list, "[[", "OZ_bin"))
max_time <- 60

gg <- ggplot(bin_off[bin == 10 & faceoff_won == T],
             aes(x = team_abbreviation, y = y, fill = team_abbreviation)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.title = element_blank(), panel.grid.minor = element_blank()) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge")
gg

gg <- ggplot(bin_off[bin == 10 & faceoff_won == T],
             aes(x = team_abbreviation, y = N, fill = team_abbreviation)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.title = element_blank(), panel.grid.minor = element_blank()) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, position = "dodge")
gg


ott_y <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "OTT"]$y/100
ott_N <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "OTT"]$N

tor_y <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "TOR"]$y/100
tor_N <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "TOR"]$N

# Number of goals
tor_y*ott_N - ott_y*ott_N


y1 <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "ARI"]$y/100
N1 <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "ARI"]$N

y2 <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "SJS"]$y/100
N2 <- bin_off[faceoff_won & bin == 10 & team_abbreviation == "SJS"]$N

# Number of goals
y2*N1 - y1*N1


y1 <- bin_off[!faceoff_won & bin == 10 & team_abbreviation == "ARI"]$y/100
N1 <- bin_off[!faceoff_won & bin == 10 & team_abbreviation == "ARI"]$N

y2 <- bin_off[!faceoff_won & bin == 10 & team_abbreviation == "SJS"]$y/100
N2 <- bin_off[!faceoff_won & bin == 10 & team_abbreviation == "SJS"]$N

# Number of goals
y2*N1 - y1*N1

#####




table_off <- rbindlist(lapply(table_list, "[[", "OZ_table"))
max_time <- 60

gg <- ggplot(table_off, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.position = c(.85,.875), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~team_abbreviation)

gg


table_def <- rbindlist(lapply(table_list, "[[", "DZ_table"))
table_def[, faceoff_won := factor(faceoff_won,
                                  levels = c(FALSE,TRUE)[(2:1)*(faceoff_won[1] == F) + (2:1)*(faceoff_won[1] == T)])]
max_time <- 60

gg <- ggplot(table_def, aes(x=time_since_faceoff, y=100*mean,
                            ymin = 100*lower, ymax = 100*upper, col=faceoff_won)) +
  theme_light() +
  ggtitle("Goal against rate following defensive faceoff") +
  theme(legend.position = c(.85,.875), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line() +
  geom_ribbon(aes(fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~team_abbreviation)

gg



### Test
max_time <- 60

gg <- ggplot() +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.direction = "vertical", legend.box = "horizontal",
        legend.position = c(.6,.875), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
  scale_fill_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
  scale_linetype_manual(labels = c("faceoff won", "faceoff lost"), values = as.factor(c(1,2))) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(-.25,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  # geom_line(data=table_off, aes(x=time_since_faceoff, y=100*mean,
  #                          ymin = 100*lower, ymax = 100*upper, col=team_abbreviation)) +
  geom_line(data=table_off[faceoff_won == T], aes(x=time_since_faceoff, y=100*mean, col=team_abbreviation, linetype=factor(1))) +
  geom_ribbon(data=table_off[faceoff_won == T], aes(x=time_since_faceoff, y=100*mean,
                                  ymin = 100*lower, ymax = 100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
  geom_line(data=table_off[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean, col=team_abbreviation, linetype=factor(2))) +
  geom_ribbon(data=table_off[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean,
                                  ymin = -100*lower, ymax = -100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
  geom_abline(slope=0, intercept = 0, col="gray50")
  # geom_line(data=table_def[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean, col=team_abbreviation)) +
  # geom_ribbon(data=table_def[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean,
  #                                                   ymin = -100*lower, ymax = -100*upper, fill=team_abbreviation), alpha=0.15, col=NA) +
    # facet_wrap(~faceoff_zone)

gg <- ggplot() +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.direction = "vertical", legend.box = "horizontal",
        legend.position = c(.6,.875), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
  scale_fill_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
  scale_linetype_manual(labels = c("faceoff won", "faceoff lost"), values = c(2,1)) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(-.25,.45)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_line(data=table_off[team_abbreviation %in% c("OTT", "TOR")], aes(x=time_since_faceoff, y=(-1)^(1-faceoff_won)*100*mean, col=team_abbreviation, linetype=faceoff_won)) +
  geom_ribbon(data=table_off[team_abbreviation %in% c("OTT", "TOR")], aes(x=time_since_faceoff, y=(-1)^(1-faceoff_won)*100*mean, linetype=faceoff_won,
                                                    ymin = (-1)^(1-faceoff_won)*100*lower, ymax = (-1)^(1-faceoff_won)*100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
  geom_abline(slope=0, intercept = 0, col="gray50")

gg


gg <- ggplot() +
  theme_light() +
  ggtitle("Goal for rate following offensive faceoff") +
  theme(legend.direction = "vertical", legend.box = "horizontal",
        legend.position = c(.6,.875), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(labels = c("ARI", "SJS"), values = c("red", "blue")) +
  scale_fill_manual(labels = c("ARI", "SJS"), values = c("red", "blue")) +
  scale_linetype_manual(labels = c("faceoff won", "faceoff lost"), values = c(1,2)) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(-.2,.6)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  # geom_line(data=table_off, aes(x=time_since_faceoff, y=100*mean,
  #                          ymin = 100*lower, ymax = 100*upper, col=team_abbreviation)) +
  geom_line(data=table_off[team_abbreviation %in% c("ARI", "SJS")], aes(x=time_since_faceoff, y=(-1)^(1-faceoff_won)*100*mean, col=team_abbreviation, linetype=faceoff_won)) +
  geom_ribbon(data=table_off[team_abbreviation %in% c("ARI", "SJS")], aes(x=time_since_faceoff, y=(-1)^(1-faceoff_won)*100*mean, linetype=faceoff_won,
                                                                          ymin = (-1)^(1-faceoff_won)*100*lower, ymax = (-1)^(1-faceoff_won)*100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
  geom_abline(slope=0, intercept = 0, col="gray50")
# geom_line(data=table_def[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean, col=team_abbreviation)) +
# geom_ribbon(data=table_def[faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean,
#                                                   ymin = -100*lower, ymax = -100*upper, fill=team_abbreviation), alpha=0.15, col=NA) +

gg



# bin_off <- rbindlist(lapply(table_list, "[[", "OZ_bin"))
# max_time <- 60
# 
# gg <- ggplot() +
#   ggtitle("Goal for rate following offensive faceoff") +
#   theme_light() +
#   theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
#   scale_color_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
#   scale_fill_manual(labels = c("OTT", "TOR"), values = c("red", "blue")) +
#   coord_cartesian(xlim = c(0,max_time-2), ylim = c(-.25,.35)) +
#   xlab("time elapsed since faceoff") +
#   ylab("goals/sequences (%)") +
#   geom_col(data=bin_off[faceoff_won == T], aes(x = bin, y = y, fill = team_abbreviation, group=faceoff_won), alpha = .75, position = "dodge2", col="gray15") +
#   geom_col(data=bin_off[!faceoff_won == F], aes(x = bin, y = -y, fill = team_abbreviation, group=faceoff_won), alpha = .75, position = "dodge2", col="gray15")
# gg
# ggsave("report/figures/bar_off_ott_tor.png", gg, "png", width=3.5, height=4, units="in")
# 
