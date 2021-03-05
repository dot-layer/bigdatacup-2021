

# To perform this analysis, we needed to construct several new objects.
# For each team:
#   1. a table containing the offensive and defensive binned data
#   2. a table containing the results of gam models

# Those are created with the script src/faceoffs/table-creator-teams.R
# They are saved under the names
#   src/faceoffs/objects/bin_teams.csv
#   src/faceoffs/objects/res_teams.csv


##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(ggplot2)
library(ggrepel)
library(mgcv)


###########
# Load data ---------------------------------------------------------------
###########

seas <- 20
sequences_exp <- fread(paste0("data/sequences_exp_",seas,".csv"))
sequences_exp <- sequences_exp[faceoff_zone %in% c("offense","defense")]



# Create and visualize binned data ----------------------------------------


bin_width <- 20
sequences_exp[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
seq_bin <- sequences_exp[, .(N = .N, goal_for_scored = sum(goal_for_scored), goal_against_scored = sum(goal_against_scored)),
                         .(bin, faceoff_zone, faceoff_won, team_abbreviation, season_years2)]
seq_bin <- seq_bin[(faceoff_won==T & faceoff_zone == "offense") |
                     (faceoff_won==F & faceoff_zone == "defense")]

seq_bin[faceoff_zone == "offense", prop_for := goal_for_scored/N]
seq_bin[faceoff_zone == "defense", prop_against := goal_against_scored/N]
seq_bin[,`:=`(prop_diff = sum(prop_for, na.rm=T) - sum(prop_against, na.rm=T)),.(bin, team_abbreviation)]

# offense
ggplot(seq_bin[bin == 10 & faceoff_won == T,],
             # aes(x = team_abbreviation, y = prop, fill = faceoff_won)) +
             aes(x = team_abbreviation, y = prop_for, fill = team_abbreviation)) +
  ggtitle("Goal rate following faceoff") +
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, col="gray15")

# defense
ggplot(seq_bin[bin == 10 & faceoff_won == F,],
       # aes(x = team_abbreviation, y = prop, fill = faceoff_won)) +
       aes(x = team_abbreviation, y = prop_against, fill = team_abbreviation)) +
  ggtitle("Goal rate following faceoff") +
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, col="gray15")

# diff
ggplot(seq_bin[bin == 10 & faceoff_won == T],
             # aes(x = team_abbreviation, y = prop, fill = faceoff_won)) +
             aes(x = team_abbreviation, y = prop_diff, fill = team_abbreviation)) +
  ggtitle("Goal rate following faceoff") +
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .9, col="gray15")

# dual
ggplot() +
  ggtitle("Goal rate following faceoff") +
  theme_light() +
  theme(legend.position = "none", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  ylab("goals/sequences (%)") +
  geom_col(data = seq_bin[bin == 10 & faceoff_won == T & faceoff_zone == "offense"],
           aes(x = team_abbreviation, y = prop_for), fill = "green", alpha = .25, col="gray15") +
  geom_col(data = seq_bin[bin == 10 & faceoff_won == F & faceoff_zone == "defense"],
           aes(x = team_abbreviation, y = -prop_against), fill = "red", alpha = .25, col="gray15")




##########
# X-Y plot ----------------------------------------------------------------
##########
ran <- range(c(range(100*seq_bin[bin==10]$prop_for, na.rm=T),
               range(100*seq_bin[bin==10]$prop_against, na.rm=T))) + c(-.01,.01)

gg <- ggplot(seq_bin[bin == 10,.(prop_for = sum(prop_for, na.rm = T),
                  prop_against = sum(prop_against, na.rm = T)),
               .(team_abbreviation)],
       aes(x = 100*prop_for, y = 100*prop_against)) +
  ggtitle("GF rate (OZ) vs GA rate (DZ) for the NHL 2019-20 season") +
  geom_point(size=.35, color="blue", alpha=.35) +
  geom_text_repel(aes(label=team_abbreviation), size=1.75, min.segment.length = .05,
                  segment.size = .05, segment.color="blue", segment.alpha=.5) +
  theme_light(base_size = 6) +
  theme(panel.grid.minor = element_blank()) +
  xlab("goal for rate (%) following offensive faceoffs") +
  ylab("goal against rate (%) following defensive faceoffs") +
  scale_x_continuous(breaks = seq(0,.25,.05), limits = ran) +
  scale_y_continuous(breaks = seq(0,.25,.05), limits = ran)
gg
range(100*seq_bin[bin==10]$prop_for, na.rm=T)
diff(range(seq_bin[bin==10]$prop_for, na.rm=T))
range(100*seq_bin[bin==10]$prop_against, na.rm=T)
diff(range(seq_bin[bin==10]$prop_against, na.rm=T))

ggsave("report/figures/xy_nhl.png", gg, "png", width=3, height=2.8, units="in", dpi=320)
#




########
# Models ------------------------------------------------------------------
########
sequences_exp[, faceoff_won := factor(faceoff_won)]

# model BOS
mod_off_BOS <- gam(formula = goal_for_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, pc = 0),
                 family = binomial(),
                 data = sequences_exp[team_abbreviation == "BOS" & faceoff_zone == "offense"])
saveRDS(mod_off_BOS, "src/faceoffs/objects/mod_off_BOS.rds")

mod_def_BOS <- gam(formula = goal_against_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, pc = 0),
                   family = binomial(),
                   data = sequences_exp[team_abbreviation == "BOS" & faceoff_zone == "defense"])
saveRDS(mod_def_BOS, "src/faceoffs/objects/mod_def_BOS.rds")

# model BOS
mod_off_BUF <- gam(formula = goal_for_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, pc = 0),
                   family = binomial(),
                   data = sequences_exp[team_abbreviation == "BUF" & faceoff_zone == "offense"])
saveRDS(mod_off_BUF, "src/faceoffs/objects/mod_off_BUF.rds")

mod_def_BUF <- gam(formula = goal_against_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, pc = 0),
                   family = binomial(),
                   data = sequences_exp[team_abbreviation == "BUF" & faceoff_zone == "defense"])
saveRDS(mod_def_BUF, "src/faceoffs/objects/mod_def_BUF.rds")

# build res tables
t_max <- max(sequences_exp$time_since_faceoff)
buildResTable <- function(mod){
  res_table <- data.table(faceoff_won = factor(rep(c(FALSE,TRUE), each = t_max+1)),
                          time_since_faceoff = 0:t_max)
  # res_table <- data.table(faceoff_won = rep(c(FALSE,TRUE), each = t_max+1),
  #                         time_since_faceoff = 0:t_max)

  ress <- predict(mod, newdata = res_table, type = "response", se.fit = T)
  res_table[, `:=`(mean = ress$fit, se = c(ress$se.fit))]
  res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
  res_table
}

res <- lapply(list(mod_off_BOS, mod_def_BOS, mod_off_BUF, mod_def_BUF), buildResTable)

for(k in seq_along(res)){
  res[[k]][, team_abbreviation := c("BOS","BOS","BUF","BUF")[k]]
  res[[k]][, faceoff_zone := c("offense","defense","offense","defense")[k]]
}
res <- rbindlist(res)
fwrite(res, "src/faceoffs/objects/res_BOS_BUF.csv")


#####
# Vul ---------------------------------------------------------------------
#####
res <- fread("src/faceoffs/objects/res_BOS_BUF.csv")
max_time <- 60

ggplot(res[team_abbreviation == "BOS" & faceoff_zone == "offense" & faceoff_won == T], aes(x = time_since_faceoff, y=100*mean)) + geom_line()
ggplot(res[team_abbreviation == "BUF" & faceoff_zone == "offense" & faceoff_won == T], aes(x = time_since_faceoff, y=100*mean)) + geom_line()
ggplot(res[team_abbreviation == "BUF" & faceoff_zone == "defense" & faceoff_won == T], aes(x = time_since_faceoff, y=100*mean)) + geom_line()

gg <- ggplot() +
  theme_light(base_size=6) +
  ggtitle("Goal rates comparison") +
  theme(legend.direction = "vertical", legend.box = "horizontal",
        legend.position = c(.8,.85), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(), legend.key.size = unit(.75,"line")) +
  scale_color_manual(labels = c("BOS", "BUF"), values = c("gold", "gray50")) +
  scale_fill_manual(labels = c("BOS", "BUF"), values = c("gold", "gray50")) +
  # scale_linetype_manual(labels = c("defensive faceoff won", "offensive faceoff lost"), values = c(2,1)) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(-.4,.5)) +
  xlab("time elapsed since faceoff") +
  ylab("model-based goal rate (%)") +
  geom_line(data=res[faceoff_zone == "offense" & faceoff_won == T], aes(x=time_since_faceoff, y=100*mean, col=team_abbreviation), size = .35) +
  geom_ribbon(data=res[faceoff_zone == "offense" & faceoff_won == T], aes(x=time_since_faceoff, y= 100*mean,
                                                                          ymin = 100*lower, ymax = 100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
  geom_line(data=res[faceoff_zone == "defense" & faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean, col=team_abbreviation), size = .35) +
  geom_ribbon(data=res[faceoff_zone == "defense" & faceoff_won == F], aes(x=time_since_faceoff, y= -100*mean,
                                                                          ymin = -100*lower, ymax = -100*upper, fill=team_abbreviation), alpha=0.075, col=NA)+
  geom_abline(slope=0, intercept = 0, col="black", linetype = 3, size = .25)

gg
ggsave("report/figures/curve_BOS_BUF.png", gg, "png", width=3, height=2.8, units="in", dpi=320)






# bin_width <- 3
# sequences_exp[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
# seq_bin <- sequences_exp[, .(N = .N, goal_for_scored = sum(goal_for_scored), goal_against_scored = sum(goal_against_scored)),
#                          .(bin, faceoff_zone, faceoff_won, team_abbreviation, season_years2)]
# seq_bin <- seq_bin[(faceoff_won==T & faceoff_zone == "offense") |
#                      (faceoff_won==F & faceoff_zone == "defense")]
# 
# seq_bin[faceoff_zone == "offense", prop_for := goal_for_scored/N]
# seq_bin[faceoff_zone == "defense", prop_against := goal_against_scored/N]
# seq_bin[,`:=`(prop_diff = sum(prop_for, na.rm=T) - sum(prop_against, na.rm=T)),.(bin, team_abbreviation)]
# 
# max_time <- 30
# ggplot() +
#   theme_light() +
#   ggtitle("Goal for/against rates following offensive/defensive faceoffs") +
#   theme(legend.direction = "vertical", legend.box = "horizontal",
#         legend.position = c(.8,.85), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"),
#         panel.grid.minor = element_blank()) +
#   scale_color_manual(labels = c("BOS", "BUF"), values = c("gold", "gray50")) +
#   scale_fill_manual(labels = c("BOS", "BUF"), values = c("gold", "gray50")) +
#   # scale_linetype_manual(labels = c("defensive faceoff won", "offensive faceoff lost"), values = c(2,1)) +
#   coord_cartesian(xlim = c(0,max_time), ylim = c(-.4,.5)) +
#   xlab("time elapsed since faceoff") +
#   ylab("model-based goal rate (%)") +
#   geom_line(data=res[faceoff_zone == "offense" & faceoff_won == T], aes(x=time_since_faceoff, y=100*mean, col=team_abbreviation)) +
#   geom_ribbon(data=res[faceoff_zone == "offense" & faceoff_won == T], aes(x=time_since_faceoff, y= 100*mean,
#                                                                           ymin = 100*lower, ymax = 100*upper, fill=team_abbreviation), alpha=0.075, col=NA) +
#   geom_line(data=res[faceoff_zone == "defense" & faceoff_won == F], aes(x=time_since_faceoff, y=-100*mean, col=team_abbreviation)) +
#   geom_ribbon(data=res[faceoff_zone == "defense" & faceoff_won == F], aes(x=time_since_faceoff, y= -100*mean,
#                                                                           ymin = -100*lower, ymax = -100*upper, fill=team_abbreviation), alpha=0.075, col=NA)+
#   geom_col(data = seq_bin[team_abbreviation == "BUF" & faceoff_won == T],
#            aes(x = bin, y = 100*prop_for), fill = "green", alpha = .25, col="gray15") +
#   geom_col(data = seq_bin[team_abbreviation == "BUF" & faceoff_won == F],
#            aes(x = bin, y = -100*prop_against), fill = "red", alpha = .25, col="gray15") +
#   geom_abline(slope=0, intercept = 0, col="blue", linetype = 2)
# ggplot() +
#   ggtitle("Goal rate following faceoff") +
#   theme_light() +
#   theme(legend.position = "none", legend.title = element_blank(), panel.grid.minor = element_blank()) +
#   ylab("goals/sequences (%)") +
#   geom_col(data = seq_bin[team_abbreviation == "BUF" & faceoff_won == T],
#            aes(x = bin, y = prop_for), fill = "green", alpha = .25, col="gray15") +
#   geom_col(data = seq_bin[team_abbreviation == "BUF" & faceoff_won == F],
#            aes(x = bin, y = prop_against), fill = "red", alpha = .25, col="gray15") +
#   facet_wrap(~faceoff_zone)




# Let look at them

#### THIS IS CRAP HERE. I NEED TO WORK THIS OUT

sequences_exp <- sequences_exp[bin == 10 & faceoff_won]
sequences_exp[season_years2 == 20][order(prop)]

y1 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "BUF"]$prop
N1 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "BUF"]$N

y2 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "NSH"]$prop
N2 <- sequences_exp[faceoff_won & bin == 10 & team_abbreviation == "NSH"]$N

# Number of goals
y2*N1 - y1*N1

# 185 GF ARI in 2019-2020
(y2*N1 - y1*N1)/195
