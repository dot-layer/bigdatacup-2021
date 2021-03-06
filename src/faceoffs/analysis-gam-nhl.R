# Here, ew replicate the analysis done the Erie dataset, this time using
# NHL data from 2019-20 (fetched with the package tidynhl)

# In this version, you need the dataset
#     data/sequences_exp_20.csv

# If you don't have that data, run the script 
#     src/faceoffs/data-prossessing-scripts/construct-data-nhl.R
# with
#     seas <- 20


# DO YOU WANT TO SAVE THE FIGURES GENERATED?
save_figures <- T
# save_figures <- F


##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
library(ggplot2)


###########
# Load data ---------------------------------------------------------------
###########

# YOU SHOULD NOT HAVE TO RUN THAT PART -- RELEVANT OBJECTS SHOULD BE AVAILABLE
# IN THE REPO IN src/faceoffs/objects/

# if("sequences_exp" %in% ls()) rm(sequences_exp)
# 
# # which years to load -- 20 means 2019-20
# # watch out, no more than 3 years fit in memory
# seas <- 20
# filenames <- paste0("data/sequences_exp_",seas,".csv")
# sequences_exp <- rbindlist(lapply(filenames, fread))
# 
# # keep a particular zone
# # offensive and defensive results will be the same
# FO_zone <- c("offense")
# sequences_exp <- sequences_exp[faceoff_zone %in% FO_zone]

# max_time variable for later purposes
max_time <- 60


############
# Bar charts --------------------------------------------------------------
############

# general bin_width
# bin_width <- 2

# sqo <- sequences_exp[time_since_faceoff < max_time, .(goal_for_scored = sum(goal_for_scored), N = .N), .(time_since_faceoff, faceoff_won)]
# sqo[, bin := as.integer(floor(time_since_faceoff/bin_width)*bin_width) + bin_width/2]
# sqo <- sqo[, .(y = sum(goal_for_scored)/sum(N)*100), .(bin, faceoff_won)]
# fwrite(sqo, "src/faceoffs/objects/bin_off_nhl.csv")
sqo <- fread("src/faceoffs/objects/bin_off_nhl.csv")

gg <- ggplot(sqo,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoffs") +
  theme_light(base_size = 7) +
  theme(legend.position = c(.875,.875), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank(),
        legend.key.size = unit(.75,"line")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time+1), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goal for rate (%)") +
  scale_x_continuous(breaks = seq(0,60,6)) + 
  geom_col(alpha = .9, position = "dodge", col="gray15", size = .25)
gg
if(save_figures) ggsave("report/figures/bar_off_nhl.png", gg, "png", width=3, height=2, units="in", dpi = 320)


######################
# Fit model -- offense ----------------------------------------------------
######################
# sequences_exp[, faceoff_won := factor(faceoff_won)]

# final model
# mod_final <- gam(formula = goal_for_scored ~ s(log(time_since_faceoff+1), by = faceoff_won, pc = 0),
#                  family = binomial(),
#                  data = sequences_exp)
# saveRDS(mod_final, "src/faceoffs/objects/mod_final_nhl.rds")


# old model
# mod_obsolete <- gam(formula = goal_for_scored ~ s(time_since_faceoff, by = faceoff_won, pc = 0),
#                family = binomial(),
#                data = sequences_exp)
# saveRDS(mod_obsolete, "src/faceoffs/objects/mod_obsolete_nhl.rds")


#### diagnostics
# plot(mod)
# gam.check(mod)
# summary(mod)


###########################
# Construct relevant tables -----------------------------------------------
###########################

# t_max <- max(sequences_exp$time_since_faceoff)
# buildResTable <- function(mod){
#   res_table <- data.table(faceoff_won = rep(c(FALSE,TRUE), each = t_max+1),
#                           time_since_faceoff = 0:t_max,
#                           time_since_faceoff_F = c(0:t_max, rep(0, t_max+1)),
#                           time_since_faceoff_T = c(rep(0, t_max+1), 0:t_max))
#   # res_table <- data.table(faceoff_won = rep(c(FALSE,TRUE), each = t_max+1),
#   #                         time_since_faceoff = 0:t_max)
#   
#   res <- predict(mod, newdata = res_table, type = "response", se.fit = T)
#   res_table[, `:=`(mean = res$fit, se = c(res$se.fit))]
#   res_table[, `:=`(lower = mean - 1.96*se, upper = mean + 1.96*se)]
#   res_table
# }
# 
# res_final <- buildResTable(mod_final)
# res_final[, time_variable := "log(t+1)"]
# fwrite(res_final, "src/faceoffs/objects/res_final.csv")
# 
# res_obsolete <- buildResTable(mod_obsolete)
# res_obsolete[, time_variable := "t"]
# fwrite(res_obsolete, "src/faceoffs/objects/res_obsolete.csv")



#############
# Curve plots -------------------------------------------------------------
#############

res_final <- fread("src/faceoffs/objects/res_final.csv")
res_obsolete <- fread("src/faceoffs/objects/res_obsolete.csv")

gg <- ggplot() +
  theme_light(base_size = 7) +
  ggtitle("Goal for rate following offensive faceoffs") +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), legend.box = "horizontal", legend.background = element_rect(fill = "transparent"), panel.grid.minor = element_blank()) +
  guides(linetype = FALSE) +
  scale_color_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  # scale_linetype_manual(name = "Time variable (t*)",labels = c("log(t+1)", "t"), values = c(1, 2)) +
  coord_cartesian(xlim = c(0,max_time), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("model-based goal for rate (%)") +
  geom_line(data=res_final,
            aes(x=time_since_faceoff, y=100*mean, col=faceoff_won, linetype = time_variable), size=.35) +
  geom_ribbon(data=res_final,
              aes(x=time_since_faceoff, y=100*mean, ymin = 100*lower, ymax = 100*upper, fill=faceoff_won),
              alpha=0.15, col=NA) +
  geom_line(data=res_obsolete,
            aes(x=time_since_faceoff, y=100*mean, col=faceoff_won, linetype = time_variable), size=.35, alpha = .5)# +
  # geom_ribbon(data=res_obsolete,
  #             aes(x=time_since_faceoff, y=100*mean, ymin = 100*lower, ymax = 100*upper, fill=faceoff_won),
  #             alpha=0.15, col=NA)
gg
if(save_figures) ggsave("report/figures/curve_off_nhl.png", gg, "png", width=3, height=2, units="in", dpi=320)
