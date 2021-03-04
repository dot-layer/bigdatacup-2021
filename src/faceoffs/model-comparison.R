# Compares models built in:
#   dev-model-with-weights-at-zero.R
#   analysis-gam-nhl.R

# Ce serait plus clean d'avoir en mémoire les résultats du model-weights pour
# éviter de load le data, mais on ne devrait plus en avoir besoin.


##########
# Packages ----------------------------------------------------------------
##########

library(data.table)
library(mgcv)
library(ggplot2)


############
# Bar charts --------------------------------------------------------------
############

sqo <- fread("src/faceoffs/objects/bin_off_nhl.csv")

ggplot(sqo,
             aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time+1), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .2, position = "dodge", col="gray15") +
  facet_wrap(~ faceoff_won)



#############################
# Results for mod_weights_nhl ---------------------------------------------
#############################

mod <- readRDS("src/faceoffs/objects/mod_weights_nhl.rds")
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



ggplot(sqo,
       aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time+1), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .2, position = "dodge", col="gray15") +
  geom_line(data = res_table, aes(x=time_since_faceoff, y=100*mean, col=faceoff_won)) +
  geom_ribbon(data = res_table, aes(x=time_since_faceoff, y=100*mean,
                  ymin = 100*lower, ymax = 100*upper, fill=faceoff_won), alpha=0.15, col=NA) +
  facet_wrap(~ faceoff_won)



################################################
# Results for mod_final_nhl and mod_obsolete_nhl --------------------------
################################################

res_final <- fread("src/faceoffs/objects/res_final.csv")
res_obsolete <- fread("src/faceoffs/objects/res_obsolete.csv")

ggplot(sqo,
       aes(x = bin, y = y, fill = faceoff_won)) +
  ggtitle("Goal for rate following offensive faceoff") +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_manual(labels = c("FO lost", "FO won"), values = c("red", "green")) +
  coord_cartesian(xlim = c(-1,max_time+1), ylim = c(0,.35)) +
  xlab("time elapsed since faceoff") +
  ylab("goals/sequences (%)") +
  geom_col(alpha = .2, position = "dodge", col="gray15") +
  # geom_line(data = res_table, aes(x=time_since_faceoff, y=100*mean, col=faceoff_won)) +
  # geom_ribbon(data = res_table, aes(x=time_since_faceoff, y=100*mean,
  #                                   ymin = 100*lower, ymax = 100*upper, fill=faceoff_won), alpha=0.15, col=NA) +
  geom_line(data=res_final,
            aes(x=time_since_faceoff, y=100*mean, col=faceoff_won, linetype = time_variable)) +
  geom_ribbon(data=res_final,
              aes(x=time_since_faceoff, y=100*mean, ymin = 100*lower, ymax = 100*upper, fill=faceoff_won),
              alpha=0.15, col=NA) +
  geom_line(data=res_obsolete,
            aes(x=time_since_faceoff, y=100*mean, col=faceoff_won, linetype = time_variable), alpha = .5) +
  geom_ribbon(data=res_obsolete,
            aes(x=time_since_faceoff, y=100*mean, ymin = 100*lower, ymax = 100*upper, fill=faceoff_won),
            alpha=0.15, col=NA) +
facet_wrap(time_variable ~ faceoff_won)

