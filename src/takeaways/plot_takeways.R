library(data.table)
library(ggplot2)


# source("src/takeaways/utils/create_rink.R")
source("src/takeaways/utils/geom_shotplot.R")


# Import data -------------------------------------------------------------

source("src/data/import_data.R")
data <- import_data()

data[,time_period := as.POSIXct(Clock, format="%M:%S")]

# Plot shots --------------------------------------------------------------

ggplot(data[Event == "Takeaway",], aes(x = x_coordinate, y = y_coordinate)) +
  geom_shotplot() +
  labs(title = "Takeaway locations") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


# Identify critical takeways ----------------------------------------------

TIME_WINDOW <- 10
data[, scoring_chance := NA]

for (row in seq_len(nrow(data))) {
  
  if (data[row,]$Event == "Takeaway") {
    
    # Extract basic infos
    takeaway_date <- data[row,]$game_date
    takeaway_period <- data[row,]$Period
    takeaway_clock <- data[row,]$time_period
    takeaway_team <- data[row,]$Team
    
    # Filter data in the time window
    dt_temp <- data[game_date == takeaway_date &
                    Period == takeaway_period & 
                    time_period >= takeaway_clock - TIME_WINDOW &
                    time_period < takeaway_clock &
                    Team == takeaway_team,]
    
    if (any(dt_temp$Event == "Shot")) {
      
      data[row,]$scoring_chance <- TRUE
      
    } else {
      
      data[row,]$scoring_chance <- FALSE
      
    }
    
  }
  
}

table(data[Event == "Takeaway",]$scoring_chance)

ggplot(data[Event == "Takeaway",], aes(x = x_coordinate, y = y_coordinate, color = scoring_chance)) +
  geom_shotplot() +
  labs(title = "Takeaway locations") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
