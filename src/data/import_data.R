library(data.table)

import_data <- function() {
  
    url <- "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv"
    data <- fread(url)
    
    setnames(data, old = c("X Coordinate", "Y Coordinate"), new = c("x_coordinate", "y_coordinate"))
  
}
