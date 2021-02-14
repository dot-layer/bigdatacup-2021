library(data.table)

data_nwhl <- fread("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv")
data_scouting <- fread("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
data_womens <- fread("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv")

data_nwhl[, dataset := "nwhl"]
data_scouting[, dataset := "scouting"]
data_womens[, dataset := "womens"]

data <- rbindlist(list(data_nwhl, data_scouting, data_womens))
setnames(data, function(old) {tolower(gsub(" ", "_", old, fixed = TRUE))})
setcolorder(data, "dataset")

saveRDS(data, "data/data.rds")
