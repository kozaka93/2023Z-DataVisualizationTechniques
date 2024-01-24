library(dplyr)
library(readr)
# setwd("./projekty/health")
data <- read.csv("./data/gym/predki.csv")

map <- read.csv("./src/gym/utils/map.csv")

merged_data <- left_join(data, map, by = "exercise_title")

write_csv(merged_data, "./data/gym/predki.csv")
