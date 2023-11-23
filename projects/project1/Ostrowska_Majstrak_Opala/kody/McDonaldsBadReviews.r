setwd("C:/Users/User/Desktop/IAD/TWD/PrototypMCD")

negative <- read.csv("Reviews.csv")
urls <- read.csv("urls.csv")

install.packages("fmsb")
library(fmsb)

data1 <- negative

attributes_list <- c(
  "RudeService",
  "OrderProblem",
  "Filthy",
  "BadFood",
  "Cost",
  "SlowService",
  "ScaryMcDs"
)

for (attribute in attributes_list) {
  negative[, attribute] <- as.numeric(grepl(attribute, negative$policies_violated, ignore.case = TRUE))
}

filtered_df <- negative[, attributes_list]
column_sums <- colSums(filtered_df)
result_df <- data.frame(
  Attribute = names(column_sums),
  SumOfOnes = column_sums
)

transposed_result_df <- as.data.frame(t(result_df))
colnames(transposed_result_df) <- transposed_result_df[1, ]
transposed_result_df <- transposed_result_df[-1, ]

radar_data <- data.frame(
  row.names = NULL,
  attributes = colnames(transposed_result_df),
  values = as.numeric(unlist(transposed_result_df))
)

library(scales)
library(dplyr)
library(scales)
radar_data <- as.data.frame(t(result_df$SumOfOnes))
radar_data[1, ] <- radar_data[1, ]/1525 *100
colnames(radar_data) <- attributes_list
radar_data <- rbind(rep(33.5082,7) , rep(0,7) , radar_data)
radar_data1 <- radar_data %>% select(-ScaryMcDs)

svg("radar_chart.svg", bg = "transparent")
radarchart(radar_data1, axistype = 1,
           pcol = "goldenrod", pfcol = alpha("gold", 0.75), plwd = 4,
           cglcol = "grey", cglty = 1, axislabcol = "gray",
           caxislabels = seq(0, 100, by = 10),
           cglwd = 0.8,
           vlcex = 0.7)
dev.off()
