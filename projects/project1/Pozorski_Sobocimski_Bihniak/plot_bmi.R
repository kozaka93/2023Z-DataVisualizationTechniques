library(readxl)
library(ggplot2)
library(dplyr)
library(countrycode)
library(class)

data <- as.data.frame(read_excel("dane_glod.xlsx"))

colnames(data) <- data[3,]
data <- data[-c(1:3), ]
data <-
  data %>% mutate(Continent = countrycode(
    `Country ISO-3 Code`,
    origin = "iso3c",
    destination = "continent"
  ))

processed_data <- data %>%
  mutate(
    `Underweight` = as.numeric(`Underweight`),
    `Overweight` = as.numeric(`Overweight`),
    `Median Year` = as.numeric(`Median Year`)
  ) %>%
  filter(`Median Year` > 2005) %>%
  group_by(`Country Short Name`) %>%
  reframe(
    `MeanUnderweight` = mean(`Underweight`, na.rm = TRUE),
    `MeanOverweight` = mean(`Overweight`, na.rm = TRUE),
    `Continent`
  ) %>% 
  select(`MeanUnderweight`, `MeanOverweight`, `Continent`)
processed_data <- na.omit(processed_data)

grid <- expand.grid(
  `MeanUnderweight` = seq(min(processed_data$MeanUnderweight), max(processed_data$MeanUnderweight), length.out = 100),
  `MeanOverweight` = seq(min(processed_data$MeanOverweight), max(processed_data$MeanOverweight), length.out = 100)
)
grid$Continent <- knn(train = processed_data[, c("MeanUnderweight", "MeanOverweight")], test = grid, cl = processed_data$Continent, k = 3)

plot_bmi <- ggplot() +
  geom_point(data=processed_data, aes(x = `MeanUnderweight`, y = `MeanOverweight`, color = `Continent`), size=2) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  scale_x_continuous(breaks = seq(0, 40, by = 5)) +
  theme_minimal() +
  labs(
    x = "Mean Underweight (%)",
    y = "Mean Overweight (%)",
    title = "Underweight vs Overweight Children"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, vjust = 0.5)
  ) # + geom_raster(data = grid, aes(x = `MeanUnderweight`, y = `MeanOverweight`, fill=`Continent`), alpha=0.3)

ggsave("plot_bmi.png", plot = plot_bmi, dpi = 300)
