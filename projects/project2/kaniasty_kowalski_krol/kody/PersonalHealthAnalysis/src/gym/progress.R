library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)

gym_progess <- function(data){

data <- data %>%
  mutate(date = as.Date(start_time, format = "%d %b %Y")) %>%
  group_by(date, muscle_group) %>%
  summarize(average_weight = mean(weight_kg, na.rm = TRUE)) %>%
  ungroup()

plot <- ggplot(data, aes(x = date, y = average_weight, color = muscle_group)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = 'Data', y = 'Średnia waga [kg]', color = 'Grupa mięśniowa') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Progresja średniej wagi dla każdej grupy mięśniowej")

return(plot)
}