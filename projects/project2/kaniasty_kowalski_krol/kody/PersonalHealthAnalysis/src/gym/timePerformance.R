library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)

data <- read.csv("./data/gym/predki.csv")

# Przygotowanie danych
data <- data %>%
  mutate(
    workout_datetime = as.POSIXct(start_time, format = "%d %b %Y, %H:%M"),
    session_type = ifelse(hour(workout_datetime) < 12, "Morning", ifelse(hour(workout_datetime) < 18, "Afternoon", "Evening")),
    total_weight = weight_kg * reps / 1000,
    scaled_reps = reps / 100 # Scale down the reps for plotting purposes
  ) %>%
  group_by(muscle_group, session_type) %>%
  summarize(
    average_total_weight = mean(total_weight, na.rm = TRUE),
    total_reps = sum(reps, na.rm = TRUE),
    average_scaled_reps = mean(scaled_reps, na.rm = TRUE) # Average of scaled reps
  )

# Define color palettes
weight_colors <- c("Morning" = "blue", "Afternoon" = "green", "Evening" = "red")
rep_colors <- c("Morning" = "lightblue", "Afternoon" = "lightgreen", "Evening" = "pink")

# Tworzenie wykresu
plot <- ggplot(data) +
  geom_bar(aes(x = muscle_group, y = average_total_weight, fill = session_type),
           stat = "identity", position = position_dodge(width = 0.9), alpha = .5) +
  scale_fill_manual(values = weight_colors, name = "Session Type") +
  geom_bar(aes(x = muscle_group, y = average_scaled_reps, fill = session_type),
           stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
  scale_y_continuous(
    name = "Average Total Weight Lifted [Tonnes]",
    sec.axis = sec_axis(~ . * 100, name="Average Total  [100]") # Secondary axis for reps
  ) +
  labs(x = 'Muscle Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Wyświetlenie wykresu
print(plot)
