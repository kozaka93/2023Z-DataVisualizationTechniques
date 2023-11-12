library(ggplot2)
library(tidyr)

data <- read.csv("./repo/homeworks/hw2/KaniastyAdam/political_party_preferences.csv")

data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Transform data from wide to long format
data_long <- pivot_longer(
  data,
  cols = -Date,
  names_to = "Party",
  values_to = "Preference"
)

# Define the events data frame
events <- data.frame(
  Date = as.Date(c('2019-10-13', '2020-10-22', '2022-02-24')),
  Event = c('2019 general election', 'Court ruling against EU law', 'Russian invasion')
)


# Base plot with the light color palette
ggplot(data_long, aes(x = Date, y = Preference, color = Party)) +
  geom_line(size = .5) + # Make lines thicker
  geom_vline(data = events, aes(xintercept = Date), linetype = "dashed") +
  geom_text(data = events, aes(x = Date, y = max(data_long$Preference), label = Event),
            angle = 90, vjust = -0.5, color = "grey50", size = 5, inherit.aes = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  expand_limits(y = c(0, max(data_long$Preference) + 5)) +
  labs(title = "Simulated Political Party Preferences Over Time",
       x = "Date",
       y = "Preference (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(size = 14), # Increase legend title size
        legend.text = element_text(size = 12)) # Increase legend text size
