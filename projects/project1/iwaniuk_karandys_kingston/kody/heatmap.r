library(dplyr)
library(ggplot2)

data <- read.csv('./orders.csv')
head(data)

orders_count <- data %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(order_count = n())

# Custom color palette from black to gold
colors = c("black", "gold")

my_plot <- ggplot(orders_count, aes(x = order_hour_of_day, y = order_dow, fill = order_count)) +
  geom_tile() +
  scale_fill_gradientn(colours = colors) + # Apply custom color gradient
  labs(x = "Hour of Day", y = "Day of Week", fill = "Number of Orders", title = "Hour / Day of week heatmap") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Fira Sans Condensed", size = 22, margin = margin(10, 0, 30, 0), color = "white"),
    text = element_text(family = "Fira Sans Condensed Light", size = 14, color = "white"),
    axis.text = element_text(size = 12, color = "white"),
    axis.title = element_text(size = 16, color = "white"),
    plot.background = element_rect(fill = "transparent", color = NA), # Transparent plot background
    panel.background = element_rect(fill = "transparent", color = NA), # Transparent panel background
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(color = "white"), # Legend text color
    legend.title = element_text(color = "white") # Legend title color
  ) +
  scale_y_continuous(
    breaks = 0:6, 
    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4))

# Save the plot with a transparent background and white font
ggsave("heatmap_transparent.png", plot = my_plot, bg = "transparent", device = "png", width = 10, height = 8, units = "in")