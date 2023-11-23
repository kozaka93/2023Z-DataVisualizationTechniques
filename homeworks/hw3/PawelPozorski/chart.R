library(dplyr)
library(ggplot2)
library(maps)
library(colorspace)

# load data
data <- as.data.frame(read.csv("data.csv"))
us_map <- map_data("state")

chart_data <- data %>%
  mutate(`State`=tolower(`State`)) %>% 
  group_by(`State`) %>% 
  summarize(`TotalFatalities`=sum(`Fatalities`)) %>% 
  right_join(us_map, by=c("State"="region"))

plot <- ggplot(chart_data, aes(x = `long`, y = `lat`, group = `group`, fill = `TotalFatalities`)) +
  geom_polygon(color = "lightblue") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgray") +
  theme_void() +
  labs(title = "Total Fatalities from 1990 by State") +
  theme(plot.background = element_rect(fill = "lightblue")) +
  theme(legend.position = "right",  plot.title = element_text(face = "bold", size = 18, hjust = 0.5, vjust = 0.5)) + 
  guides(fill = guide_colorbar(title = "", barwidth = 1, barheight = 10))

ggsave("plot.png", plot = plot, width=7, height=5) 

