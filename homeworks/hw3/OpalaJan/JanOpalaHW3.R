setwd("C:/Users/User/Desktop/IAD/TWD/HW3")

youtubers <- read.csv("YouTubers2.csv")

library(dplyr)
youtubers <- youtubers %>% 
  rename(country_code = Country) %>% 
  rename(subscribers = Suscribers)

country_codes_vector = unique(youtubers$country_code)


country_mapping <- data.frame(
  country_code = country_codes_vector,
  country = c(
    'India', 'United States', 'Japan', 'South Korea', 'Brazil', 'Argentina', 'United Kingdom', 'Chile', 'Russia', 'Belarus',
    'Mexico', 'El Salvador', 'Puerto Rico', 'Pakistan', 'Philippines', 'Cyprus', 'Colombia', 'Norway', 'Spain', 'United Arab Emirates',
    'Canada', 'Indonesia', 'Thailand', 'Ireland', 'Turkey', 'Italy', 'Kuwait', 'Jordan', 'Netherlands', 'Australia', 'Singapore',
    'Ecuador', 'Germany', 'Saudi Arabia', 'Latvia', 'Sweden', 'Ukraine', 'Finland', 'Switzerland', 'France', 'Vietnam', 'Malaysia',
    'Iraq', 'Egypt', 'Israel', 'Peru', 'Bangladesh', 'Hong Kong', 'Slovenia', 'Qatar', 'Portugal', 'Austria', 'Algeria'
  )
)

yt_data <- left_join(youtubers, country_mapping, by = "country_code") %>% 
  select(country, subscribers) %>% 
  group_by(country) %>% 
  summarise(max_subs = max(subscribers))

library(maps)
library(mapdata)
library(ggplot2)

world_map <- map_data("world")

yt_data <- yt_data %>% 
  rename(region = country) %>% 
  mutate(region = ifelse(region=="United States", "USA", region))

yt_map_data <- left_join(world_map, yt_data, by = "region") %>% 
  mutate(max_subs = as.numeric(gsub(",", "", max_subs))) %>% 
  filter(region != "Antarctica")

library(scales)

labels_to_add <- data.frame(
  lat = c(36, 35, 28),
  long = c(-78, 139, 77),
  names = c("Mr. Beast", "PewDiePie", "T-Series")
)

ggplot(data = yt_map_data, mapping = aes(x = long, y = lat, group = group, fill = max_subs)) +
  geom_polygon(color = "white") +
  labs(
    title = "Liczba subskrypcji najpopularniejszego YouTubera",
    subtitle = "Spośród 1000 YouTuberów o największej liczbie subskrypcji na świecie"
  ) +
  scale_fill_gradient(low = "#f7e4e4", high = "#FF0000", labels = comma) +
  guides(fill = guide_legend(title = "Liczba subskrypcji")) +
  theme_void() +
  geom_point(data = labels_to_add, aes(x = long, y = lat), inherit.aes = FALSE, color = "black", size = 4, shape = 19) +
  geom_linerange(data = labels_to_add, aes(x = long, ymin = lat-10, ymax = lat), inherit.aes = FALSE, color = "black") +
  geom_point(data = labels_to_add, aes(x = long, y = lat), inherit.aes = FALSE, color = "yellow", size = 3, shape = 19) +
  geom_rect(data = labels_to_add, aes(
  xmin = long - 20, xmax = long + 20,
  ymin = lat - 13, ymax = lat -7
), inherit.aes = FALSE, fill = "black", alpha = 0.8) +
  geom_text(data = labels_to_add, aes(x = long, y = lat-10, label = names), inherit.aes = FALSE, color = "white", size = 3)



