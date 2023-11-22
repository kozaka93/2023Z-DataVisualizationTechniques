library(ggplot2)
library(dplyr)
library(showtext)

df <- read.csv("../coffee/data/coffee_analysis.csv")

# View(df)

top_rated_coffees <- df %>%
  top_n(20, wt = rating) %>%
  select(name, roaster, loc_country, origin_1, rating, `X100g_USD`)

View(top_rated_coffees)

# Average price for every rating

avg_price <- df %>%
  group_by(rating) %>%
  summarise(avg_price = mean(`X100g_USD`)) %>%
  arrange(desc(rating))

View(avg_price)

plot_avg_price <- ggplot(avg_price, aes(x = rating, y = avg_price)) +
  geom_bar(stat = "identity") + 
  geom_smooth(method = loess, formula = y ~ x, se = F) + 
  labs(title = "Average Price for Every Rating",
       x = "Rating",
       y = "Average Price per 100g (USD)")

plot_avg_price

# Saving plot to png:
ggsave("../coffee/plots/avg_price.png", plot_avg_price, width = 14, height = 9, units = "cm")

# Increase in price (%) for 1 point increase in rating

avg_price %>%
  arrange(rating) %>%
  mutate(price_increase = (avg_price - lag(avg_price)) / lag(avg_price) * 100) %>%
  mutate(rating = paste0(as.character(lag(rating)), " > ", as.character(rating))) %>%
  filter(!is.na(price_increase)) %>%
  ggplot(aes(x = rating, y = price_increase, fill = price_increase)) +
  geom_bar(stat = "identity") +
  labs(title = "Increase in Price (%) for 1 Point Increase in Rating",
       x = "Increase in Rating",
       y = "Increase in Price (%)",
       fill = "") + 
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
  ) +
  scale_fill_viridis_c(option = "plasma")
  
# Saving plot to png:
ggsave("../coffee/plots/price_increase.png", width = 14, height = 9, units = "cm")

