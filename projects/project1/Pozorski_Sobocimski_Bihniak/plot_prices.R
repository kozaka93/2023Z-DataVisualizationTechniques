library(dplyr)
library(maps)
library(ggplot2)
library(countrycode)

data <- as.data.frame(read.csv("dane_ceny.csv"))

# prepare data - calculate mean price and encode date
data <- data %>%
  mutate(year = as.numeric(year(ymd(`date`))), month = as.numeric(month(ymd(`date`)))) %>%
  group_by(`CountryCode`, `commodity`, `year`, `month`) %>%
  summarise(`mean_price` = mean(`price`)) %>%
  ungroup() %>%
  mutate(
    `date` =  as.Date(paste(year, month, "01", sep = "-")),
    `country` = countrycode(`CountryCode`, origin = "iso3c", destination = "country.name")
  )

# prepare set of data that represent all needed entries
# (for each country for each product all possible dates)
products <- data %>% distinct(`commodity`)
countries <- data %>% distinct(`country`)
date_range <- data.frame(`year` = rep(2020:2021, each = 12),
                         `month` = rep(1:12, 2)) %>%
  mutate(`date` =  as.Date(paste(year, month, "01", sep = "-")))

date <- data.frame(
  `country` = rep(countries$country, each = dim(products)[1] * dim(date_range)[1]),
  `commodity` = rep(products$commodity,  dim(countries)[1], each = dim(date_range)[1]),
  `date` = rep(date_range$date, dim(products)[1] * dim(countries)[1])
)

# join it with our data to indicate missing data points
prices <- data %>%
  right_join(date,
             by = c(
               "country" = "country",
               "commodity" = "commodity",
               "date" = "date"
             )) %>%
  select(`country`, `commodity`, `date`, `mean_price`) %>%
  arrange(`country`, `commodity`, `date`) %>%
  mutate(`mean_price` = if_else(
    format(`date`, "%Y") == "2020",
    530 / (`mean_price` / 1000),
    560 / (`mean_price` / 1000)
  ))

filtered_prices <- prices %>%
  filter(`country` == "Uganda" &
           `commodity` %in% c("Maize", "Millet", "Rice"))

plot_prices <- ggplot(
  filtered_prices,
  aes(
    x = `date`,
    y = `mean_price`,
    group = `commodity`,
    color = `commodity`,
  )
) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "How much can you buy by average monthly salary in Uganda?",
       y = "kilograms of product",
       color = "") +
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +  # Adjust colors
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10)),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )

ggsave("plot_prices.png", plot = plot_prices, dpi = 300)