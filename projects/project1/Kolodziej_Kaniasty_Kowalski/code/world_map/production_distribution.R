library(dplyr)
library(tidyr)
library(ggplot2)

source("./projekty/coffee/code/predki_kod/load.r")

biggest_production_countries <- total_production %>%
  rowwise() %>%
  mutate(mean_value = mean(c_across(X2010:X2018), na.rm = TRUE)) %>%
  arrange(desc(mean_value)) %>%
  select(total_production) %>%
  rename(country_name = total_production) %>%
  head(5)

top5_production <- total_production %>%
  filter(total_production %in% biggest_production_countries$country_name) %>%
  select(total_production, X2010:X2018) %>%
  rename(country_name = total_production) %>%
  pivot_longer(cols = X2010:X2018, names_to = "year", values_to = "value")

top5_export <- exports_calendar_year %>%
  filter(exports %in% biggest_production_countries$country_name) %>%
  select(exports, X2010:X2018) %>%
  rename(country_name = exports) %>%
  pivot_longer(cols = X2010:X2018, names_to = "year", values_to = "value")

top5_domestic_consumption <- domestic_consumption %>%
  filter(domestic_consumption %in% biggest_production_countries$country_name) %>%
  select(domestic_consumption, X2010:X2018) %>%
  rename(country_name = domestic_consumption) %>%
  pivot_longer(cols = X2010:X2018, names_to = "year", values_to = "value")

top5_merged <- merge(top5_production, top5_export, by = c("country_name", "year")) %>%
  rename(production = value.x, export = value.y)

top5_merged <- merge(top5_merged, top5_domestic_consumption, by = c("country_name", "year")) %>%
  rename(domestic_consumption = value)

top5_merged <- top5_merged %>%
  mutate(production = production,
         export = export / production * 100,
         domestic_consumption = domestic_consumption / production * 100)

ggplot(top5_merged, aes(x = year, color = country_name, group = country_name)) +
  geom_line(aes(y = export, linetype = "Export"), size = 1) +
  geom_point(aes(y = export), size = 2) +
  geom_area(aes(y = export, fill = country_name), alpha = 0.2, show.legend = FALSE) +

  geom_line(aes(y = domestic_consumption, linetype = "Domestic Consumption"), size = 1, alpha = 0.6) +
  geom_point(aes(y = domestic_consumption), size = 2, alpha = 0.5) +
  geom_area(aes(y = domestic_consumption, fill = country_name), alpha = .5, show.legend = FALSE) +

  labs(title = "Export and Domestic Consumption Percentages Over Years",
       y = "Percentage", x = "Year", color = "Country", linetype = "Metric") +
  scale_x_discrete(breaks = unique(top5_merged$year)) +
  facet_wrap(~country_name, ncol = 1, scales = "free_y") +
  theme_minimal()
