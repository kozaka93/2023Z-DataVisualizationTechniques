## PROJECT 1

library(ggplot2)
library(dplyr)
library(extrafont)


## Coffee consumption vs happiness 

# install.packages("ggflags", repos = c(
#   "https://jimjam-slam.r-universe.dev",
#   "https://cloud.r-project.org"))

library(ggflags)

caff_consumption = read.csv("Top Countries in Global Coffee Consumption - Sheet1.csv")
country_raport <- read.csv("world-happiness-report-2019.csv")
iso <- read.csv("wikipedia-iso-country-codes.csv")


caff_consumption %>%
  left_join(country_raport, by = c("Country" = "Country..region.")) %>%
  na.omit() %>%
  left_join(iso, by = c("Country" = "English.short.name.lower.case")) %>%
  mutate(Coffee.Consumed.Per.Capita.kg = (Coffee.Consumed.Per.Capita..lbs. * 0.4536)) %>% 
  mutate(alpha2 = tolower(Alpha.2.code)) %>% 
  select(Country, Coffee.Consumed.Per.Capita.kg, Positive.affect, alpha2) -> data

data %>% 
  ggplot() +
  geom_smooth(aes(x = Coffee.Consumed.Per.Capita.kg, y = Positive.affect), 
              method=lm , color = "#6B1D0C", se=FALSE, linewidth = 1.5) +
  geom_point(aes(x = Coffee.Consumed.Per.Capita.kg, y = Positive.affect), size = 8, color = "grey30") +
  geom_flag(aes(x = Coffee.Consumed.Per.Capita.kg, y = Positive.affect, country = alpha2), size = 6.5) +
  scale_country() +
  scale_x_continuous(limits = c(4, max(data$Coffee.Consumed.Per.Capita.kg))) +
  theme(legend.position = "none") + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "black", linetype = "longdash"),
        axis.text = element_text(color = "black", family = "Bell MT", size = 16),
        axis.title = element_text(color = "black", family = "Bell MT", size = 16),
        plot.title = element_text(color = "black", family = "Bell MT", size = 22, hjust = 0.5)) +
  xlab("Coffee Consumed per capita (kg)") +
  ylab("Measure of positive emotion") +
  ggtitle("Coffee vs Happiness")

ggsave("my_plot.png", device = "png", bg = "transparent", width = 6.88, height = 5.7)


