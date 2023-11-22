library(dplyr)
library(ggplot2)
library(mapdata)

minimum_wage <- read.csv("Minimum_Wage_Data.csv")

# filter for year 2020 and state minimal wage

minimum_wage_2020 <- minimum_wage %>% 
  filter(Year == 2020) %>% 
  select(State, State.Minimum.Wage)

#min(minimum_wage_2020$State.Minimum.Wage)
#max(minimum_wage_2020$State.Minimum.Wage)

minimum_wage_2020 <- minimum_wage_2020 %>% 
  mutate(group_of_minimal_wage = case_when(State.Minimum.Wage < 2 ~ 1,
                                           State.Minimum.Wage < 6 ~ 2,
                                           State.Minimum.Wage < 9 ~ 3,
                                           State.Minimum.Wage < 12 ~ 4,
                                           .default = 5)) %>% 
  rename(region = State) %>% 
  mutate(region = tolower(region)) %>% 
  select(region, group_of_minimal_wage)

states <- map_data("state") %>% 
  inner_join(minimum_wage_2020)

plot <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group, fill = factor(group_of_minimal_wage)),
               color = "black") +
  theme_void() +
  scale_fill_manual(values = c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"),
                    labels = c("< $2", "$2 to $6", "$6 to $9", "$9 to $12", "$12 to $14")) +
  coord_map("albers", 25, 50) +
  labs(title = "Minimum wage per hour in USA in 2020",
       fill = "Minimum wage",
       caption = "Source: https://www.kaggle.com/datasets/lislejoem/us-minimum-wage-by-state-from-1968-to-2017/") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.05),
        plot.caption = element_text(size = 7, color = "gray"))

plot
