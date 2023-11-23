### working hours and foodwaste correlation

### libraries and packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
install.packages("readxl")
library(readxl)

### source data

df_wh <- read.csv("working_hours.csv")
df <- read_excel("fw_Data.xlsx")

### data frames

# choosing the latest average annual working hours per worker (year 2017)

ave_wh_by_country <- df_wh %>% 
  filter(Year == 2017) %>% 
  select(Entity, Average.annual.working.hours.per.worker)


food_waste_by_country <- df %>% 
  select(Countrry, `Annual kg per Capita`)

prepared <- inner_join(ave_wh_by_country, food_waste_by_country, by = join_by( Entity == Countrry)) %>% 
  mutate(weekly = Average.annual.working.hours.per.worker / 37.5) 

colnames(prepared) <- c("Country", "Average_annual_working_hours_per_worker", 
                        "foodwaste", "Average_weekly_working_hours_per_worker")

prepared$continent <- countrycode(sourcevar = prepared[, "Country"],
                                  origin = "country.name",
                                  destination = "continent")


### plot

plot <- ggplot(prepared, aes( x = Average_weekly_working_hours_per_worker, y = foodwaste, color = continent)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE, color = "grey50") +
  labs(
       y="Annual food waste per capita, kg", 
       x="Average weekly working hours per worker", 
       title="Correlation between food waste and number of working hours", 
       ) +
  theme_bw() +
  theme(
    text = element_text(color = "black"),
    axis.line = element_line(color="black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    plot.title = element_text(size=10, color="black"),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", face = "bold", size = 9),
    panel.grid.major = element_line(color="black"),
    panel.grid.minor = element_line(color="black"),
    legend.key  = element_blank(),
    legend.box = element_blank()
    
  ) + scale_color_manual(
    values = c("Asia" = "tomato2", "Europe" = "springgreen4", 
               "Africa" = "brown4", "Americas" = "gold1", 
               "Oceania" = "black"))

plot


ggsave("workh.png", plot = plot, bg = "transparent",
       width = 6, height = 3, dpi = 250)

