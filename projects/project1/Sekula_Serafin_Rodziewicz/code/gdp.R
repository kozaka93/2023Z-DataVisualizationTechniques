### Dependence of food waste [kg] per capita on GDP [USD * 10^3] per capita 
### and greenhouse gas emissions [t] per capita

### libraries and packages 
library(ggplot2)
library(dplyr)
library(readxl)

### source data 
food_waste <- read_excel("data/fw_Data.xlsx")
ds_em <- read.csv("data/co-emissions-per-capita.csv")
ds_poverty <- read.csv("data/gdp 2.csv", sep = ";")

### data frames 
df_fw <- food_waste %>% 
  select(Countrry, `Annual kg per Capita`) %>% 
  rename(Food_waste = `Annual kg per Capita`) %>% 
  rename(Country = Countrry)

df_avg_emission <- ds_em %>% 
  filter(Year == 2021) %>% 
  select(Entity, Annual.CO..emissions..per.capita.) %>% 
  rename(Country = Entity, Gh_emissions = Annual.CO..emissions..per.capita.)

df_pov <- ds_poverty %>% 
  select(Country.Name, X2020) %>% 
  rename(Country = Country.Name, GDP = X2020) 

# join
df_em_fw <- df_avg_emission %>% 
  left_join(df_pov, by = "Country") %>% 
  left_join(df_fw, by = "Country") %>% 
  na.omit() %>% 
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country)) %>% 
  mutate(Country = ifelse(Country == "United States", "USA", Country)) %>% 
  filter(Country != "Bermuda") 

# podpisy
napisy <- c("Nigeria", "Qatar", "Denmark", "Botswana", "Somalia", "Japan", "Slovenia", "Angola")
text_data_nigeria <- subset(df_em_fw, Country %in% napisy)

### plot
p <- ggplot(df_em_fw, aes(x = GDP, y = Food_waste)) +
  geom_point(aes(color= Gh_emissions), size = 3) +
  geom_smooth(method = "lm", formula = y ~ log(x), color = "#c6b7cc", se = FALSE) +
  scale_x_log10(labels = scales::label_number(scale = 1e0, suffix = "")) +
    #geom_text(vjust = 2, hjust = 0, size = 3) +
  #scale_x_continuous(limits = c(0, 60)) +
  scale_color_gradientn(colors = c("springgreen4", "gold1", "tomato3"), 
                        limits = c(0, 36)) +
  labs(x = "log(GDP per capita in USD)",
       y = "Food waste per capita in kg",
       color = "Green house\ngases emission\nper capita in tones",
       title = "Dependence of food waste [kg] per capita on GDP [USD * 10^3] per capita 
and greenhouse gas emissions [t] per capita") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.minor = element_line(color = "black"),
    panel.grid.major = element_line(color = "black"),
    plot.title = element_text(size = 14, color="black"),
    legend.title = element_text(color = "black", size = 14),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_text(color = "black", size = 14)
    ) +
  geom_text(data = text_data_nigeria, aes(label = Country), 
            vjust = -0.7, hjust = 0.2, size = 6)


p

ggsave("pkb.png", plot = p, width = 8, height = 6.6, units = "in")


