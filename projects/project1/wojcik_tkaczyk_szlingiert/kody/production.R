library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
options(scipen = 12)


df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")

df %>%
  filter(Area.Code < 300) %>%
  filter(
    stri_detect_regex(Item, "oil") &
      !stri_detect_regex(Item, "seeds") &
      !stri_detect_regex(Item, "Saff")
  ) %>%
  group_by(Item, Element, Unit) %>%
  summarise(across(Y1961:Y2020, ~ sum(.x, na.rm = TRUE))) -> df2

df2 %>% 
  filter(Element == "Production") %>% 
  pivot_longer(matches("Y[0-9]{4}"), names_to = "year") %>% 
  mutate(year = as.double(substr(year,2,length(year))),
         value = value/1000000)-> df3
  
colnames(df3)<- c("Oil", "Element", "Unit",    "year" ,  "value"  )
  
ggplot(df3, aes(
  x = year,
  y = value,
  color = Oil,
  shape = Oil
)) +
  geom_vline(xintercept = c(2020,seq(1961,2020,4)),linetype = "longdash", color = "#303030")+
  geom_hline(yintercept = seq(0,80,2),linetype = "dotted", color = "#303030")+
  geom_point(size = 2.7) +
  geom_line() +
  scale_shape_manual(values = c(0, 1, 2, 3, 16, 7, 8, 9, 10)) +
  scale_color_manual(
    values = c(
      "#77777e",
      "#777b7e",
      "#777b7e",
      "#777b7e",
      "#ff3300",
      "#777b7e",
      "#777b7e",
      "#777b7e"
    )
  ) +
  theme(
    text = element_text(colour = "white", size = 16),
    axis.text = element_text(colour = "white"),
    axis.line = element_line(colour = "#303030"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.minor = element_blank(),
    #panel.grid.minor = element_line(color = "#303030"),
    panel.grid.major = element_line(color = "#303030"),
    panel.background = element_rect(fill = "transparent"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(0.5,"cm"),
  ) +
  xlab("Year") +
  ylab("Production (million tonnes)") +
  scale_y_continuous(expand = c(0,0), limits = c(0,82), breaks = seq(0,80,20))+
  scale_x_continuous(expand = c(0,0), 
                     limits = c(1961,2022) ,
                     breaks = c(2020,seq(1961,2020,12)))-> p

ggsave("plot_trasparent1px.png", plot = p, bg = "transparent", width = 12, height = 6)

  

