library(ggplot2)
library(dplyr)


Rok = c(rep(2012,4), rep(2013, 4), rep(2014, 4), rep(2015, 4), rep(2016, 4), rep(2017, 4), rep(2018, 4), rep(2019, 4), rep(2020, 4), rep(2021, 4), rep(2022, 4), rep(2023, 2))
Kwartal = c(rep(c("Q1", "Q2", "Q3", "Q4"), 11), "Q1", "Q2")
Bezrobotni = c(14600, 14500, 14400, 14450 ,14550, 14450, 14350, 14380, 14300, 14250, 14100, 14200, 14300, 14310, 14100, 14000, 14000, 13900, 13850, 13800, 13800, 13650, 13600, 13780, 13765, 13600, 13400, 13780, 13700, 13500, 13380, 13650, 13650, 13800, 13500, 13510, 13000, 12900, 12700, 12760, 12750, 12750, 12780, 12600, 12550, 12600)
Stopa_bezrobocia = c(46.2, 46, 45.9, 45.9, 46.1, 45.9, 45.7, 45.8, 45.7, 45.6, 45.2, 45.4, 45.6, 45.7, 45.1, 45, 45, 44.9, 44.8, 44.65, 44.65, 44.3, 44.2, 44.65, 44.65, 44.1, 43.8 , 44.3, 44.3, 44, 43.8, 44.3, 44.3, 44.8, 44, 44, 43, 42.8, 42.4, 42.5, 42.5, 42.5, 42.5, 42.3, 42.1, 42.2)
library(ggthemes)

df <- data.frame(Rok, Kwartal, Bezrobotni, Stopa_bezrobocia)

xaxis <- factor(paste0(Rok, " ", Kwartal), levels = unique(paste0(Rok, " ", Kwartal)), ordered = TRUE)

ggplot(df, aes(x = xaxis, y = Bezrobotni) ) +
  geom_col(width = 0.6, fill = "#0602ac", ) +
  labs(x = "", y = "Ilosc osób biernych zawodowo (tys)", title = "Bierni zawodowo w wieku 15-89 lat") +
  scale_y_continuous(limits = c(0,15000), breaks = seq(0, 15000, by = 1000)) +
  theme_tufte() +
  theme(
     plot.title = element_text(family = "mono", size = 20, hjust = 0.5),
     axis.title = element_text(family = "mono", size = 12),
     axis.text = element_text(family = "mono", size = 10),
     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) 


ggplot(df, aes(x = xaxis, y = Stopa_bezrobocia)) +
  geom_point(color = "#190672") +
  geom_line(aes(y = Stopa_bezrobocia, group = 1), color = "#190672") +
  labs(x = "", y = "Stopa bezrobocia (%)", title = "Udział biernych zawodowo w ludności w wieku 15-89 lat") +
  scale_y_continuous(expand = c(10,1)) +
  theme_minimal() +
  theme(
     plot.title = element_text(family = "mono", size = 17, hjust = 0.5),
     axis.title = element_text(family = "mono", size = 12),
     axis.text = element_text(family = "mono", size = 10),
     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
   )

