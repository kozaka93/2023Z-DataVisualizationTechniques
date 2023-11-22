library(ggplot2)

setwd("C:\\TWD\\Prace domowe\\Praca domowa 2\\RowickiMikołaj")

df <- data.frame(
  KomitetWyborczy =  c("Prawo i\n Sprawiedliwość", "Koalicja\n Obywatelska", 
                       "Trzecia Droga", "Konfederacja", "Lewica", 
                       "Polska jest jedna", "Bezpartyjni\n Samorządowcy"),
  LiczbaGlosow = c(6658, 4729, 3776, 1488, 1040, 389, 295),
  ProcentGlosow = c(36.2, 25.7, 20.6, 8.1, 5.7, 2.1, 1.6)
)

p <- ggplot(df, aes(x = reorder(KomitetWyborczy, -ProcentGlosow), 
                    y = ProcentGlosow)) +
  geom_bar(stat = "identity", fill = alpha("blue", 0.8), 
           position = position_dodge(width = 10)) +
  ggtitle("Oficjalne wyniki wyborów do sejmu w gminie Niepołomice") +
  labs(x = "Komitet wyborczy",
       y = "Procent głosów") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "gray", size = 0.1)) +
  geom_text(aes(label = LiczbaGlosow), 
            vjust = -0.5, size = 3, color = "#4D4D4D") +
  scale_y_continuous(labels = scales::label_percent(scale = 1), 
                     limits = c(0, 40))
  
ggsave("plot.png", plot =  p, width = 8 )
  
  
