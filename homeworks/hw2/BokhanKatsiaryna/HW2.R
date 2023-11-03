library(ggplot2)

data <- data.frame(
  Komitet = c("PRAWO I SPRAWIEDLIWOŚĆ", "KOALICJA OBYWATELSKA", "TRZECIA DROGA", "NOWA LEWICA", "KONFEDERACJA", "BEZPARTYJNI SAMORZĄDOWCY"),
  Procent = c(36.8, 31.6, 13.0, 8.6, 6.2, 2.4)
)

ggplot(data, aes(x = reorder(Komitet, Procent), y = Procent)) +
  geom_col(fill = "#537072")+
  labs(
    title = "Wyniki wyborów do Sejmu",
    x = "Komitet wyborczy",
    y = "Procent głosów"
  )+
  theme_minimal()+
  coord_flip() + 
  ggtitle("Wyniki wyborów do Sejmu w 2023 roku")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, family = "Georgia"),
        plot.title.position = "plot",
        axis.text = element_text(size =10, family = "Georgia"),
        axis.title = element_text(size = 10, family = "Georgia"))+
  geom_text(aes(label = paste0(Procent, "%")),vjust=0.1, hjust=-0.1, size = 3.8, color = "black", fontface = "bold",  family = "Georgia")
