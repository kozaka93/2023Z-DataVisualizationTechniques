library(ggplot2)

data <- data.frame(
  Partia = c("PRAWO I SPRAWIEDLIWOŚĆ", "KOALICJA OBYWATELSKA", "TRZECIA DROGA", "LEWICA", "KONFEDERACJA", "BEZPARTYJNI SAMORZĄDOWCY"),
  Procent = c(36.8, 31.6, 13.0, 8.6, 6.2, 2.4)
)

# do generowania wykresu w kolorach
ggplot(data, aes(x = reorder(Partia, -Procent), y = Procent, fill = Partia)) +
  geom_col() +
  scale_fill_manual(values = c("#00008B", "#DC143C", "darkgreen", "#FF69B4", "#FFD700", "black"), 
                    breaks = c("PRAWO I SPRAWIEDLIWOŚĆ","KOALICJA OBYWATELSKA", "TRZECIA DROGA",  "LEWICA", "KONFEDERACJA", "BEZPARTYJNI SAMORZĄDOWCY")) +
  labs(
    title = "Wyniki wyborów do Sejmu",
    x = "",
    y = "Procent głosów"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2, size = 11, family = "Tahoma"), panel.grid.major = element_blank()) +
  geom_text(aes(label = paste0(Procent, "%")), vjust = -0.5, size = 5, color = "black", fontface = "bold",  family = "Tahoma") +
  ggtitle("Wyniki wyborów do Sejmu w 2023 roku") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, family = "Tahoma"),
        plot.title.position = "plot")+
  guides(fill = FALSE)

# do generowania wykresu bez kolorów
ggplot(data, aes(x = reorder(Partia, -Procent), y = Procent)) +
  geom_col(fill = "#49423D")+
  labs(
    title = "Wyniki wyborów do Sejmu",
    x = "",
    y = "Procent głosów"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1.2, size = 11, family = "Tahoma"), panel.grid.major = element_blank()) +
  geom_text(aes(label = paste0(Procent, "%")), vjust = -0.5, size = 5, color = "black", fontface = "bold",  family = "Tahoma") +
  ggtitle("Wyniki wyborów do Sejmu w 2023 roku") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, family = "Tahoma"),
        plot.title.position = "plot")+
  guides(fill = FALSE)


