library(ggplot2)

dane <- data.frame(
  Partia = c("Bezpartyjni Samorządowcy", "KKW Trzecia Dorga\n PSL-PL2050 Szymona Hołowni",
            "KW Nowa Lewica", "KW Prawo i Sprawiedliwość", "KW Konfederacja Wolność\n i Niepodległość",
            "KKW Koalicja Obywatelska PO\n .N IPL Zieloni", "KW Polska jest jedna"),
  Glosy = c(165, 2190, 2887, 3840, 1144, 8350, 341)
)

dane$Procent <- round((dane$Glosy / sum(dane$Glosy)) * 100, 2)
dane$Partia <- factor(dane$Partia, levels = dane$Partia[order(dane$Procent, decreasing = TRUE)])

ggplot(dane, aes(x = Partia, y = Procent, fill = Partia)) +
  geom_bar(stat = "identity", fill="#0077b6") +
  labs(title = "Wyniki wyborów do sejmu RP- głosowanie we Francji ",
       x = "Partia polityczna",
       y = "Poparcie") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()+theme(legend.position = "none",
                        axis.title.x = element_text(vjust = -0.4))+
  geom_text(aes(label = sprintf("%.2f%%", Procent), y = Procent), vjust = -0.8)
  
  