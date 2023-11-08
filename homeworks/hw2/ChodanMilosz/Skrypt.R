library(ggplot2)
library(dplyr)

Data <- data.frame(
  Countries = c("Polska", "Niemcy", "Holandia", "Austria", "UE", "Strefa Euro", "Francja", "Włochy", "Hiszpania"),
  Unemployment_rate = c(2.8, 3.0, 3.6, 5.3, 5.9, 6.4, 7.3, 7.3, 11.5)
)
Data <- Data[order(Data$Unemployment_rate), ]
Data$Countries <- factor(Data$Countries, levels = Data$Countries)

wykres_slupkowy <- ggplot(Data, aes(x = Countries, y = Unemployment_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Stopa bezrobocia w różnych państwach",
       x = "",
       y = "Stopa bezrobocia (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_slupkowy)
ggsave("wykres_slupkowy.png", plot = wykres_slupkowy, width = 10, height = 6, units = "in", dpi = 300)