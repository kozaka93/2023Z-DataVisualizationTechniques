library(dplyr)
library(ggplot2)

# Niestety nie udało mi się znaleźć adekwatnej ramki danych,
# ale znalazłem informacje, że w badaniu brało udział 2500 osób.

data <- data.frame(
    score = c("Bardzo dobrze", "Dobrze", "Przeciętnie", "Źle / Bardzo źle", "Nie wiem"),
    votes = c(1875, 400, 180, 20, 25))


colors <- c("darkgreen", "lightgreen", "yellow", "orange", "red")

better_plot <- ggplot(data, aes(x = reorder(score, -votes), y = votes, fill = score)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Ocena jakości obsługi w jednostkach KOWR", 
    subtitle = "Badanie przeprowadzone na grupie 2500 mieszkańców wsi w 2023 roku",
    y = "Procent oddanych głosów") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")) +
  scale_y_continuous(labels = scales::percent_format(scale = 0.04)) +
  geom_text(
    aes(label = scales::percent(votes/sum(votes), scale = 100), vjust = -0.5),
    size = 4,
    color = "black")

ggsave("./betterPlot.jpg", better_plot, width = 6, height = 5, units = "in")