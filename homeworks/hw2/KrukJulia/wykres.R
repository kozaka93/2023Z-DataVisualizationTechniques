library(dplyr)
library(ggplot2)
df <- data.frame(
  Kraj = c("Polska", "Niemcy", "Węgry", "Czechy", "Słowacja"),
  Wzrost = c(19, 7, 5.3, 4.7, 2.9)
)

df <- df %>% 
  arrange(-Wzrost)

ggplot(df, aes(x = reorder(Kraj, -Wzrost), y = Wzrost, 
               fill = factor(ifelse(Kraj == "Polska", "Highlighted", 
                                    "Normal")))) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) +
  geom_text(aes(label = paste(Wzrost, "%")), vjust = 1.5, 
            hjust = 0.5, colour = "white") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Skumulowany wzrost produktywności gospodarki w latach 2015 - 2022",
       subtitle = "Źródło: Eurostat",
       x = "", y = "Wzrost") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(size = 10)) +
  scale_fill_manual(name = "Kraj", values = c("#5F8D4E", "#A8A196")) +
  theme_bw()

