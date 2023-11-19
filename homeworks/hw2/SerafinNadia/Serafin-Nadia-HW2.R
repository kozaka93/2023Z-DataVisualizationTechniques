# https://kielce.wyborcza.pl/kielce/7,47262,30304454,wybory-2023.html
# publikacja z 15.10.2023, 21:00

# DO POPRAWY:
# - nie ma jednostek na słupkach (procentów)
# - nie ma spójności w miejscu procentów (liczb) - na wszystkich słupkach liczba 
#   jest na słupku, a na ostatnim poza nim
# - kolory słupków mogłyby nawiązywać do kolorów na logo partii
# - nazwy partii i koalicji mogłyby być obok słupków a nie miedzy nimi, bo dla 
#   osoby, która wgl nie jest na bieżąco z tematem może to być mylące 

# przygotowanie ramki danych
library(dplyr)
library(ggplot2)

my_data <- data.frame(rbind(c("PiS", 36.8),
                         c("KO", 31.6),
                         c("Trzecia Droga", 13),
                         c("Lewica", 8.6),
                         c("Konfederacja", 6.2),
                         c("Bezpartyjni Samorządowcy", 2.4),
                         c("Polska Jest Jedna", 1.2)))

colnames(my_data) <- c("Partia", "Procent_głosów")
my_data$Procent_głosów <- as.numeric(my_data$Procent_głosów)

my_data <- my_data %>% 
  mutate(Partia = reorder(Partia, Procent_głosów))

my_data$Procent_głosów_z_procentem <- paste(my_data$Procent_głosów, "%")

# kolory:
# pis - granatowy
# ko - czerwony
# trzecia droga - zolty
# lewica - fioletowy
# konfederacja - ciemnogranatowy
# bezpartyjni - czarny
# polska jest jedna - niebieski

my_colours <- c("#059be1", "#342c54", "#242427", "#8e1a58", "#fac315", "#dc241c", "#0e377e")

my_plot <- ggplot(my_data, aes(x = Partia, y = Procent_głosów, fill = Partia)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 40), labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = Procent_głosów_z_procentem), hjust = -0.2, size = 3) +
  scale_fill_manual(values = my_colours) +
  guides(fill = "none") +
  labs(title = "Wybory 2023: wyniki exit poll",
       subtitle = "Wyniki badania opublikowanego o godz. 21.00, tuż po zamknięciu lokali wyborczych",
       y = "Procent głosów") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "white")) +
        #axis.text.y = element_text(size = 10))   // powiekszenie czcionki partii
  theme_light()

my_plot

ggsave("wykres_po.png", plot = my_plot, width = 8, height = 6, units = "in")

