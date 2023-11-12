library(dplyr)
library(ggplot2)
library(forcats)

dane <- data.frame(kurs = c("ogólne informatyczne", "specjalistyczne informatyczne", 
                            "zarządzania", "pracy zespołowej", 
                            "obsługi klienta", "rozwiązywania problemów",
                            "biurowe (administracyjne)", "językowe",
                            "techniczne, praktyczne lub zawodowe",
                            "komunikacji pisemnej lub ustnej", 
                            "liczenia, czytania i pisania",
                            "inne"),
                   ile = c(2443, 2639, 6436, 4554, 7167, 3406, 5145, 3217, 14068, 649, 98, 7485))

dane %>% 
  mutate(kurs = fct_reorder(kurs, ile)) %>% 
  ggplot(aes(x = ile, y = kurs)) +
  geom_col(fill="lightblue", width=.8) +
  theme_minimal() +
  labs(
    title = "Liczba przedsiębiorstw, których pracownicy w 2020 r.\nnabyli umiejętności na kursach",
    y = "",
    x = "Liczba przedsiębiorstw"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.x = element_text(size = 10, color = "#333333")) +
  geom_text(
    aes(label = ile),
        hjust = -.2,
        size = 3) +
  xlim(0, 15000)

