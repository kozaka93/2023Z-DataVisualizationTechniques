library(ggplot2)
library(tidyr)
library(dplyr)


df <- data.frame(
  nazwisko = c(
    "Rafał Trzaskowski",
    "Szymon Hołownia",
    "Andrzej Duda",
    "Donald Tusk",
    "Władysław Kosiniak-Kamysz",
    "Robert Biedroń",
    "Mateusz Morawiecki",
    "Włodzimierz Czarzasty",
    "Tomasz Grodzki",
    "Elżbieta Witek"
  ),
  zdecydowanie_ufa = c(31.2, 11.9, 22.3, 22.5, 11.2, 6.2, 17.4, 8.6, 14.7, 14.7),
  raczej_ufa = c(17.7, 33.1, 19.2, 18.1, 29.2, 28.1, 16.6, 23.1, 16.5, 15.4),
  obojetnosc = c(13, 22.7, 11.9, 10.1, 24.3, 23.1, 7, 23.6, 20.1, 14),
  raczej_nie_ufa = c(8.6, 13.2, 10.6, 18.1, 16.4, 11.7, 14.2, 11.5, 7.8, 9.9),
  zdecydowanie_nie_ufa = c(26.2, 17.2, 35.9, 31.1, 16.2, 29, 44.7, 21.5, 26.3, 38)
)

df %>%
  pivot_longer(
    !nazwisko,
    names_to = "poziom_zaufania",
    values_to = "procent"
  ) %>%
  mutate(poziom_zaufania = factor(
    poziom_zaufania,
    levels = c(
      "zdecydowanie_ufa",
      "raczej_ufa",
      "obojetnosc",
      "raczej_nie_ufa",
      "zdecydowanie_nie_ufa"
    )
  )) %>%
  ggplot(aes(x = nazwisko, y = procent, fill = poziom_zaufania)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.6)) +
  scale_fill_manual(
    values = c(
      "zdecydowanie_ufa" = "limegreen",
      "raczej_ufa" = "steelblue",
      "obojetnosc" = "gray",
      "raczej_nie_ufa" = "orange",
      "zdecydowanie_nie_ufa" = "red"),
    labels = c(
      "zdecydowanie_ufa" = "Zdecydowanie ufa",
      "raczej_ufa" = "Raczej ufa",
      "obojetnosc" = "Obojętność",
      "raczej_nie_ufa" = "Raczej nie ufa",
      "zdecydowanie_nie_ufa" = "Zdecydowanie nie ufa")) +
  labs(fill = "Poziom zaufania")+
  scale_y_continuous(labels = scales::percent_format(scale = 1),
    breaks = seq(0, 100, by = 5)) +
  theme_minimal() +  
  labs(title = "Poziom zaufania do polityków",x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5,size=15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "Georgia"),
        panel.grid.major.y = element_line(size = 0.3, linetype = "solid", color = "darkgrey"))
