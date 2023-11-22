library(ggplot2)
library(forcats)

procent_przewozu <- c(88.6, 10.4, 2.3, 0.4, 0.1)
rodzaj_transportu <-
  c(
    "Transport samochodowy",
    "Transport kolejowy",
    "Transport rurociągowy",
    "Transport morski",
    "Śródlądowy transport wodny oraz transport lotniczy"
  )
rodzaj_transportu <-
  c("Samochodowy",
    "Kolejowy",
    "Rurociągowy",
    "Morski",
    "Śródlądowy wodny oraz lotniczy")

df <-
  data.frame(ProcentPrzewozu = procent_przewozu, RodzajTransportu = rodzaj_transportu)

ggplot(df, aes(
  x = fct_reorder(RodzajTransportu, ProcentPrzewozu),
  y = ProcentPrzewozu
)) +
  geom_col(fill = "darkblue") +
  labs(title = "Przewozy ładunków według rodzajów transportu w 2022",
       x = "Rodzaj Transportu",
       y = "Procentowy udzial w całkowitym przewozie") +
  geom_text(aes(label = ProcentPrzewozu),
            vjust = -0.5,
            size = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(
      angle = 40,
      hjust = 1,
      size = 12
    ),
    panel.grid.minor = element_blank()
  )

