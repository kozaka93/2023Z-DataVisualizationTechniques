library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

# setwd("C:/Users/Sebastian/Desktop/Semestr 3/Techniki wizualizacji danych/Laboratorium/HW2")

# Przygotowanie danych ---------------------------------------------------------
df <- readxl::read_xlsx("dane_do_wykresu.xlsx") %>%
  mutate(Partia = str_wrap(Partia, width = 15),
         Partia = fct_reorder(Partia, `Ilość głosów`, .desc = TRUE))
kolory_slupkow <- c("#b31b00", "#ad7e07", "#096fab",
                    "#8a011a", "#36006b", "#099110")

# Rysowanie wykresu ------------------------------------------------------------
ggplot(df, aes(x = Partia, y = `Ilość głosów`)) +
  geom_col(fill = "#e0e0de",
           color = kolory_slupkow, linewidth = 1.5,
           width = 0.7) +
  coord_cartesian(ylim = c(0,50)) +
  geom_text(aes(x = Partia, y = `Ilość głosów`,
                label = paste0(`Ilość głosów`, "%")),
            vjust = df$`Ilość głosów` * 0.1 + 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Jak w wyborach parlamentarnych w 2023 roku zagłosowały osoby,\nktóre nie głosowały w wyborach parlamentarnych w roku 2019",
       caption = "źródło: sondaż Exit Poll IPSOS") +
  xlab("Partia polityczna") +
  ylab("Ilość głosów") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5,
                                        linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.5, 
                                    linetype = 'solid',
                                    colour = "black"),
    panel.grid.minor = element_line(linewidth = 0.25, 
                                    linetype = 'solid',
                                    colour = "black"))
  