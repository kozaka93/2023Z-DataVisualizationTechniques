library(dplyr)
library(ggplot2)
df <- data.frame(
  Partie = c("Prawo i Sprawiedliwosc", "Koalicja Obywatelska", "Trzecia Droga",
             "Nowa Lewica", "Konfederacja", "Bezpartyjni"),
  Procent_lp = c(36.6, 31.0, 13.5, 8.6, 6.4, 2.4))
  df <- df %>% 
    arrange(Procent_lp)

ggplot(df, aes(x = Partie, y = Procent_lp, fill = Partie)) +
  geom_col()
