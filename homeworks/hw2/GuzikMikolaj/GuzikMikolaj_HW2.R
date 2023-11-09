library(dplyr)
library(ggplot2)

# Źródło:
# https://www.eska.pl/lodz/wyniki-wyborow-2023-pis-wygrywa-ale-nie-ma-szans-na-samodzielne-rzady-19-10-aa-BHru-Srek-cXw7.html
# Artykuł z dnia 23.10.2023, 14:59

# Błędy do poprawy:
# 1. Nie ma podanych procentów na słupkach. 
# 2. Nie ma osi y, ponieważ nazwy partii znajdują się nad słupkami. 
# 3. Na wykresie nie siatki co wraz z brakiem procentów i przeskokiem na osi x co 5% utrudnia odczytaniez wykresu odpowiednich wartości.

# przygotowanie ramki danych (dane wzięte ze strony Państwowej Komisji Wyborczej)
df <- data.frame(rbind(c("Prawo i Sprawiwdliwość", 35.38),
                       c("Koalicja Obywatelska", 30.7),
                       c("Trzecia Droga", 14.4),
                       c("Nowa Lewica", 8.61),
                       c("Konfederacja",7.16),
                       c("Bezpartyjni Samorządcy",1.86),
                       c("Polska Jest Jedna",1.63)))

colnames(df) <- c("Partia","Procent_glosow")

df <- df %>% 
  mutate(Procent_glosow = as.numeric(Procent_glosow),
       Partia = forcats::fct_reorder(Partia, Procent_glosow))

colors <- c("#0e6fb5","#cf0e04","#19a10a","#8a0a8f","#0e025c","#635c63","#5aabe6")

# poprawiony wykres
ggplot(df, aes(x = Procent_glosow, y = Partia, fill = Partia)) +
  geom_col(width = 0.5, position = "identity") +
  scale_x_continuous(limits = c(0, 40), labels = scales::percent_format(scale = 1), breaks = seq(0,40,5)) +
  geom_text(aes(label = Procent_glosow), hjust = -0.1, size = 4) +
  scale_fill_manual(values = rev(colors)) +
  guides(fill = "none") +
  labs(title = "Wyniki wyborów 2023 do Sejmu",
       subtitle = "Dane PKW ze 100 % obwodów do głosowania",
       x = "Procent głosów",
       y = "Partie") +
  theme_minimal()

