library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)

x <- data.frame(years = c(2019:2022),
                naPierwszeMieszkanie = c(27, 26, 29, 26),
                poprawaWarunkowBytowych = c(27, 26, 25, 23),
                naWynajem = c(23, 18, 18, 20),
                zZamiaremOdsprzedarzy = c(8, 11, 11, 9),
                wynajemIPotrzebyWlasne = c(9, 9, 9, 12),
                wynajemIOdsprzedarz = c(6, 10, 8 ,10))

df <- x %>% 
  pivot_longer(cols = -years,
               names_to = "rodzaj",
               values_to = "procent") 

kolory <- RColorBrewer::brewer.pal(n = 6, name = "Set2")

df %>% 
  ggplot(aes(x = years, y = procent, fill = rodzaj)) +
  geom_col(width = 0.8, position = "dodge") +
  scale_fill_manual(values = kolory,
                    labels = c("naPierwszeMieszkanie" = "na pierwsze mieszkanie
(swoje lub członka rodziny)",
                               "naWynajem" = "na wynajem",
                               "poprawaWarunkowBytowych" = "poprawa warunków bytowych
(swoich lub członków rodziny)",
                               "wynajemIOdsprzedarz" = "zarówna na wynajem jak i
na odsprzedarz z zyskiem w korzystnym okresie",
                               "wynajemIPotrzebyWlasne" = "zarówna na wynajem jak i
na potrzeby własne",
                               "zZamiaremOdsprzedarzy" = "z zamiarem odsprzedarzy
z zyskiem w korzystnym okresie")) +
  geom_text(aes(label = paste0(procent, "%")),
            position = position_dodge(0.8),
            vjust = -0.4,
            size = 3) +
  labs(x = NULL, y = "Procent %", fill= "", title = "Cele nabywania mieszkań na rynku wtórnym miasta wojewódźkie i Gdynia") +
  theme_bw() +
  theme(axis.text = element_text(size= 10),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 15))
