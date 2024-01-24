library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("../app/KomunikacJA/appData/linePlot/linePlotData.csv") 

sp_a <- read.csv("../app/KomunikacJA/appData/linePlot/sp_a.csv") 
sp_f <- read.csv("../app/KomunikacJA/appData/linePlot/sp_f.csv") 

mg_a <- read.csv("../app/KomunikacJA/appData/linePlot/mg_a.csv") 
mg_f <- read.csv("../app/KomunikacJA/appData/linePlot/mg_f.csv") 
mg_z <- read.csv("../app/KomunikacJA/appData/linePlot/mg_z.csv") 

ig_a <- read.csv("../app/KomunikacJA/appData/linePlot/ig_a.csv") 
ig_f <- read.csv("../app/KomunikacJA/appData/linePlot/ig_f.csv") 
ig_z <- read.csv("../app/KomunikacJA/appData/linePlot/ig_z.csv") 


#policzenie wiadomosci
policzWiadomosci <- function(sp_a) {
  sp_a %>% 
    group_by(strDate) %>% 
    summarize(liczba_wiadomosci = n()) -> sp_a
  sp_a$strDate <- as.Date(sp_a$strDate, "%d-%m-%Y")
  sp_a$strDate <- as.Date(sp_a$strDate, "%Y-%m-%d")
  sp_a <- sp_a[order(sp_a$strDate), ] 
  sp_a$suma_kumulacyjna <- cumsum(sp_a$liczba_wiadomosci)
  return(sp_a)
}
sp_a <- policzWiadomosci(sp_a) 
mg_a <- policzWiadomosci(mg_a)
ig_a <- policzWiadomosci(ig_a)

sp_f <- policzWiadomosci(sp_f)
mg_f <- policzWiadomosci(mg_f)
ig_f <- policzWiadomosci(ig_f)

#sp_z <- policzWiadomosci(sp_z)
mg_z <- policzWiadomosci(mg_z)
ig_z <- policzWiadomosci(ig_z)


#polaczenie w 1 tabele
sp_a$app <- "sp"
sp_f$app <- "sp"
#sp_z$app <- "sp"

mg_a$app <- "mg"
mg_f$app <- "mg"
mg_z$app <- "mg"

ig_a$app <- "ig"
ig_f$app <- "ig"
ig_z$app <- "ig"

all_a <- rbind(mg_a, ig_a, sp_a)
all_z <- rbind(mg_z, ig_z)
all_f <- rbind(mg_f, ig_f, sp_f)

#wykresy
kolory <- c("mg" = "#0099FF", "ig" = "#C13584", "sp" = "#FFFC00")

plot_a <- all_a %>% 
  ggplot(aes(x=strDate, y = suma_kumulacyjna, color=app)) +
  geom_line()+
  scale_color_manual(values = kolory, name = "Aplikacja") + 
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (Ania)"  # Zmiana tytułu
  )+
  theme_minimal()

plot_f <- all_f %>% 
  ggplot(aes(x=strDate, y = suma_kumulacyjna, color=app)) +
  geom_line()+
  scale_color_manual(values = kolory, name = "Aplikacja") + 
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (Filip)"  # Zmiana tytułu
  )+
  theme_minimal()

plot_z <- all_z %>% 
  ggplot(aes(x=strDate, y = suma_kumulacyjna, color=app)) +
  geom_line()+
  scale_color_manual(values = kolory, name = "Aplikacja") + 
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (Zosia)"  # Zmiana tytułu
  )+
  theme_minimal()

ggplotly(plot_a) -> plotly_a
ggplotly(plot_f) -> plotly_f
ggplotly(plot_z) -> plotly_z

htmltools::save_html(plotly_a, file = "C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/linePlot_a.html")
htmltools::save_html(plotly_f, file = "C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/linePlot_f.html")
htmltools::save_html(plotly_z, file = "C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/linePlot_z.html")

