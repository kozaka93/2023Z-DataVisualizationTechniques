library(dplyr)
library(ggplot2)
library(plotly)

mg_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\WyslaneOdebrane\\WyslaneOdebraneData\\mg_a.csv")
ig_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\WyslaneOdebrane\\WyslaneOdebraneData\\ig_a.csv")
sp_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\WyslaneOdebrane\\WyslaneOdebraneData\\sp_a.csv")


#policzenie wiadomosci wszystkich 
policzWiadomosci <- function(sp_a) {
  sp_a %>% 
    group_by(date) %>% 
    summarize(liczba_wiadomosci = n()) -> sp_a
  sp_a <- sp_a[order(sp_a$date), ] 
  sp_a$suma_kumulacyjna <- cumsum(sp_a$liczba_wiadomosci)
  sp_a$typ <- 'wszystkie'
  return(sp_a)
}

#policzenie wiadomosci z podzialem na wyslane i odebrane
policzWiadomosciPodzial <- function(sp_a) {
  sp_a$typ[sp_a$Sender != "Anna Ostrowska"] <- "odebrane"
  sp_a$typ[sp_a$Sender == "Anna Ostrowska"] <- "wyslane"
  sp_a <- sp_a %>% 
    group_by(date, typ) %>% 
    summarize(liczba_wiadomosci = n()) %>%
    arrange(date) %>% 
    group_by(typ) %>% 
    mutate(suma_kumulacyjna = cumsum(liczba_wiadomosci)) 
  return(sp_a)
}

policzWszystkie <- function(sp_a){
  wszystkie <- policzWiadomosci(sp_a)%>% 
    select(date, suma_kumulacyjna, typ)
  podzial <- policzWiadomosciPodzial(sp_a)%>% 
    select(date, suma_kumulacyjna, typ)
  razem <- rbind(wszystkie,podzial) 
  return(razem)
  }

mg_a <- policzWszystkie(mg_a) 
sp_a <- policzWszystkie(sp_a) 
ig_a <- policzWszystkie(ig_a) 

mg_a$date <- as.Date(as.character(mg_a$date), format = "%Y%m%d")
sp_a$date <- as.Date(as.character(sp_a$date), format = "%Y%m%d")
ig_a$date <- as.Date(as.character(ig_a$date), format = "%Y%m%d")

#wykres dla Messengera
plot_mg_a <- mg_a %>% 
  ggplot(aes(x=date, y = suma_kumulacyjna, color=typ)) +
  geom_line()+
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (mg_a)"  # Zmiana tytułu
  )+
  theme_minimal()
plot_mg_a

#wykres dla Ig
plot_ig_a <- ig_a %>% 
  ggplot(aes(x=date, y = suma_kumulacyjna, color=typ)) +
  geom_line()+
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (ig_a)"  # Zmiana tytułu
  )+
  theme_minimal()
plot_ig_a
#wykres dla Snapa
plot_sp_a <- sp_a %>% 
  ggplot(aes(x=date, y = suma_kumulacyjna, color=typ)) +
  geom_line()+
  labs(
    x = "Data",   # Zmiana podpisu osi x
    y = "Liczba wiadomości", # Zmiana podpisu osi y
    title = "Liczba wymienionych wiadomości do danego dnia (sp_a)"  # Zmiana tytułu
  )+
  theme_minimal()
plot_sp_a

ggsave('C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/wyslaneOdebranePlot/mg_a.jpg',plot_mg_a, width = 7)
ggsave('C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/wyslaneOdebranePlot/ig_a.jpg',plot_ig_a,  width = 7)
ggsave('C:/twd_proj2/repo/Projekt_TWD_02/wizualizacje/wyslaneOdebranePlot/sp_a.jpg',plot_sp_a,  width = 7)
