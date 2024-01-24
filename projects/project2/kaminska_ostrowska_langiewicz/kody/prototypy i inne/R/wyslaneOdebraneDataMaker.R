library(dplyr)
library(lubridate)
#sciezki do brudnych csv z mg i ig (tych samych, co wrzucaliscie do Zosi przy emoji)
#ZMIENIC LITERKI
#ze snapa ja wzielam juz Filipa, Zosi jeszcze nie ma:()
ig_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\emojiData\\emoji_in_z.csv")
sp_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_a.csv")
ig_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\emojiData\\emoji_in_z.csv")

sp_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_z.csv")
konwertujTimestampy <- function(df) {
  
  
  # Konwersja kolumny timestamp na daty
  df$rawDate <- as.POSIXct(df$Timestamp / 1000, origin = "1970-01-01", tz = "Europe/Warsaw")
  
  # Formatowanie dat w formacie YYYYMMDD
  df$date <- format(df$rawDate, "%Y%m%d")
  df$strDate <- format(df$rawDate, "%d-%m-%Y")
  df$year <- format(df$rawDate, "%Y")
  df$month <- format(df$rawDate, "%m")
  df$day <- format(df$rawDate, "%d")
  return(df)
}


wybierzOdpowiednieKolumny <- function(df) {
  return(df %>% 
           select(Sender, GroupOrPriv, date)
  )
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy_mg_z(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}


zmienDaneSnap <- function(df){
  return(
    df %>% 
      rename(Timestamp = Created.microseconds.) %>% 
      rename(Sender=IsSender)
  )
}
#WAZNE - pozmieniac literki!
sp_z <- zmienDaneSnap(sp_z)
mg_z <- konwertujTimestampy_mg_z(mg_z)
ig_a <- konwertujIWybierz(ig_a)
sp_z<- konwertujTimestampy(sp_z)
sp_z <- sp_z %>% 
  select(Sender,date)
date<- ymd_hms(ig_z$Timestamp)

# Wydobywanie samej daty w formie liczby całkowitej
ig_z$date <- as.numeric(format(date, "%Y%m%d"))
ig_z %>% 
  select(Sender, GroupOrPriv, date) ->ig_z
#ZMIENIC LITERKI I NAZWISKA
mg_a$Sender[mg_a$Sender != "Zosia Kamińska"] <- "Other"
ig_a$Sender[ig_a$Sender != "Anna Ostrowska"] <- "Other"
sp_z$Sender[sp_z$Sender != "TRUE"] <- "Other"
sp_z$Sender[sp_z$Sender == "TRUE"] <- "Zosia Kaminska"

#TU TEZ ZMIENIC
write.csv(mg_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_mg_z.csv", row.names = FALSE)
write.csv(ig_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_ig_z.csv", row.names = FALSE)
write.csv(sp_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_sp_z.csv", row.names = FALSE)

linePlot_mg_a <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_a.csv")
linePlot_ig_a <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_a.csv")
linePlot_sp_a <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_a.csv")
linePlot_mg_f <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_f.csv")
linePlot_ig_f <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_f.csv")
linePlot_sp_f <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_f.csv")
linePlot_mg_z <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_z.csv")
linePlot_ig_z <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_z.csv")
linePlot_sp_z <- read.csv("..\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_z.csv")

linePlot_sp_a$GroupOrPriv <- "priv"
linePlot_sp_f$GroupOrPriv <- "priv"
linePlot_sp_z$GroupOrPriv <- "priv"

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
  sp_a$typ[sp_a$Sender == "Other"] <- "odebrane"
  sp_a$typ[sp_a$Sender != "Other"] <- "wyslane"
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

linePlot_mg_a <- policzWszystkie(linePlot_mg_a)
linePlot_sp_a <- policzWszystkie(linePlot_sp_a)
linePlot_ig_a <- policzWszystkie(linePlot_ig_a)
linePlot_mg_f <- policzWszystkie(linePlot_mg_f)
linePlot_sp_f <- policzWszystkie(linePlot_sp_f)
linePlot_ig_f <- policzWszystkie(linePlot_ig_f)
linePlot_mg_z <- policzWszystkie(linePlot_mg_z)
linePlot_sp_z <- policzWszystkie(linePlot_sp_z)
linePlot_ig_z <- policzWszystkie(linePlot_ig_z)
linePlot_mg_a$app <- "mg"
linePlot_mg_f$app <- "mg"
linePlot_mg_z$app <- "mg"
linePlot_sp_a$app <- "sp"
linePlot_sp_f$app <- "sp"
linePlot_sp_z$app <- "sp"
linePlot_ig_a$app <- "ig"
linePlot_ig_f$app <- "ig"
linePlot_ig_z$app <- "ig"
linePlot_mg_a$person <- "a"
linePlot_ig_a$person <- "a"
linePlot_sp_a$person <- "a"
linePlot_mg_f$person <- "f"
linePlot_ig_f$person <- "f"
linePlot_sp_f$person <- "f"
linePlot_mg_z$person <- "z"
linePlot_ig_z$person <- "z"
linePlot_sp_z$person <- "z"

linePlot_data <- rbind(linePlot_mg_a, linePlot_ig_a, linePlot_sp_a, linePlot_mg_f, linePlot_ig_f, linePlot_sp_f, linePlot_mg_z, linePlot_ig_z, linePlot_sp_z)
linePlot_data$date <- as.Date(as.character(linePlot_data$date), format = "%Y%m%d")

write.csv(linePlot_data, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_all.csv", row.names = FALSE)
