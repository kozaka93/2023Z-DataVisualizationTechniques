library(dplyr)
library(tidyverse)

mg_f <- read.csv("./data_csv/mg_f.csv")
mg_z <- read.csv("./data_csv/mg_z.csv")
mg_a <- read.csv("./data_csv/mg_a.csv")

ig_f <- read.csv("./data_csv/ig_f.csv")
ig_z <- read.csv("./data_csv/ig_z.csv")
ig_a <- read.csv("./data_csv/ig_a.csv")

sp_f <- read.csv("./data_csv/sp_f.csv")
sp_z <- read.csv("./data_csv/sp_z.csv")
sp_a <- read.csv("./data_csv/sp_a.csv")

konwertujTimestampy <- function(df) {
  
  
  # Konwersja kolumny timestamp na daty
  df$rawDate <- as.POSIXct(df$timestamp_ms / 1000, origin = "1970-01-01", tz = "Europe/Warsaw")
  
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
           select(person, app, day, month, year)
  )
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}


zmienDaneSnap <- function(df){
  return(
    df %>% 
      rename(timestamp_ms = Created.microseconds.)
  )
}

sp_f <- zmienDaneSnap(sp_f)
sp_z <- zmienDaneSnap(sp_z)
sp_a <- zmienDaneSnap(sp_a)



mg_f <- konwertujIWybierz(mg_f)
mg_z <- konwertujIWybierz(mg_z)
mg_a <- konwertujIWybierz(mg_a)

ig_f <- konwertujIWybierz(ig_f)
ig_z <- konwertujIWybierz(ig_z)
ig_a <- konwertujIWybierz(ig_a)

sp_f <- konwertujIWybierz(sp_f)
sp_z <- konwertujIWybierz(sp_z)
sp_a <- konwertujIWybierz(sp_a)

sp_z <- sp_z %>% filter(year != 2024)

main_df <- bind_rows(mg_f,
                     mg_z,
                     mg_a,
                     ig_f,
                     ig_z,
                     ig_a,
                     sp_f,
                     sp_z,
                     sp_a)


main_df <- main_df %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) %>% 
  mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day))) %>% 
  mutate(liczba = 1) 

main_df <- main_df %>% 
  select(person, app, date, liczba)

main_df <- main_df %>% group_by(person, app, date) %>% summarise(liczba = sum(liczba))

# w celu przyspieszenia aplikacji moze zajsc koniecznosc zrobienia gotowej ramki danych

write.csv(main_df, "../app/KomunikacJA/appData/heatMap/heatMapData.csv", row.names = FALSE)

write.csv(ig_a, "../app/KomunikacJA/appData/heatMap/ig_a.csv", row.names = FALSE)
write.csv(ig_f, "../app/KomunikacJA/appData/heatMap/ig_f.csv", row.names = FALSE)
write.csv(ig_z, "../app/KomunikacJA/appData/heatMap/ig_z.csv", row.names = FALSE)

write.csv(mg_a, "../app/KomunikacJA/appData/heatMap/mg_a.csv", row.names = FALSE)
write.csv(mg_f, "../app/KomunikacJA/appData/heatMap/mg_f.csv", row.names = FALSE)
write.csv(mg_z, "../app/KomunikacJA/appData/heatMap/mg_z.csv", row.names = FALSE)

write.csv(sp_f, "../app/KomunikacJA/appData/heatMap/sp_f.csv", row.names = FALSE)
  

ee <- mg_a %>% 
  select(person, app, day, month, year)


write.csv(ee, "../app/KomunikacJA/appData/heatMap/ee.csv", row.names = FALSE)


