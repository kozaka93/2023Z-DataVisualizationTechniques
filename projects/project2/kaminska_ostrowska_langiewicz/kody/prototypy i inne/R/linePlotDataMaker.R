library(dplyr)

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

main_df <- bind_rows(mg_f,
                     mg_z,
                     mg_a,
                     ig_f,
                     ig_z,
                     ig_a,
                     sp_f,
                     # sp_z,
                     sp_a)
main_df %>% 
  select(c("person","app","day","month","year")) -> main_df

# w celu przyspieszenia aplikacji moze zajsc koniecznosc zrobienia gotowej ramki danych

write.csv(main_df, "../app/KomunikacJA/appData/linePlot/linePlotData.csv", row.names = FALSE)

