library(jsonlite)
library(dplyr)

konwertujTimestampy <- function(df) {
  
  
  # Konwersja kolumny timestamp na daty
  df$rawDate <- as.POSIXct(df$timestamp, origin = "1970-01-01", tz = "Europe/Warsaw")
  
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
           select(person, app, day, month, year, strDate, date)
  )
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}


# podajemy sciezke do pliku JSON your_friends, ktory powinien byc wsrod pobranych przez
# was plikow z messengera
data <- fromJSON("podaj tu swoja sciezke, wczytaj ramke i cofnij do poprzedniego napisu :)")

#istnieje niezerowe pstwo ze u was nie bedzie dzialalo
df <- data$friends_v2

df <- df %>% 
  select(timestamp) %>% 
  mutate(person = "f") %>% # zmien na moment i wroc do oryginalu potem
  mutate(app = "mg")

df <- konwertujIWybierz(df)

# tu tez trzeba zmienic literke by zapisalo do odpowiedniego pliku
write.csv(df, "../app/KomunikacJA/appData/friendsPlot/friends_mg_f.csv", row.names = FALSE)




