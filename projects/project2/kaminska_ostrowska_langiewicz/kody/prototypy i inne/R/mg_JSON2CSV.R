#skrypt do przejscia z JSONa na CSVke dla plikow z messengera

library(jsonlite)
library(dplyr)



# Ścieżka do folderu z plikami JSON czystymi
sciezka_do_folderu <- "C:\\Users\\flang\\Downloads\\czysteZosia\\czyste"

# Pobierz listę plików JSON w folderze
pliki_json <- list.files(sciezka_do_folderu, pattern = "\\.json$", full.names = TRUE)

# Wczytaj i przetwórz każdy plik JSON
ramki_danych <- lapply(pliki_json, function(sciezka_do_pliku) {
  # Wczytaj plik JSON
  json_data <- fromJSON(sciezka_do_pliku)
  
  # Wyciągnij informacje o uczestnikach
  participants <- json_data$participants
  
  # Wyciągnij informacje o wiadomościach
  messages <- json_data$messages
  
  # Liczba uczestników
  liczba_uczestnikow <- nrow(participants)
  
  # Dodaj kolumnę "NumOfParticipants" do ramki danych messages
  messages_df <- as.data.frame(messages)
  messages_df$NumOfParticipants <- liczba_uczestnikow
  
  # Dodaj kolumnę "MainFrom" z nazwą pliku
  messages_df$MainFrom <- basename(sciezka_do_pliku)
  
  
  
  return(messages_df)
})

# Połącz ramki danych w jedną
merged_df <- bind_rows(ramki_danych)

# Przetwórz kolumnę "reactions"

merged_df$reactions <- lapply(merged_df$reactions, function(reaction) {
  ifelse(is.null(reaction), NA, reaction$reaction)
})

merged_df$reactions <- unlist(merged_df$reactions)


# Przetwórz kolumnę "audio_files"
merged_df$audio_files <- lapply(merged_df$audio_files, function(audio) {
  ifelse(is.null(audio), NA, audio$creation_timestamp)
})


merged_df$audio_files <- unlist(merged_df$audio_files)
colnames(merged_df)[colnames(merged_df) == "audio_files"] <- "audioFilesTimestamp"


# Przetwórz kolumnę "videos"
merged_df$videos <- lapply(merged_df$videos, function(video) {
  ifelse(is.null(video), NA, video$creation_timestamp)
})

merged_df$videos <- unlist(merged_df$videos)
colnames(merged_df)[colnames(merged_df) == "videos"] <- "videosTimestamp"


# Przetwórz kolumnę "photos"
merged_df$photos <- lapply(merged_df$photos, function(photo) {
  ifelse(is.null(photo), NA, photo$creation_timestamp)
})

merged_df$photos <- unlist(merged_df$photos)
colnames(merged_df)[colnames(merged_df) == "photos"] <- "photosTimestamp"



# Przetwórz kolumnę "files"
merged_df$files <- lapply(merged_df$files, function(file) {
  ifelse(is.null(file), NA, file$creation_timestamp)
})

merged_df$files <- unlist(merged_df$files)
colnames(merged_df)[colnames(merged_df) == "files"] <- "filesTimestamp"



# Przetwórz kolumnę "gifs"
merged_df$gifs <- lapply(merged_df$gifs, function(gif) {
  ifelse(is.null(gif), NA, gif$uri)
})

merged_df$gifs <- unlist(merged_df$gifs)

merged_df <- cbind(merged_df, app = "mg")
merged_df <- cbind(merged_df, person = "f")



# Wyświetl wynik
print(merged_df)


# zapisz do csvki
write.csv(merged_df, file = "./data_csv/mg_f.csv", row.names = FALSE)


