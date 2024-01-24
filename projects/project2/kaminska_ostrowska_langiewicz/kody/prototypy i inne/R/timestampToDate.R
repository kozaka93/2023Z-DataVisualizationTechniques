# dla danej ramki danych, w której jest kolumna timestamp_ms dodaje kolumne
# date formatu YYYYMMDD

file_path <- "D:\\STUDIA\\Semestr 3\\Techniki wizualizacji danych\\Projekt\\Projekt_TWD_2\\repo\\Projekt_TWD_02\\kody\\data_csv\\ig_f.csv"
df <- read.csv(file_path)

# Konwersja kolumny timestamp na daty
df$rawDate <- as.POSIXct(df$timestamp_ms / 1000, origin = "1970-01-01", tz = "Europe/Warsaw")

# Formatowanie dat w formacie YYYYMMDD
df$date <- format(df$rawDate, "%Y%m%d")
df$strDate <- format(df$rawDate, "%d-%m-%Y")

# Wyświetlenie wynikowej ramki danych
print(df)
