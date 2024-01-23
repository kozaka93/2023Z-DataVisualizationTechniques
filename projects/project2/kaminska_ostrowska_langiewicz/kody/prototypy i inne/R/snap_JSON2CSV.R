#skrypt do przejscia z JSONa na CSVke dla plikow ze Snapchata


library(jsonlite)


# wczytywanie ze snapa

path <- "D:\\STUDIA\\Semestr 3\\Techniki wizualizacji danych\\Projekt\\Projekt_TWD_2\\poufne_dane\\snapchat\\czyste.json"

json_data_snap_f <- fromJSON(path)


result_df <- data.frame(
  From = character(),
  MediaType = character(),
  Created = character(),
  Content = character(),
  ConversationTitle = character(),
  IsSender = logical(),
  CreatedMicroseconds = numeric(),
  MainFrom = character(),
  stringsAsFactors = FALSE
)

# Iteruj po każdym obiekcie JSON i dodaj do ramki danych
for (i in seq_along(json_data_snap_f)) {
  json_str <- json_data_snap_f[[i]]
  json_data <- cbind(json_str, MainFrom = rep(names(json_data_snap_f)[[i]], times = nrow(json_str)))
  result_df <- rbind(result_df, json_data)
}

result_df <- cbind(result_df, app = "sp")
result_df <- cbind(result_df, person = "f")

# Wyświetl wynik
print(result_df)

# zapisz do csvki
write.csv(result_df, file = "./data_csv/sp_f.csv", row.names = FALSE)






