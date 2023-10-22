library(tidyr)
library(dplyr)

df <- read.csv("H:\\Documents\\spotify-2023.csv")


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df$streams <- as.numeric(df$streams)

df %>% 
  filter(released_year == 2023, released_month <= 3) %>%
  summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568 odtworzeń.


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(more_than_two_artists = ifelse(artist_count > 2, TRUE, FALSE)) %>% 
  group_by(more_than_two_artists) %>% 
  summarise(total_spotify_playlists = sum(in_spotify_playlists))

## Odp. Nie (>2 artystów: 428126 playlist, pozostałe: 4527593 playlist).


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(released_day = as.character(released_day), released_month = as.character(released_month), released_year = as.character(released_year)) %>% 
  mutate(released_day = ifelse(nchar(released_day) == 1, paste("0", released_day, sep = ""), released_day)) %>% 
  mutate(released_month = ifelse(nchar(released_month) == 1, paste("0", released_month, sep = ""), released_month)) %>% 
  mutate(date = paste(released_year, released_month, released_day, sep = "-")) %>% 
  mutate(weekday = weekdays(as.Date(date))) %>%
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  top_n(1)

## Odp. Piątek - wydano w sumie 526 piosenek.


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1, released_year == 2021 | released_year == 2022) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(songs_count = n()) %>% 
  filter(n() == 2) %>%
  summarise(percentage_increase = ((songs_count[released_year == 2022] - songs_count[released_year == 2021]) / songs_count[released_year == 2021]) * 100) %>% 
  top_n(1)

## Odp. SZA, wzrost o 1600%.


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(desc(danceability_.)) %>%
  head(round(0.1 * nrow(df), digits = 0)) %>% 
  mutate(released_day = as.character(released_day), released_month = as.character(released_month), released_year = as.character(released_year)) %>% 
  mutate(released_day = ifelse(nchar(released_day) == 1, paste("0", released_day, sep = ""), released_day)) %>% 
  mutate(released_month = ifelse(nchar(released_month) == 1, paste("0", released_month, sep = ""), released_month)) %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(total_years = as.numeric(as.Date("2023-12-31") - date) / 365) %>% 
  group_by(track_name, artist.s._name) %>%
  summarise(average_streams_year = streams / total_years) %>% 
  arrange(desc(average_streams_year)) %>% 
  head(1)

## Odp. Chencho Corleone, Bad Bunny - piosenka "Me Porto Bonito", 870656628 średnio odtworzeń/rok.


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

result <- df %>% 
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>% 
  arrange(desc(streams_per_playlist)) %>% 
  head(round(0.2 * nrow(df), digits = 0))
  
average_beat <- result %>% 
  summarise(mean = mean(bpm))

most_common_mode <- result %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  top_n(1)

average_beat
most_common_mode

## Odp.Średnie tempo: 125.2775, skala: Minor (96 piosenek).


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(released_day = as.character(released_day), released_month = as.character(released_month), released_year = as.character(released_year)) %>% 
  mutate(released_day = ifelse(nchar(released_day) == 1, paste("0", released_day, sep = ""), released_day)) %>% 
  mutate(released_month = ifelse(nchar(released_month) == 1, paste("0", released_month, sep = ""), released_month)) %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(season = case_when(
    date >= as.Date(paste(released_year, "03-21", sep = "-")) & date < as.Date(paste(released_year, "06-22", sep = "-")) ~ "spring",
    date >= as.Date(paste(released_year, "06-22", sep = "-")) & date < as.Date(paste(released_year, "09-23", sep = "-")) ~ "summer",
    date >= as.Date(paste(released_year, "09-23", sep = "-")) & date < as.Date(paste(released_year, "12-22", sep = "-")) ~ "autumn",
    TRUE ~ "winter"
  )) %>% 
  group_by(season) %>% 
  summarise(average_bmp = mean(bpm), average_danceability = mean(danceability_.), average_valence = mean(valence_.),
            average_energy = mean(energy_.), average_acousticness = mean(acousticness_.), average_instrumentalness = mean(instrumentalness_.),
            average_liveness = mean(liveness_.), average_speechiness = mean(speechiness_.)
            ) %>% 
  View()

## Odp. (Cała tabela wynikowa)


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najczęściej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  mutate(solo_artist = ifelse(artist_count == 1, TRUE, FALSE)) %>% 
  group_by(key, mode) %>% 
  summarise(n = n(), solo_artists_count = sum(solo_artist)) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  arrange(desc(solo_artists_count)) %>% 
  head(1)

## Odp. Para G-Major, 33 piosenki (w tym 25 wykonywanych solo).


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8", sub = "byte")

df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams)) %>% 
  top_n(1)

## Odp. The Weeknd, 23929760757 odtworzeń.


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

artists <- df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(debut_year = min(released_year)) %>% 
  filter(debut_year == 2022) %>%
  select(artist.s._name)

df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  semi_join(artists, by = "artist.s._name") %>% 
  arrange(artist.s._name) %>% 
  mutate(key_mode = paste(key, mode, sep = ", ")) %>%
  group_by(artist.s._name, key_mode) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = key_mode, values_from = n, values_fill = 0) %>% 
  View()

## Odp. (Cała tabela wynikowa)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists, in_apple_charts > 0, 
         in_spotify_charts == 0) %>% 
  select(track_name) %>% 
  View()

## Odp. (Lista wszystkich takich piosenek w tabeli wynikowej)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  mutate(solo_artist = ifelse(artist_count == 1, TRUE, FALSE)) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>%
  summarise(n = n(), solo_tracks_count = sum(solo_artist), collab_tracks_count = n - solo_tracks_count, solo_streams_count = sum(streams[solo_artist]), collab_streams_count = sum(streams[solo_artist == FALSE])) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  mutate(average_streams_per_solo_song = solo_streams_count / solo_tracks_count, average_streams_per_collab_song = collab_streams_count / collab_tracks_count) %>% 
  filter(solo_tracks_count > 0 & collab_tracks_count > 0) %>% 
  filter(average_streams_per_solo_song > average_streams_per_collab_song) %>% 
  View()

## Odp. (Lista wszystkich takich artystów w pierwszej kolumnie wynikowej tabeli)




