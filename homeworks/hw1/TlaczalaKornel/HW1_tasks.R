library(tidyr)
library(dplyr)
library(ggplot2)

df <- read.csv('spotify-2023.csv')
View(df)

#### 0. Czy rów babicze jest top 1

## Odp. Tak

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

songs <- df %>% 
  filter(released_year == "2023") %>% 
  filter(released_month <= 3) %>% 
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE))
songs


## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  group_by("more than 2 artists" = artist_count > 2) %>% 
  summarise(in_playlist_occurences = sum(in_spotify_playlists))

## Odp. Nie, w większej ilości playlist są zawarte piosenki stworzone przez jednego lub 2 artystów.


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(released_weekday = weekdays(as.Date(paste(released_year, released_month, released_day, sep = '-')))) %>% 
  group_by(released_weekday) %>% 
  summarise(release_count = n()) %>% 
  arrange(-release_count) %>% 
  head(1)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1, released_year %in% c("2021", "2022")) %>%
  group_by(artist.s._name, released_year) %>% 
  summarise(song_count = n()) %>% 
  pivot_wider(names_from = "released_year", values_from = "song_count") %>% 
  na.omit() %>% 
  rename("year_2022" = "2022", "year_2021" = "2021") %>% 
  mutate(progress = ((year_2022 / year_2021) - 1)*100) %>% 
  arrange(-progress) %>% 
  head(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  filter(danceability_. > quantile(danceability_., 0.9)) %>% 
  transmute(artist = artist.s._name, title = track_name, streams = streams, released_year = released_year, years_available = 2024 - as.numeric(released_year)) %>% 
  mutate(streams_per_year = as.numeric(streams) / years_available) %>% 
  arrange(-streams_per_year) %>% 
  head(1) %>% 
  transmute(artist = artist, title = title, streams_per_year = streams_per_year)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams = as.numeric(streams)) %>% 
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>% 
  filter(!is.na(streams_per_playlist)) %>% 
  filter(streams_per_playlist >= quantile(streams_per_playlist, 0.8)) %>% 
  summarise(average_tempo = mean(bpm), most_common_mode = names(which.max(table(df$mode)))) %>% 
  View()
  
## Odp. Średnie tempo - 125.2775, Najczęstsza skala - Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when(
    (released_month*100 + released_day) %in% 321:621 ~ "spring",
    (released_month*100 + released_day) %in% 622:922 ~ "summer",
    (released_month*100 + released_day) %in% 923:1221 ~ "autumn",
    TRUE ~ "winter"
  )) %>% 
  group_by(season) %>% 
  summarise(
    avg_danceability = mean(danceability_.),
    avg_valence = mean(valence_.),
    avg_energy = mean(energy_.),
    avg_acousticness = mean(acousticness_.),
    avg_instrumentalness = mean(instrumentalness_.),
    avg_liveness = mean(liveness_.),
    avg_speechiness = mean(speechiness_.)
  ) %>% 
  View()

## Odp. Najmniej taneczne są piosenki jesienne, a najbardziej te letnie i wiosenne. Najbardziej pozytywne są piosenki zimowe. Najmniej energii niasą ze sobą piosenki jesienne, ale są za to najbardziej akustyczne. Najmniejszy udział instrumentów zauważymy zimą a największy latem. Piosenki, w których słychać realną widownię pojawiają się podobnie często w każdej porze roku z delikatną przewagą zimy. Podobnie jest w kwestii ilości słów mówionych, jednak tutaj wygrywa wiosna.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == "2022") %>% 
  group_by(key, mode) %>% 
  summarise(song_count = n(), solo_song_count = sum(artist_count == 1)) %>% 
  arrange(-song_count) %>% 
  head(10) %>% 
  arrange(-solo_song_count) %>% 
  head(1)
  
## Odp. G-Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(as.numeric(streams))) %>% 
  arrange(-sum_of_streams) %>% 
  head(1)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debuting_artists <- df %>% 
  mutate(artist = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist, sep = ", ") %>%
  filter(artist != "") %>%
  group_by(artist) %>%
  mutate(debut_year = min(as.numeric(released_year))) %>%
  filter(debut_year == "2022") %>% 
  ungroup %>%
  group_by(artist, key, mode) %>%
  summarise(song_count = n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = song_count, values_fill = 0) %>% 
  View()

## Odp. Tabelka

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  transmute(songs = track_name) %>% 
  View()

## Odp. Lista piosenek w tabelce

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  mutate(artist = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist, sep = ", ") %>% 
  filter(artist != "" & artist != "Edison Lighthouse") %>% 
  group_by(artist, solo = artist_count == 1) %>%
  summarise(mean_streams = mean(as.numeric(streams))) %>% 
  pivot_wider(names_from = solo, values_from = mean_streams, values_fill = 0) %>% 
  rename(Solo = "TRUE", Collab = "FALSE") %>% 
  filter(Solo > Collab) %>% 
  transmute(artist = artist) %>% 
  View()

## Odp. 253 artystów widocznych w tabelce



