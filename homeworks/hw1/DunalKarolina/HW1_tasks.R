library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')
View(df)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month < 4) %>% 
  summarise(mean_streams = mean(strtoi(streams), na.rm = TRUE))


## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
more_than_two <- df %>% 
  filter(artist_count  > 2) %>% 
  summarise(sum(in_spotify_playlists, na.rm = TRUE))

one_or_two <- df %>% 
  filter(artist_count <= 2) %>% 
  summarise(sum(in_spotify_playlists, na.rm = TRUE))

more_than_two > one_or_two


## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, 
                                      sep = "-"), format = "%Y-%m-%d")) %>% 
  mutate(day_of_week = weekdays(release_date)) %>% 
  group_by(day_of_week) %>% 
  summarise(n = n()) %>% 
  top_n(1)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? 
#### (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2022 i w 2021.
  
df %>% 
  filter(artist_count == 1, released_year == 2021 | released_year == 2022) %>% 
  group_by(artist.s._name, released_year)  %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = released_year, values_from = n, values_fill = 0) %>%
  filter(`2021` > 0, `2022` > 0) %>% 
  mutate(increase = ifelse(`2021` == 0, 0, (`2022` - `2021`) / `2021` * 100)) %>%
  arrange(-increase) %>% 
  head(1) 

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty
#### ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  arrange(-danceability_.) %>% 
  head(round(nrow(df) * 0.1)) %>% 
  mutate(years_active = 2024 - released_year) %>%
  group_by(artist.s._name, track_name) %>% 
  summarise(average_streams = sum(strtoi(streams)) / sum(years_active)) %>% 
  arrange(-average_streams) %>% 
  head(1) %>% 
  select(artist.s._name)

## Odp. Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
  mutate(streams_per_playlist = strtoi(streams) / in_spotify_playlists) %>%
  arrange(-streams_per_playlist) %>% 
  head(round(nrow(df) * 0.2)) %>% 
  summarise(average_bpm = mean(bpm, na.rm = TRUE))

df %>%
  mutate(streams_per_playlist = strtoi(streams) / in_spotify_playlists) %>%
  arrange(-streams_per_playlist) %>% 
  head(round(nrow(df) * 0.2)) %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(mode)

## Odp. skala: Minor, średnie tempo: 125.2775


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
seasons <- df %>% 
  mutate(season = case_when(
    (released_month * 100 + released_day) %in% 321:621 ~ "Spring",
    (released_month * 100 + released_day) %in% 622:922 ~ "Summer",
    (released_month * 100 + released_day) %in% 923:1221 ~ "Autumn",
    TRUE ~ "Winter")) %>% 
  group_by(season) %>% 
  summarise(avg_danceability = mean(danceability_.), avg_valence = mean(valence_.),
            avg_energy = mean(energy_.), avg_acousticness = mean(acousticness_.),
            avg_instrumentalness = mean(instrumentalness_.), avg_liveliness = mean(liveness_.),
            avg_speechiness = mean(speechiness_.))
View(seasons)

## Odp.największa taneczność - lato, największa pozytywność - zima, największa energetyka - zima,
## największy procent dźwięku akustycznego - jesien, największa instrumentalność - lato
## największa obecność elementów występów na żywo - zima, największa ilość słów mówionych - wiosna
## (wyniki dla każdej pory w ramce seasons)


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode, artist_count) %>% 
  summarise(n = n()) %>% 
  filter(key != "") %>% 
  arrange(-n) %>% 
  head(10) %>% 
  filter(artist_count == 1)

## Odp.G-Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(num_of_streams = sum(strtoi(streams))) %>% 
  arrange(-num_of_streams) %>% 
  head(1)

## Odp.Bad Bunny


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
separated_artists <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "")

artists_before_2022 <- separated_artists %>% 
  filter(released_year < 2022) %>% 
  distinct(artist.s._name)

artists_debut_2022 <- separated_artists %>% 
  filter(released_year == 2022) %>% 
  filter(artist.s._name != "", !(artist.s._name %in% artists_before_2022$artist.s._name)) %>% 
  distinct(artist.s._name)

mode_key_chart <- separated_artists %>% 
  filter(artist.s._name != "", artist.s._name %in% artists_debut_2022$artist.s._name) %>% 
  filter(key != "", mode != "") %>% 
  mutate(mode_key = paste(mode, key, sep = "-")) %>% 
  group_by(artist.s._name, mode_key) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = mode_key, values_from = n, values_fill = 0)

View(mode_key_chart)

## Odp zestawienie w ramce artists_debut_2022


#### 11. Jakie piosenki mimo tego, że były bardziej popularne 
### (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
songs_spotify_apple <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists,
         in_spotify_charts == 0 & in_apple_charts != 0) %>% 
  select(track_name, artist.s._name)
  
View(songs_spotify_apple)

## Odp. 337 takich piosenek (odp w ramce songs_spotify_apple)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę 
### gdy tworzy solo niż gdy tworzy z innymi artystami?
solo_or_collab <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist.s._name, sep = ", ") %>% 
  filter(artist.s._name != "") %>%
  mutate(is_solo = ifelse(artist_count == 1, "solo", "collab")) %>% 
  mutate(artist.s._name = trimws(artist.s._name)) %>% 
  group_by(artist.s._name, is_solo) %>% 
  summarise(average_streams = mean(strtoi(streams), na.rm = TRUE)) %>%
  pivot_wider(names_from = is_solo, values_from = average_streams, values_fill = 0) %>% 
  filter(solo > 0, collab > 0, solo > collab)

View(solo_or_collab)

## Odp. Jest 51 takich artystów, np. Taylor Swift, John Legend, Kendrick Lamar
## (odp w ramce solo_or_collab)



