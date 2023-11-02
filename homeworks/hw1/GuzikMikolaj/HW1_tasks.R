library(tidyr)
library(dplyr)

library(lubridate)

df <- read.csv('spotify-2023.csv')

df <- df %>% 
  mutate(streams = as.numeric(streams))


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 , released_month %in% c(1,2,3)) %>% 
  summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  mutate(one_or_two = artist_count <= 2) %>% 
  group_by(one_or_two) %>%
  summarise(sum_playlists = sum(in_spotify_playlists, na.rm = TRUE))

## Odp. Nie

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(released_date = make_date(released_year, released_month, released_day)) %>% 
  mutate(weekday = wday(released_date, label = TRUE)) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  top_n(1)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(released_year %in% c(2021, 2022), artist_count == 1) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(songs_count = n()) %>% 
  pivot_wider(names_from = released_year, values_from = songs_count) %>% 
  mutate(percentage_growth = ((`2022` - `2021`) / `2021`) * 100) %>% 
  arrange(-percentage_growth) %>% 
  top_n(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  head(nrow(df) * 0.1) %>% 
  group_by(artist.s._name) %>% 
  mutate(avg_streams_per_year = streams / (2024 - released_year)) %>% 
  arrange(-avg_streams_per_year) %>% 
  select(artist.s._name) %>% 
  top_n(1)
  
## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  arrange(-in_spotify_playlists) %>% 
  head(nrow(df) * 0.2) %>% 
  group_by(mode) %>% 
  summarise(mean_bpm = mean(bpm, na.rm = TRUE), n = n()) %>% 
  arrange(-n) %>% 
  top_n(1) %>% 
  View()

## Odp. 123.725, Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

zadanie7 <- df %>% 
  mutate(season = case_when(
    released_month %in% c(4,5) | (released_month == 3 & released_day > 20) | (released_month == 6 & released_day < 22) ~ "spring",
    released_month %in% c(7,8) | (released_month == 6 & released_day > 21) | (released_month == 9 & released_day < 23) ~ "summer",
    released_month %in% c(10,11) | (released_month == 9 & released_day > 22) | (released_month == 12 & released_day < 22) ~ "fall",
    released_month %in% c(1,2) | (released_month == 12 & released_day > 21) | (released_month == 3 & released_day < 21) ~ "winter"
  )) %>% 
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_., na.rm = TRUE),
            mean_valence = mean(valence_., na.rm = TRUE),
            mean_energy = mean(energy_., na.rm = TRUE),
            mean_acousticness = mean(acousticness_., na.rm = TRUE),
            mean_instrumentalness = mean(instrumentalness_., na.rm = TRUE),
            mean_liveness = mean(liveness_., na.rm = TRUE),
            mean_speechiness = mean(speechiness_., na.rm = TRUE)
  )

## Odp.
zadanie7 %>% View()

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(total_streams = sum(streams), solo_artists = sum(artist_count == 1)) %>% 
  arrange(-total_streams) %>% 
  top_n(10) %>% 
  arrange(-solo_artists)

## Odp. key: G, mode: Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>% 
  arrange(-total_streams) %>% 
  top_n(1)

## Odp. The Weeknd 

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debut <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8", sub = "")) %>%
  separate_rows(artist.s._name, sep = ', ') %>% 
  group_by(artist.s._name) %>% 
  summarise(debut = min(released_year)) %>% 
  filter(debut == 2022) %>% 
  select(artist.s._name) %>% 
  simplify2array()

zadanie10 <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8", sub = "")) %>%
  separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name %in% debut & artist.s._name != "") %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = n, values_fill = 0)

## Odp.
zadanie10 %>% View()

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

zadanie11 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name, artist.s._name)

## Odp.
zadanie11 %>% View()

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(mean_solo_streams = mean(streams, na.rm = TRUE))

multi <- df %>% 
  filter(artist_count > 1) %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8", sub = "")) %>%
  separate_rows(artist.s._name, sep = ', ') %>% 
  group_by(artist.s._name) %>% 
  summarise(mean_multi_streams = mean(streams, na.rm = TRUE))

zadanie12 <- solo %>% 
  left_join(multi, by = "artist.s._name") %>% 
  filter(mean_solo_streams > mean_multi_streams) %>% 
  arrange(-mean_solo_streams)
  
## Odp.
zadanie12 %>% View()

