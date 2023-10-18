library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>% 
  summarise(streams_mean = mean(as.numeric(streams)))
  

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  group_by(more_than_2_artists = artist_count > 2) %>% 
  summarise(s_playlists_count = sum(in_spotify_playlists)) %>% 
  arrange(s_playlists_count)

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         released_day_of_week = weekdays(released_date)) %>% 
  group_by(released_day_of_week) %>% 
  summarise(songs_released = n()) %>% 
  top_n(1)

## Odp. Piątek, liczba piosenek 526

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

artists_2021_2022 <- df %>% 
  filter(released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name) %>% 
  summarise(both_2021_2022 = n_distinct(released_year)) %>% 
  filter(both_2021_2022 == 2) %>% 
  select(artist.s._name)

df %>% 
  filter(artist.s._name %in% artists_2021_2022$artist.s._name,
         released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(released_songs = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = released_year, values_from = released_songs) %>% 
  rename(songs2021 = '2021', songs2022 = '2022') %>% 
  mutate(released_songs_increase = (songs2022 - songs2021) * 100 / songs2021) %>% 
  top_n(1)
  

## Odp. SZA, 1600%

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  top_frac(0.1, wt = danceability_.) %>% 
  mutate(years_count = 2023 - released_year + 1,
         streams_by_year_mean = as.numeric(streams) / years_count) %>% 
  top_n(1, wt = streams_by_year_mean) %>% 
  select(artist.s._name, streams_by_year_mean)
  
## Odp. Chencho Corleone, Bad Bunny, średnio 720378909 na rok

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_20_frac_streams_by_s_playlists <- df %>% 
  mutate(streams_s_playlists_frac = as.numeric(streams) / in_spotify_playlists) %>% 
  filter(!is.na(streams_s_playlists_frac)) %>% 
  top_frac(0.2, wt = streams_s_playlists_frac)

top_20_frac_streams_by_s_playlists %>% 
  summarise(bpm_mean = mean(bpm))

top_20_frac_streams_by_s_playlists %>% 
  group_by(mode) %>% 
  summarise(mode_count = n()) %>% 
  top_n(1, wt = mode_count)

## Odp. Średnie tempo 125.2 bpm, najczęstsza skala Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df_seasons <- df %>% 
  mutate(season = case_when(released_month < 3 ~ "Winter",
                             released_month == 3 & released_day < 21 ~ "Winter",
                             released_month < 6 ~ "Spring",
                             released_month == 6 & released_day < 22 ~ "Spring",
                             released_month < 9 ~ "Summer",
                             released_month == 9 & released_day < 23 ~ "Summer",
                             released_month < 12 ~ "Autumn",
                             released_month == 12 & released_day < 22 ~ "Autumn",
                             TRUE ~ "Winter"))

df_seasons %>% 
  select(danceability_.:season) %>% 
  group_by(season) %>% 
  summarise_at(vars(danceability_.:speechiness_.), mean)

## Odp. Ponizej jest zestawienie
#   season  danceability_. valence_. energy_. acousticness_. instrumentalness_. liveness_.
# 1 Autumn           63.9      47.0     61.2           29.7              1.96        17.6
# 2 Spring           68.6      49.5     64.2           27.7              1.67        18.3
# 3 Summer           68.6      51.5     65.8           24.7              2.17        18.0
# 4 Winter           66.7      57.5     66.0           25.7              0.730       18.7

#### 8. Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

top_10_key_mode <- df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  top_n(10, wt = count)

df %>% 
  filter(artist_count == 1, key %in% top_10_key_mode$key, mode %in% top_10_key_mode$mode) %>% 
  group_by(key, mode) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  top_n(1, wt = count)

## Odp. Są dwie takie pary "" Major i "G" Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  select(artist.s._name, streams) %>% 
  separate_longer_delim(artist.s._name, delim = ",") %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_sum = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  top_n(1, wt = streams_sum)

## Odp. The Weeknd, 21516545916 piosenek

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debutants <- df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(year_min = min(released_year)) %>% 
  filter(year_min == 2022) %>% 
  select(artist.s._name)

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  filter(artist.s._name %in% debutants$artist.s._name) %>% 
  mutate(mode_key = paste(mode, key)) %>% 
  group_by(artist.s._name, mode_key) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = mode_key, values_from = count, values_fill = 0)
  
## Odp. Zrobione, ale zestawienie jest za duże, żeby tu wstawić i nie popsuć czytelności

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists,
         in_spotify_charts == 0,
         in_apple_charts > 0) %>% 
  select(track_name)

## Odp. Jest 337 takich piosenek, wiec nie wklejam zestawienia

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  mutate(solo = case_when(artist_count == 1 ~ 'solo',
                   TRUE ~ 'with_others')) %>% 
  group_by(artist.s._name, solo) %>% 
  summarise(total_songs = n(), total_streams = sum(as.numeric(streams), na.rm = TRUE), .groups = 'drop') %>% 
  mutate(streams_by_songs_mean = total_streams / total_songs) %>% 
  group_by(artist.s._name) %>% 
  summarise(
    solo_streams_by_song = sum(streams_by_songs_mean[solo == 'solo']),
    with_others_streams_by_song = sum(streams_by_songs_mean[solo == 'with_others'])
  ) %>% 
  filter(solo_streams_by_song > 0, with_others_streams_by_song > 0) %>% 
  arrange(desc(solo_streams_by_song > with_others_streams_by_song))

## Odp. Sporo ich, sa wymienieni w powyzszym zestaiweniu



