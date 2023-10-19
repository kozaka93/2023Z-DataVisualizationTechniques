library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & released_month < 4) %>% 
  transmute(num_streams = as.numeric(streams)) %>% 
  summarise(mean_num_streams = mean(num_streams))

## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(more_than_2_artists = ifelse(artist_count <= 2, FALSE, TRUE)) %>% 
  group_by(more_than_2_artists) %>% 
  summarise(num_spotify_playlist = sum(in_spotify_playlists)) %>% 
  top_n(1, num_spotify_playlist) %>% 
  select(more_than_2_artists)

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

library(lubridate)
#install.packages("clock")
library(clock)

df %>%
  mutate(
    date = date_build(released_year, released_month, released_day), 
    .keep = "unused", 
    .before = 1
  ) %>% 
  mutate(released_day_of_week = weekdays(date)) %>% 
  group_by(released_day_of_week) %>% 
  summarise(num_of_releases_on_that_day_of_week = n()) %>% 
  arrange(-num_of_releases_on_that_day_of_week) %>% 
  top_n(1, num_of_releases_on_that_day_of_week) %>% 
  select(released_day_of_week)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

library(stringr)
df$artist.s._name <- str_remove(df$artist.s._name, "<ef>")

df %>% 
  filter(released_year == 2021 | released_year == 2022) %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(num_of_released_songs_that_year = n()) %>%  
  group_by(artist.s._name) %>% 
  mutate(released_songs_in_21_and_22 = sum(released_year)) %>% 
  filter(released_songs_in_21_and_22 == 4043) %>% 
  arrange(-released_year) %>% 
  arrange(artist.s._name) %>% 
  mutate(temp = ifelse(released_year == 2021, -num_of_released_songs_that_year, num_of_released_songs_that_year)) %>% 
  group_by(artist.s._name) %>% 
  mutate(difference = sum(temp)) %>% 
  filter(released_year == 2021) %>% 
  mutate(percent_increase_in_songs_released = difference*100 / num_of_released_songs_that_year, .keep = "used") %>% 
  select(artist.s._name, percent_increase_in_songs_released) %>% 
  arrange(-percent_increase_in_songs_released) %>% 
  head(1) %>% 
  select(artist.s._name)
  
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  top_n(0.1*nrow(df), danceability_.) %>% 
  mutate(age = 2024-released_year) %>% 
  mutate(stream_per_year = as.numeric(streams)/age) %>% 
  select(artist.s._name, stream_per_year, track_name) %>% 
  arrange(-stream_per_year) %>% 
  top_n(1, stream_per_year) %>% 
  select(artist.s._name)

## Odp. Chencho Corleone i Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(popularity = as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(-popularity) %>% 
  top_n(0.2*nrow(df)) %>% 
  select(mode, bpm) %>%
  mutate(mean_bpm = mean(bpm)) %>% 
  group_by(mode) %>% 
  mutate(count_mode = n()) %>% 
  filter(count_mode >= 0.5*0.2*nrow(df)) %>% 
  select(mode, mean_bpm) %>%
  head(1)
  
## Odp. minor i 125.2

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

#install.packages("hydroTSM")
library("hydroTSM")

df %>%
  mutate(
    date = date_build(released_year, released_month, released_day), 
    .keep = "unused", 
    .before = 1
  ) %>% 
  mutate(season = time2season(date, out.fmt = "seasons")) %>% 
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.), mean_valence = mean(valence_.), mean_energy = mean(energy_.), 
            mean_acousticness = mean(acousticness_.), mean_instrumentalness = mean(instrumentalness_.), 
            mean_livenes = mean(liveness_.), mean_speechiness = mean(speechiness_.))

## Odp. 
#         season      mean_danceability   mean_valence    mean_energy    mean_acousticness   mean_instrumentalness   mean_livenes    mean_speechiness
#         <chr>            <dbl>            <dbl>           <dbl>             <dbl>                   <dbl>             <dbl>            <dbl>
#       1 autumm          65.3             46.2             62.2              26.7                    2.02              18.0            10.2 
#       2 spring          68.0             51.0             64.3              28.3                    1.4               18.4            11.0 
#       3 summer          69.2             51.2             65.8              23.7                    2.74              17.6             9.71
#       4 winter          65.7             56.1             64.7              28.5                    0.596             18.7             9.50

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(10) %>% 
  inner_join(df, by=c("key", "mode")) %>% 
  filter(artist_count == 1 & released_year == 2022) %>% 
  group_by(key, mode) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(1)

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  separate_rows(artist.s._name, sep= ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams))) %>% 
  arrange(-total_streams) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp. The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

artists_with_2022_debuts <- df %>% 
  filter(artist_count == 1) %>% 
  mutate(before_2022 = ifelse(released_year < 2022, 1, 0)) %>% 
  group_by(artist.s._name) %>% 
  summarise(years_before_22 = sum(before_2022)) %>% 
  filter(years_before_22 == 0) %>% 
  select(artist.s._name) %>% 
  inner_join(df, "artist.s._name") %>% 
  mutate(in_2022 = ifelse(released_year == 2022, 1, 0)) %>% 
  group_by(artist.s._name) %>% 
  summarise(in_22 = sum(in_2022)) %>% 
  filter(in_22 > 0) %>% 
  select(artist.s._name)

mode_count <- artists_with_2022_debuts %>% 
  inner_join(df, "artist.s._name") %>% 
  group_by(artist.s._name, mode) %>% 
  count() %>% 
  pivot_wider(names_from = mode, values_from = n, values_fill = 0)

key_count <- artists_with_2022_debuts %>% 
  inner_join(df, "artist.s._name") %>% 
  filter(key != "") %>% 
  group_by(artist.s._name, key) %>% 
  count() %>% 
  pivot_wider(names_from = key, values_from = n, values_fill = 0)

mode_key_chart_debut_22 <- full_join(mode_count, key_count, "artist.s._name")

View(mode_key_chart_debut_22)

## Odp. Odpowiedz w ramce: mode_key_chart_debut_22.


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

songs_not_on_charts_spotify_apple <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name, artist.s._name)

View(songs_not_on_charts_spotify_apple)

## Odp. Jest 337 takich piosenek. Odpowiedz w ramce: songs_not_on_charts_spotify_apple.


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

artists_with_more_streams_solo <- df %>% 
  separate_rows(artist.s._name, sep= ", ") %>% 
  mutate(artists_count_1 = ifelse(artist_count == 1, "alone", "with_others")) %>% 
  group_by(artist.s._name, artists_count_1) %>% 
  summarise(mean_streams_per_song = mean(as.numeric(streams))) %>% 
  pivot_wider(names_from = artists_count_1, values_from = mean_streams_per_song) %>% 
  filter(alone > with_others)

View(artists_with_more_streams_solo)

## Odp. Jest 58 takich artystów. Odpowiedz w ramce: artists_with_more_streams_solo.