library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(released_year == 2023, released_month %in% c(1,2,3), !is.na(streams)) %>%
  mutate(streams = as.numeric(streams)) %>%
  summarize(mean_streams = mean(streams, na.rm = TRUE))


## Odp.
## 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>%
  select(artist_count, in_spotify_playlists) %>%
  summarize(
    sum_artists_gt_2 = sum(in_spotify_playlists[artist_count > 2], na.rm = TRUE),
    sum_artists_leq_2 = sum(in_spotify_playlists[artist_count <= 2], na.rm = TRUE),
    answer = sum_artists_gt_2 > sum_artists_leq_2
  ) %>%
  select(answer)

## Odp.
## nie

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>%
  mutate(
    released_date = as.Date(paste(released_year, released_month, released_day, sep = '-')),
    day_of_week = weekdays(released_date)
  ) %>%
  group_by(day_of_week) %>%
  summarize(number_of_realeases = n()) %>%
  arrange(desc(number_of_realeases)) %>%
  head(1) %>%
  select(day_of_week)

## Odp.
# piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarize(
    number_of_realeases_2021 = sum(released_year == 2021, na.rm = TRUE),
    number_of_realeases_2022 = sum(released_year == 2022, na.rm = TRUE)
  ) %>%
  filter(number_of_realeases_2021 > 0) %>%
  mutate(percent_change = ((number_of_realeases_2022 - number_of_realeases_2021) / number_of_realeases_2021) * 100) %>%
  arrange(desc(percent_change)) %>%
  head(1) %>%
  select(artist.s._name)

## Odp.
## SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  filter(danceability_. > quantile(danceability_., 0.9, na.rm = TRUE)) %>%
  mutate(
    streams = as.numeric(streams),
    released_year = as.numeric(released_year)
  ) %>%
  group_by(track_name) %>%
  summarize(
    mean_streams_per_year = sum(streams) / (2023 - released_year + 1)
  ) %>%
  arrange(desc(mean_streams_per_year)) %>%
  head(1) %>%
  left_join(df, by = 'track_name') %>%
  select(artist.s._name)

## Odp.
## Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>%
    filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8, na.rm = TRUE)) %>%
    mutate(
        bpm = as.numeric(bpm),
        mode = as.factor(mode)
    ) %>%
    summarize(
        mean_bpm = mean(bpm, na.rm = TRUE),
        most_frequent_mode = names(which.max(table(mode)))
    )

## Odp.
## 121.3194, skala durowa

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?


## Odp.
df %>%
  mutate(
    season = case_when(
      released_month %in% c(3,4,5) ~ 'spring',
      released_month %in% c(6,7,8) ~ 'summer',
      released_month %in% c(9,10,11) ~ 'autumn',
      released_month %in% c(12,1,2) ~ 'winter'
    )
  ) %>%
  group_by(season) %>%
  summarize(
    mean_danceability = mean(danceability_.),
    mean_valence = mean(valence_.),
    mean_energy = mean(energy_.),
    mean_acousticness = mean(acousticness_.),
    mean_instrumentalness = mean(instrumentalness_.),
    mean_liveness = mean(liveness_.),
    mean_speechiness = mean(speechiness_.)
  ) %>%
  mutate(season = factor(season, levels = c('spring', 'summer', 'autumn', 'winter'))) %>%
  arrange(season)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

# Zakładam, że popularność par key-mode mierzymy ilością wydanych w nich piosenek
df %>%
  filter(released_year == 2022, key != "", mode != "") %>%
  mutate(
    key = as.factor(key),
    mode = as.factor(mode)
  ) %>%
  group_by(key, mode) %>%
  summarize(
    number_of_songs = n(),
    number_of_solo_artists = sum(artist_count == 1, na.rm = TRUE)
  ) %>%
  top_n(10, number_of_songs) %>%
  arrange(desc(number_of_solo_artists)) %>%
  head(1) %>%
  select(key, mode)

## Odp.
## G-dur

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>%
  summarize(total_streams = sum(streams, na.rm = TRUE)) %>%
  top_n(1, total_streams) %>%
  select(artist.s._name)

## Odp.
## The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 


## Odp.
df %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  group_by(artist.s._name) %>%
  mutate(min_year = min(released_year, na.rm = TRUE)) %>%
  filter(min_year == 2022, key != "", mode != "") %>%
  mutate(
    key = as.factor(key),
    mode = as.factor(mode)
  ) %>%
  ungroup() %>%
  group_by(artist.s._name, key, mode) %>%
  summarize(number_of_songs = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = number_of_songs, values_fill = 0)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
## Odp.
df %>%
  filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_spotify_charts == 0, in_apple_charts > 0) %>%
  select(artist.s._name, track_name)

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
## Odp.
df %>%
  mutate(streams = as.numeric(streams)) %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  group_by(artist.s._name) %>%
  summarize(
    mean_solo_streams = mean(streams[artist_count == 1], na.rm = TRUE),
    mean_collab_streams = mean(streams[artist_count > 1], na.rm = TRUE)
  ) %>%
  filter(mean_solo_streams > mean_collab_streams)
