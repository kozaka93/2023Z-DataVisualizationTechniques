library(tidyr)
library(dplyr)
#install.packages("hydroTSM")
library(hydroTSM)

df <- read.csv("spotify-2023.csv")


# 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023, released_month < 4) %>% 
  summarise(strams_mean = mean(as.double(streams)))

## Odp. średnia odtworzeń: 216150568



# 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#    niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  filter(artist_count > 2) %>% 
  summarise(sum(in_spotify_playlists)) -> tmp1

df %>% 
  filter(artist_count <= 2) %>% 
  summarise(sum(in_spotify_playlists)) -> tmp2

odp2 <- tmp1 > tmp2

## Odp. nie

  

# 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, sep = '-'))) %>% 
  mutate(release_weekday = weekdays(release_date)) %>% 
  group_by(release_weekday) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  head(1) -> odp3

## Odp. piątek - 526



# 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#    Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  select(track_name, artist.s._name, artist_count, released_year) %>% 
  filter(artist_count == 1, (released_year == 2021 | released_year == 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(track_count = n()) %>% 
  pivot_wider(names_from = released_year, values_from = track_count) %>% 
  mutate(growth_percent = (`2022` - `2021`) * 100 / `2021`) %>% 
  filter(!is.na(growth_percent)) %>% 
  arrange(desc(growth_percent)) %>% 
  head(1) -> odp4

## Odp. SZA - 1600%



# 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
#    Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  select(track_name, artist.s._name, released_year, streams, danceability_.) %>% 
  mutate(mean_streams_per_year = as.double(streams) / (2024 - released_year)) %>% 
  arrange(desc(danceability_.)) %>% 
  head(n = 0.1 * nrow(df)) %>%
  arrange(desc(mean_streams_per_year)) %>%
  head(1) -> odp5
  
## Odp. Chencho Corleone, Bad Bunny - 720378909



# 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#    które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  select(track_name, streams, in_spotify_playlists, bpm) %>% 
  mutate(streams_by_playlist = as.double(streams) / df$in_spotify_playlist) %>%
  arrange(-streams_by_playlist) %>% 
  head(n = 0.2 * nrow(df)) %>% 
  summarise(mean_bpm = mean(bpm)) -> odp6a

df %>% 
  select(track_name, streams, in_spotify_playlists, mode) %>% 
  mutate(streams_by_playlist = as.double(streams) / df$in_spotify_playlist) %>%
  arrange(-streams_by_playlist) %>% 
  head(n = 0.2 * nrow(df)) %>%
  group_by(mode) %>% 
  summarise(count = n()) %>%
  top_n(1, count) -> odp6b

## Odp. BPM: 125.2, Mode: Minor - 96



# 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = time2season(as.Date(paste(released_year, released_month, released_day, sep = '-')), out.fmt = "seasons")) %>%
  group_by(season) %>% 
  summarise(danceability = mean(danceability_.),
            valence = mean(valence_.), 
            energy = mean(energy_.), 
            acousticness = mean(acousticness_.),
            instrumentalness = mean(instrumentalness_.),
            liveness = mean(liveness_.),
            speechiness = mean(speechiness_.)) -> odp7

## Odp.   season danceability valence energy acousticness instrumentalness liveness speechiness
#         <chr>         <dbl>   <dbl>  <dbl>        <dbl>            <dbl>    <dbl>       <dbl>
#       1 autumm         65.3    46.2   62.2         26.7            2.02      18.0       10.2 
#       2 spring         68.0    51.0   64.3         28.3            1.4       18.4       11.0 
#       3 summer         69.2    51.2   65.8         23.7            2.74      17.6        9.71
#       4 winter         65.7    56.1   64.7         28.5            0.596     18.7        9.50




# 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(count = n()) %>%
  arrange(-count) %>% 
  head(10) -> tmp
  
  df %>% 
    inner_join(tmp, by = c('key' = 'key', 'mode' = 'mode')) %>% 
    filter(artist_count == 1) %>%
    group_by(key, mode) %>% 
    summarise(count_solo = n()) %>% 
    arrange(-count_solo) %>%
    head(1) -> odp8
    
  
  
## Odp. "" - Major - 47



# 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  rowwise() %>% 
  mutate(artist = strsplit(artist.s._name, ", ")) %>%
  unnest(artist) %>%
  select(artist, streams) %>%
  group_by(artist) %>%
  summarise(total_streams = sum(as.double(streams), na.rm = TRUE)) %>% 
  arrange(-total_streams) %>%
  head(1) -> odp9

## Odp. The Weeknd - 23929760757



# 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
#     zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
#     W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(debute = min(released_year)) %>% 
  filter(debute == 2022) -> artists

df  %>% 
  inner_join(artists, by = c('artist.s._name' = 'artist.s._name')) %>% 
  group_by(artist.s._name, mode, key) %>% 
  summarise(count = n()) %>%
  pivot_wider(names_from = c(mode, key), values_from = count, values_fill = 0) -> odp10

## Odp. odp10



# 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
#     nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts != 0) %>% 
  select(track_name, artist.s._name) -> odp11

## Odp. odp11



# 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>%
  summarise(mean_solo_streams = mean(as.double(streams), na.rm = T)) -> solo_streams

df %>% 
  filter(artist_count != 1) %>% 
  rowwise() %>% 
  mutate(artist = strsplit(artist.s._name, ", ")) %>%
  unnest(artist) %>%
  group_by(artist) %>% 
  summarise(mean_coop_streams = mean(as.double(streams), na.rm = T)) -> coop_streams

solo_streams %>% 
  inner_join(coop_streams, by = c("artist.s._name" = "artist")) %>% 
  filter(mean_solo_streams > mean_coop_streams, mean_coop_streams != 0) -> odp12

## Odp. odp12