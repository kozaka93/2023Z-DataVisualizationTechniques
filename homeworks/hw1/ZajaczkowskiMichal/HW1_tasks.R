library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
head(df)
df %>% 
  filter(released_year == 2023, released_month == 1 | released_month == 2 | released_month == 3) %>% 
  mutate(streams_asInt = strtoi(streams)) %>% 
  summarise(mean = mean(streams_asInt, na.rm = T))

  
## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(artist_count_v2 = case_when(artist_count == 1 ~ "1/2",
                                     artist_count == 2 ~ "1/2",
                                     TRUE ~ "More")) %>% 
  group_by(artist_count_v2) %>% 
  summarise(mean_in_playlists = mean(in_spotify_playlists))

## Odp. Nie - piosenki z 1/2 wykonawcami pojawiaja sie na playlistach srednio 5384 razy, a te tworzone przez wieksza liczbe artystow srednio 3823 razy

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(weekday) %>% 
  summarise(suma = n()) %>% 
  arrange(-suma) %>% 
  head(1)

## Odp. piątek - 526 piosenek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

artists_21 <- df %>%
  filter(artist_count == 1) %>% 
  filter(released_year == 2021) %>% 
  group_by(artist.s._name) %>% 
  summarise(NrTracks21 = n()) 

artists_22 <- df %>% 
  filter(artist_count == 1) %>% 
  filter(released_year == 2022) %>% 
  group_by(artist.s._name) %>% 
  summarise(NrTracks22 = n())

merge(artists_22, artists_21) %>% 
  mutate(percentageIncrease = (NrTracks22 - NrTracks21)/NrTracks21 *100) %>% 
  arrange(-percentageIncrease) %>% 
  head(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  top_frac(0.1, danceability_.) %>%
  mutate(streams_asInt = strtoi(streams)) %>%
  group_by(track_name) %>% 
  mutate(mean_views = (streams_asInt / (2024 - released_year))) %>% 
  arrange(-mean_views) %>% 
  select(artist.s._name) %>% 
  head(1)
  
## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>% 
  mutate(streams_asInt = strtoi(streams)) %>%
  mutate(freq_by_playlist = streams_asInt/in_spotify_playlists) %>% 
  top_frac(0.2, freq_by_playlist) %>% 
  summarise(mean(bpm))
  
  
df %>% 
  mutate(streams_asInt = strtoi(streams)) %>%
  mutate(freq_by_playlist = streams_asInt/in_spotify_playlists) %>% 
  top_frac(0.2, freq_by_playlist) %>% 
  select(mode) %>% 
  table

## Odp. majczestszy mode - Minor, srednie bpm - 125.2

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
# srednie staty w zaleznosci od pory roku
df %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(season = case_when((released_month %in% c(4, 5))  | (released_month == 3 & released_day %in% c(21:31)) | (released_month == 6 & released_day %in% c(1:21)) ~ "wiosna",
                            (released_month %in% c(7, 8)) | (released_month == 6 & released_day %in% 22:30) | (released_month == 9 & released_day %in% 1:22) ~ "lato",
                            (released_month %in% c(10, 11)) | (released_month == 9 & released_day %in% 23:30) | (released_month == 12 & released_day %in% 1:21) ~ "jesien",
                            TRUE ~ "zima"
                            )) %>% 
  group_by(season) %>% 
  summarise(mean_dance = mean(danceability_., na.rm = T),
            mean_valence = mean(valence_., na.rm = T),
            mean_energy = mean(energy_., na.rm = T),
            mean_acousticness = mean(acousticness_., na.rm = T),
            mean_instrumentalness = mean(instrumentalness_., na.rm = T),
            mean_liveness = mean(liveness_., na.rm = T),
            mean_speechiness = mean(speechiness_., na.rm = T))


## Odp.
# season mean_dance mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechiness
# <chr>       <dbl>        <dbl>       <dbl>             <dbl>                 <dbl>         <dbl>            <dbl>
# 1 jesien       63.9         47.0        61.2              29.7                 1.96           17.6             9.84
# 2 lato         68.6         51.5        65.8              24.7                 2.17           18.0            10   
# 3 wiosna       68.6         49.5        64.2              27.7                 1.67           18.3            11.0 
# 4 zima         66.7         57.5        66.0              25.7                 0.730          18.7             9.56


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>%
  filter(released_year == 2022) %>%
  filter(artist_count == 1) %>% 
  mutate(key_mode = paste(key, mode, sep = "-")) %>% 
  group_by(key_mode) %>% 
  summarise(x = n()) %>% 
  arrange(-x)

## Odp. G-Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  mutate(streams_asInt = as.double(streams)) %>%
  select(artist.s._name, streams_asInt) %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarize(total_views = sum(streams_asInt, na.rm = T)) %>% 
  arrange(-total_views) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

x <- df %>%
  group_by(artist.s._name) %>% 
  summarize(dataDebiutu = min(released_year)) %>% 
  filter(dataDebiutu == 2022) %>% 
  select(artist.s._name)


y <- df %>% 
  mutate(key_mode = paste(key, mode, sep = "-")) %>% 
  group_by(artist.s._name, key_mode) %>% 
  summarise(total_songs = n()) %>% 
  pivot_wider(names_from = key_mode, values_from = total_songs, values_fill = 0)
  
merge(x, y) %>% 
  View()
## Odp.



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts >=1)
  
## Odp.

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
x <- df %>% 
  mutate(streams_asDouble = as.double(streams)) %>%
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarize(mean_views_perSong_solo = mean(streams_asDouble, na.rm = T)) 

y <- df %>% 
  mutate(streams_asDouble = as.double(streams)) %>%
  filter(artist_count != 1) %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarize(mean_views_perSong_colab = mean(streams_asDouble, na.rm = T))

merge(x, y) %>% 
  filter(mean_views_perSong_solo > mean_views_perSong_colab)

str(df)
## Odp.



