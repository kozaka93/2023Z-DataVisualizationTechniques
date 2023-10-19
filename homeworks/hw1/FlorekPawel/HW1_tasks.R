library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('C:/PW/TWD/HW1/spotify-2023.csv')
df <- df %>% mutate(streams = as.numeric(streams))


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023 & released_month <= 3) %>% 
  summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  filter(artist_count > 2) %>% 
  summarise(sum = sum(in_spotify_playlists)) -> sum1

df %>% 
  filter(artist_count <= 2) %>% 
  summarise(sum = sum(in_spotify_playlists)) -> sum2

sum1 > sum2

## Odp. FALSE


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(day_of_the_week = weekdays(date)) %>% 
  group_by(day_of_the_week) %>% 
  summarise(n = n()) %>% 
  top_n(1)

## Odp. piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? 
#### (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1 & released_year == 2021) %>% 
  select(artist.s._name, released_year) %>% 
  group_by(artist.s._name) %>% 
  summarise(n_2021 = n()) -> df2021

df %>% 
  filter(artist_count == 1 & released_year == 2022) %>% 
  select(artist.s._name, released_year) %>% 
  group_by(artist.s._name) %>% 
  summarise(n_2022 = n()) -> df2022
  
left_join(df2021, df2022, by = "artist.s._name") %>% 
  mutate(change = (n_2022 - n_2021)/n_2021 * 100) %>% 
  select(artist.s._name, change) %>% 
  top_n(1, change)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  top_frac(0.1, danceability_.) %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(streams_per_year = ifelse(released_year == 2023, streams,
                                   streams/(as.numeric(difftime(as.Date(Sys.time()), date, units = "days"))/365.25))) %>% 
  top_n(1, streams_per_year) %>% 
  select(track_name, artist.s._name)
  

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>% 
  mutate(streams_per_pl = streams/in_spotify_playlists) %>% 
  top_frac(0.2, streams_per_pl) -> df6

df6 %>% 
  summarise(mean_bpm = mean(bpm, na.rm = TRUE))

table(df6$mode)

## Odp. mean_bpm = 125.2, most_frequent_mode = minor 

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(season = case_when((released_month == 12 & released_day >= 21) | (released_month == 3 & released_day <= 20) | released_month == 1 | released_month == 2 ~ "Winter",
                            (released_month == 3 & released_day >= 21) | (released_month == 6 & released_day <= 20) | released_month == 4 | released_month == 5  ~ "Spring",
                            (released_month == 6 & released_day >= 21) | (released_month == 9 & released_day <= 22) | released_month == 7 | released_month == 8  ~ "Summer",
                            TRUE ~ "Autumn")) %>% 
  select(season, danceability_.:speechiness_.) %>% 
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.), mean_valence = mean(valence_.), mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.), mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.), mean_liveness = mean(liveness_.), mean_speechiness = mean(speechiness_.)) %>% 
  View()


## Odp. W zimę piosenki są najbardziej pozytywne, energiczne i żywe. Na wiosnę piosenki
## są najbardziej taneczne i posiądają najwięcej słów.
## W lato piosenki posiadają najwięcej elementów instrumentalnych. Na jesień piosenki
## są najbardziej akuctyczne.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(artist_count == 1) %>% 
  filter(released_year == 2022 & key != "" & mode != "") %>% 
  mutate(key_mode = paste(key, mode, sep = " - ")) -> df8

df8 %>% 
  group_by(key_mode) %>%  
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  pull(key_mode) -> top_key_mode

num_art <- df %>%
  select(artist_count) %>%
  max()
artists <- paste("artist", 1:num_art, sep = "_")

df %>%
  filter(artist_count > 1) %>%
  separate(artist.s._name, into = artists, sep = ", ", remove = TRUE, fill = "right") %>% 
  select(artists) %>% 
  pivot_longer(artists, names_to = NULL, values_to = "artist") %>% 
  unique(na.rm = TRUE) %>% 
  pull(artist) -> not_solo

df8 %>% 
  filter(!(artist.s._name %in% not_solo) & key_mode %in% top_key_mode) %>% 
  group_by(key_mode) %>%  
  summarise(n = n()) %>% 
  top_n(1, n)
  

## Odp. G - Major, C# - Major, G# - Major, wszystkie po 10

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>% 
  top_n(1, total_streams)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df %>%
  separate(artist.s._name, into = artists, sep = ", ", remove = TRUE, fill = "right") %>% 
  pivot_longer(artists, names_to = NULL, values_to = "artist") %>% 
  group_by(artist) %>% 
  summarise(debut_year = min(released_year)) %>% 
  filter(debut_year == 2022) %>% 
  select(artist) %>% 
  unique(na.rm = TRUE) %>% 
  pull(artist) -> artists_debut_2022

df10 %>% 
  filter(artist %in% artists_debut_2022) %>% 
  select(artist, mode) %>% 
  group_by(artist, mode) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = mode, values_from = n, values_fill = 0) -> artists_mode

df10 %>% 
  filter(artist %in% artists_debut_2022) %>% 
  filter(key != "") %>% 
  select(artist, key) %>% 
  group_by(artist, key) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = key, values_from = n, values_fill = 0) -> key_mode
  

## Odp.
key_mode

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
## Odp.
df %>% 
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  pull(track_name)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
not_solo_streams <- df %>%
  filter(artist_count > 1) %>% 
  separate(artist.s._name, into = artists, sep = ", ", remove = TRUE, fill = "right") %>% 
  pivot_longer(artists, names_to = NULL, values_to = "artist") %>% 
  group_by(artist) %>% 
  summarise(mean_streams_group = mean(streams))

solo_streams <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(mean_streams_solo = mean(streams))

df12 <- merge(not_solo_streams, solo_streams, by.x = "artist", by.y = "artist.s._name") %>% 
  filter(mean_streams_solo > mean_streams_group)


## Odp. 
df12$artist
