library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & (released_month >= 1 & released_month <= 3)) %>% 
  summarise(avg_streams_number = mean(as.numeric(streams))) -> odp_1

## Odp. Średnia liczba odtworzeń opublikowanych w 2023 w pierwszym kwartale to 216 150 568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  filter(artist_count > 2) %>% 
  summarise(number_of_song_in_spotify_playlist = sum(in_spotify_playlists)) -> create_by_more_than_2

df %>% 
  filter(artist_count <= 2) %>% 
  summarise(number_of_song_in_spotify_playlist = sum(in_spotify_playlists)) -> create_by_less_than_3

as.numeric(create_by_less_than_3) > as.numeric(create_by_more_than_2) -> odp_2 #TRUE


## Odp. Nie, piosenki stworzone przez 1 lub 2 artystów są zawarte na większej liczbie playlist spotify niż te
##      stoworzone przez więcej niż 2 osoby.


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(Date = as.Date(paste(released_year, released_month, released_day, sep = "-"), "%Y-%m-%d")) %>%
  mutate(weekday = format(Date, "%A")) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  top_n(1) -> odp_3

## Odp. Najpopularniejszy dzień na wypuszczenie piosenki to piątek (526 wypuszczonych utworów)


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1, (released_year == 2022 | released_year == 2021)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(number_of_released_song = n()) %>% 
  pivot_wider(names_from = released_year, values_from =  number_of_released_song) %>% 
  na.omit() %>% 
  mutate(pct_increase = 100 * ((`2022` - `2021`)/`2021`)) %>% 
  arrange(-pct_increase) %>% 
  head(1) -> odp_4

## Odp. Największy procentowy wzrost wypuszczonych piosenek odnotowała SZA


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>%
  arrange(-danceability_.) %>% 
  slice_head(n = round(0.1*nrow(df))) %>% 
  mutate(avg_stream = as.numeric(streams)/(2023 - released_year + 1)) %>% 
  arrange(-avg_stream) %>% 
  select(artist.s._name, track_name, avg_stream) %>% 
  head(1) -> odp_5
  

## Odp. Wśród 10% najbardziej tanecznych piosenek, średnio najwięcej odtorzeń ma piosenka 
##      Chencho Corleone i Bad Bunny "Me Porto Bonito" 


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
  mutate(num_str = as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(-num_str) %>% 
  slice_head(n = round(0.2*nrow(df))) -> tmp

tmp %>% 
  summarise(avg_bpm = mean(bpm)) -> avg_bpm

tmp %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  top_n(1)-> mode

## Odp. 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify ma średnie tempo 125.2775,
##      a najczęsciej występująca skala to Minor (96 wystąpień na 191 piosenek)


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(day_of_year = released_month * 100 + released_day,
         season = case_when(between(day_of_year, 321, 621) ~ "Wiosna",
                            between(day_of_year, 622, 922) ~ "Lato",
                            between(day_of_year, 923, 1221) ~ "Jesien",
                            day_of_year >= 1222 | day_of_year <= 320 ~ "Zima")) %>% 
  group_by(season) %>% 
  summarise(mean_danceability_. = mean(danceability_.), mean_valence_. = mean(valence_.),
            mean_energy_. = mean(energy_.), mean_acousticness_. = mean(acousticness_.),
            mean_instrumentalness_. = mean(instrumentalness_.), mean_liveness_. = mean(liveness_.),
            mean_speechiness_. = mean(speechiness_.)) -> odp_7


## Odp.   season  mean_danceability_.  mean_valence_.  mean_energy_.  mean_acousticness_.  mean_instrumentalness_.  mean_liveness_.  mean_speechiness_.
##      1 Jesien            63.9           47.0           61.2                29.7                   1.96               17.6               9.84
##      2 Lato              68.6           51.5           65.8                24.7                   2.17               18.0               10   
##      3 Wiosna            68.6           49.5           64.2                27.7                   1.67               18.3               11.0 
##      4 Zima              66.7           57.5           66.0                25.7                   0.730              18.7               9.56


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(num_of_song = n(), num_of_solo = sum(artist_count == 1)) %>% 
  arrange(-num_of_song) %>% 
  head(10) %>%  
  mutate(per_of_solo = num_of_solo / num_of_song * 100) %>% 
  arrange(-per_of_solo) %>% 
  head(1) -> odp_8

## Odp. Wsród 10 najpopularniejszych par key-mode w roku 2022 najwięcej piosenek utworzonych przez solowych artystów
##      było pary: G-Major (75,8% piosenke tej pary było utworzonych przez solowych artystów)


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df$artist.s._name <- str_remove(df$artist.s._name, "<ef>")

df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(streams = sum(as.numeric(streams))) %>% 
  arrange(-streams) %>% 
  top_n(1, streams) -> odp_9

## Odp. Najwięcej odtworzeń w sumie ma The Weekend (23 929 760 757 odtworzeń)

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df$artist.s._name <- str_remove(df$artist.s._name, "<ef>")

df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(first_released_song = min(released_year)) %>% 
  filter(first_released_song >= 2022) -> artists_who_debute_in_2022

df %>%
  separate_rows(artist.s._name, sep = ", ") %>% 
  inner_join(artists_who_debute_in_2022, by = "artist.s._name") %>%
  mutate(mode_key = paste(mode, key, sep = "-")) %>%
  group_by(artist.s._name, mode_key) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = mode_key, values_from = n) -> odp_10

## Odp.


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts != 0) %>% 
  select(track_name, in_spotify_playlists, in_spotify_charts, in_apple_playlists, in_apple_charts) -> odp_11
  
## Odp. Jest 337 piosenek, które nie pojawiły się w zestawieniu na spotify, ale w apple tak (mimo tego, że są popularniejsze na spotify)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  mutate(solo_group = case_when(artist_count == 1 ~ "Solo",
                                 TRUE ~ "Group")) %>% 
  group_by(artist.s._name, solo_group) %>% 
  summarise(sum_streams = sum(as.numeric(streams)), number_of_song = n()) %>% 
  mutate(avg_streams = sum_streams / number_of_song) %>% 
  filter(artist.s._name != "") %>%
  select(artist.s._name, solo_group, avg_streams) %>% 
  pivot_wider(names_from = solo_group, values_from = avg_streams) %>% 
  na.omit() %>% 
  mutate(diff = Solo - Group) %>% 
  filter(diff > 0) %>% 
  select(artist.s._name)-> odp_11
  
  
## Odp. Jest 58 artystów, którzy mają średnio więcej odtworzeń na piosenkę gdy tworzą sami



