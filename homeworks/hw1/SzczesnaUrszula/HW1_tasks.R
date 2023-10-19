library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month >= 1, released_month <= 3) %>% 
  filter( streams >=0) %>% 
  summarise(mean_streams = mean(as.numeric(streams)) )

## Odp. Średnia ilość odtworzeń to 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(artist_count = case_when(
    artist_count == 1 ~ "1",
    artist_count == 2 ~ "2",
    artist_count > 2 ~"more than 2"
  )) %>% 
  group_by(artist_count) %>% 
  summarise(playlist_count = sum(in_spotify_playlists)) 
  
  

## Odp. Nie, najwięcej na playlistach jest piosenek stworzonych przez 1 artystę a potem przez 2. 
## Piosenek stworzonych przez więcej niż 2 artystów jest prawie 8 razy mniej niz tych stworzonych 
## przez 1 artystę


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  select(released_year, released_month, released_day) %>% 
  mutate(release_day = as.Date(paste(released_year, released_month, 
                                     released_day, sep = "-"))) %>% 
  mutate(week_day = weekdays(release_day)) %>% 
  group_by(week_day) %>% 
  summarise(count = n()) %>% 
  top_n(1) 


## Odp. piątek- 526

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df %>% 
  filter(released_year %in% c(2021, 2022), artist_count == 1) %>% 
  group_by(artist.s._name, released_year) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = released_year, values_from = n ) %>% 
  rename(release_year_21 = '2021', release_year_22 = '2022') %>% 
  mutate(precentage = (release_year_22 - release_year_21)* 100 /release_year_21) %>% 
  arrange(-precentage) %>% 
  head(1)
 
  
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  top_n(n = nrow(df) * 0.10) %>% 
  select(track_name, artist.s._name, released_year, streams) %>%
  mutate(years_since_release = 2023 - released_year + 1) %>% 
  mutate(streams_per_year = as.numeric(streams)/years_since_release) %>% 
  arrange(-streams_per_year) %>% 
  head(1)
## Odp. TQG Karol G, Shakira 

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


df %>% 
  mutate(stream_per_playlist = as.numeric(streams) / as.numeric(in_spotify_playlists) )%>% 
  arrange(-stream_per_playlist) %>% 
  slice(1: round(0.2 * n())) -> top_songs

average_bpm <- top_songs %>% 
  summarise(mean_bpm = mean(bpm))

most_frequent_mode <- top_songs %>%
  count(mode) %>%
  arrange(desc(n)) %>%
  head(1)
## Odp. Srednie tempo to 	125.2775, a najczęsciej występująca skala to Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(season = case_when(
    released_month %in% 3:5 ~ "Wiosna",
    released_month %in% 6:8 ~ "Lato",
    released_month %in% 9:11 ~ "Jesień",
    TRUE ~ "Zima"
  )) %>% 
  group_by(season) %>% 
  summarise(
    mean_dancebility = mean(danceability_.),
    mean_valence = mean(valence_.),
    mean_energy = mean(energy_.),
    mean_acousticness = mean(acousticness_.),
    mean_instrumentalness = mean(instrumentalness_.),
    mean_liveness = mean(liveness_.),
    mean_speachiness = mean(speechiness_.)
  ) 

## Odp.  season mean_dancebility mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speachiness
#<chr>             <dbl>        <dbl>       <dbl>             <dbl>                 <dbl>         <dbl>            <dbl>
#1 Jesień             65.3         46.2        62.2              26.7                 2.02           18.0            10.2 
#2 Lato               69.2         51.2        65.8              23.7                 2.74           17.6             9.71
#3 Wiosna             68.0         51.0        64.3              28.3                 1.4            18.4            11.0 
#4 Zima               65.7         56.1        64.7              28.5                 0.596          18.7             9.50

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022) %>% 
  group_by(key , mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(10) -> top_all

df %>% 
  filter(released_year == 2022, artist_count == 1) %>% 
  select(artist.s._name, key, mode) %>% 
  group_by(key, mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1) 
  
  
 ## Odp. G     Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_sum = sum(as.numeric(streams))) %>% 
  top_n(1, streams_sum)

## Odp.The Weeknd     14185552870

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(debute_year = min(released_year)) %>% 
  filter(debute_year == 2022) -> debute_2022

df %>% 
  inner_join(debute_2022, by = "artist.s._name") %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(total_songs = n()) %>% 
  pivot_wider(names_from = c("mode","key") , values_from = total_songs,
              values_fill = 0) -> result_10


## Odp. result_10



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0, in_apple_charts != 0) %>% 
  select(track_name) -> result_11
  View()

## Odp. result_11



#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(n_artists = n(), sum_streams = sum(as.numeric(streams))) %>% 
  mutate(streams_per_song_solo = sum_streams / n_artists) %>% 
  select(artist.s._name, streams_per_song_solo) -> solo
  
df %>% 
  filter(artist_count != 1) %>% 
  separate_longer_delim(artist.s._name, delim = ", ")  %>% 
  group_by(artist.s._name) %>% 
  summarise(n_artists = n(), sum_streams = sum(as.numeric(streams))) %>%
  mutate(streams_per_song_with_others = sum_streams / n_artists) %>% 
  select(artist.s._name, streams_per_song_with_others) -> with_others

with_others %>% 
  inner_join(solo, by = "artist.s._name") %>% 
  filter(streams_per_song_with_others  < streams_per_song_solo) %>% 
  arrange(-streams_per_song_solo) -> result_12

  
## Odp. result_12



