library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv',  encoding = 'UTF-8')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_month < 4 & released_year == 2023) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  summarise(average_streams = mean(streams, na.rm = TRUE))

## Odp.   average_streams = 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(artist_counter = case_when(
    artist_count <= 2 ~ "1 or 2 artist",
    artist_count > 2 ~ "more than 2 artists")) %>% 
  group_by(artist_counter) %>% 
  summarise(playlists_count = sum(in_spotify_playlists))
  

## Odp. Nie, 
##  artist_counter      playlists_count
##  <chr>                         <int>
##   1 or 2 artist               4527593
##   more than 2 artists          428126


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(day_of_week = weekdays(release_date)) %>% 
  group_by(day_of_week) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

## Odp. piątek        526

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df %>% 
  filter(artist_count == 1 & (released_year == 2021 | released_year == 2022)) %>% 
  group_by(artist.s._name , released_year) %>% 
  summarise(song_count = n()) %>% 
  pivot_wider(names_from = released_year, values_from = song_count, values_fill = 0) %>% 
  filter(as.numeric(`2021`) > 0 & as.numeric(`2022`) > 0) %>% 
  mutate(percentage_increase = ((`2022` - `2021`) / `2021`) * 100) %>% 
  arrange(-percentage_increase) %>% 
  head(1)
  
  

## Odp. SZA
##  artist.s._name `2022` `2021` percentage_increase
##  <chr>           <int>  <int>               <dbl>
##  SZA                17      1                1600

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  slice(1:round(0.10 * nrow(df))) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  filter(!is.na(streams) & !is.na(released_year)) %>%
  mutate(streams_per_year = case_when(released_year == 2023 ~ streams,
                                      released_year != 2023 ~ streams/(2023 - released_year))) %>% 
  group_by(artist.s._name) %>% 
  summarise(avg_streams_per_year = mean(streams_per_year, na.rm = TRUE)) %>% 
  arrange(-avg_streams_per_year) %>% 
  top_n(1, wt = avg_streams_per_year)
  

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams = strtoi(streams)) %>% 
  filter(!is.na(streams) & !is.na(in_spotify_playlists)) %>% 
  mutate(streams_per_playlist = streams /in_spotify_playlists) %>% 
  arrange(-streams_per_playlist) %>% 
  head(round(0.20 * nrow(df))) %>% 
  summarise(avg_tempo = mean(bpm),
            most_popular_mode = names(sort(table(mode), decreasing = TRUE)[1]))


## Odp.  average_bpm = 125.2775    najczestsza skala = Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when(
    between(released_month, 3, 5) ~ "Spring",
    between(released_month, 6, 8) ~ "Summer",
    between(released_month, 9, 11) ~ "Autumn",
    TRUE ~ "Winter"
  )) %>% 
  group_by(season) %>% 
  summarise(danceability_avg = mean(danceability_.),
            valence_avg = mean(valence_.),
            energy_avg = mean(energy_.),
            acousticness_avg = mean(acousticness_.),
            instrumentalness_avg = mean(instrumentalness_.),
            liveness_avg = mean(liveness_.),
            speechiness_avg = mean(speechiness_.)) -> by_season_characteristics

## Odp.  dane dostepne w ramce "by_season_characteristics"

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode, artist_count) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10) %>% 
  filter(artist_count == 1) %>% 
  head(1)
  

## Odp.  key = G     mode = Major 

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(strtoi(streams))) %>% 
  arrange(-total_streams) %>% 
  head(1)


## Odp. Bad Bunny

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 


debuts <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(debut = min(released_year)) %>% 
  filter(debut == 2022)

df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name != "") %>% 
  semi_join(debuts, by = "artist.s._name") %>% 
  filter(key != "") %>% 
  group_by(artist.s._name, mode, key) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = c(mode, key),
              values_from = count, values_fill = 0) -> debutants
  

## Odp. jest 247 takich artystów, zestawienie w ramce "debutants"



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>%
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts > 0) %>%
  select(track_name, artist.s._name) -> no_spotify_charts
  
## Odp. jest 337 takich piosenek, zestawienie w ramce "no_spotify_charts"


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name != "") %>%
  mutate(artist_counter = case_when(
    artist_count == 1 ~ "Solo",
    artist_count > 1 ~ "Coop")) %>% 
  group_by(artist.s._name, artist_counter) %>% 
  summarise(avg_streams = mean(strtoi(streams), na.rm = TRUE)) %>% 
  pivot_wider(names_from = artist_counter, values_from = avg_streams) %>% 
  filter(!is.na(`Solo`) & !is.na(`Coop`)) %>% 
  filter(`Solo` > `Coop`) -> better_solo


## Odp. jest 57 takich artystów, zestawienie w ramce "better_solo"



