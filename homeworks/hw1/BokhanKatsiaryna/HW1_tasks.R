library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df$streams = as.numeric(df$streams)

df %>% 
  filter(released_year == 2023 & released_month >0 & released_month<4 & is.numeric(streams)) %>% 
  summarise(mean = mean(streams,  rm.na = TRUE))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  filter(artist_count>2) %>%
  summarise(mean_spotify_playlists = mean(in_spotify_playlists,  rm.na = TRUE))

df %>% 
  filter(artist_count==1 | artist_count==2) %>%
  summarise(mean_spotify_playlists = mean(in_spotify_playlists,  rm.na = TRUE))

## Odp. Nie, tak nie jest, bo mean_spotify_playlists dla 1 lub 2 artystów  = 5383.583, a dla większej liczby artystów - 3822.554


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(day_of_week = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>% 
  group_by(day_of_week) %>% 
  summarise(total_number_of_released_songs = n()) %>% 
  arrange(desc(total_number_of_released_songs)) %>% 
  head(1) %>%
  select(day_of_week)

## Odp. Friday

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

data_2022 <- df%>% 
  filter(artist_count==1 & (released_year==2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise("released_2022" = n()) %>% 
  filter(released_2022 > 0) %>% 
  select(artist.s._name, released_2022)

data_2021 <- df %>% 
  filter(artist_count==1 & (released_year==2021)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise("released_2021" = n()) %>% 
  filter(released_2021 > 0) %>% 
  select(artist.s._name, released_2021)

inner_join(data_2021, data_2022, by = "artist.s._name") %>% 
  mutate(increase = released_2022/released_2021) %>% 
  arrange(desc(increase)) %>% 
  head(1) %>% 
  select(artist.s._name)


## Odp. SZA (in 2021 - 1 song, in 2022 - 17 songs - powiększenie w 17 razy) 

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


df %>% 
  arrange(desc(danceability_.)) %>% 
  head(n = round(0.10 * nrow(df))) %>%
  select(artist.s._name, released_year, streams, danceability_.) %>% 
  mutate(years_after_release = 2024-released_year) %>% 
  mutate(streams_per_year = streams/years_after_release) %>% 
  arrange(desc(streams_per_year)) %>% 
  head(1) %>% 
  select(artist.s._name)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams_per_playlist_spotify = streams/in_spotify_playlists) %>% 
  arrange(desc(streams_per_playlist_spotify)) %>% 
  head(n = round(0.20 * nrow(df))) %>% 
  group_by(mode) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

df %>% 
  mutate(streams_per_playlist_spotify = streams/in_spotify_playlists) %>% 
  arrange(desc(streams_per_playlist_spotify)) %>% 
  head(n = round(0.20 * nrow(df))) %>%
  summarise(mean_tempo = mean(bpm,  rm.na = TRUE))


## Odp. Średni temp = 125.2775; najczęściejszy mode - Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(season = case_when(
    released_month %in% c(1,2) | (released_month==12 & released_day >21) | (released_month==3 & released_day<21) ~ "Winter",
    released_month %in% c(4,5) | (released_month==3 & released_day >20) | (released_month==6 & released_day<21) ~ "Spring",
    released_month %in% c(7,8) | (released_month==6 & released_day >20) | (released_month==9 & released_day<23) ~ "Summer",
    T ~ "Autumn"
  )) %>% 
  select(season, danceability_., energy_., valence_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  group_by(season) %>% 
  summarise(mean_dancebility = mean(danceability_.,  rm.na = TRUE),
            mean_energy = mean(energy_.,  rm.na = TRUE),
            mean_acousticness = mean(acousticness_.,  rm.na = TRUE),
            mean_instrumentalness = mean(instrumentalness_.,  rm.na = TRUE),
            mean_liveness = mean(liveness_.,  rm.na = TRUE),
            mean_speechiness = mean(speechiness_.,  rm.na = TRUE),
            mean_valence = mean(valence_.,  rm.na = TRUE)) %>% 
  View()


## Odp. - data frame z średnimi wartościami.


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

top_10_most_popular_key_mode_2022 <- df %>% 
  filter(released_year==2022) %>% 
  group_by(key, mode) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  select(key, mode)

df_key_mode <- df %>% 
  filter(artist_count==1) %>% 
  group_by(key, mode) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

inner_join(top_10_most_popular_key_mode_2022, df_key_mode, by=c("key", "mode")) %>% 
  arrange(desc(n)) %>% 
  head(1) %>% 
  select(key, mode)


## Odp. key-mode = G-Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df$streams = as.numeric(df$streams)
df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8")

df %>%
  filter(is.character(artist.s._name)) %>% 
  separate_rows(artist.s._name, sep=", ") %>%
  group_by(artist.s._name) %>%
  summarize(total_number_of_streams = sum(streams,  rm.na = TRUE)) %>% 
  arrange(desc(total_number_of_streams)) %>% 
  head(1) 
  
## Odp: The Weeknd with total_number_of_streams (in a group and solo): 23929760758

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

# zadebiutowanych artystów będziemy szukać nawet nie w solo piosenkach
df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8")
df_separated <- df %>% 
  separate_rows(artist.s._name, sep = ", ")

df_first_appearence_2022 <- df_separated %>% 
  group_by(artist.s._name) %>% 
  summarise(first_appearence = min(released_year)) %>% 
  filter(first_appearence==2022) %>% 
  select(artist.s._name)

answer10 <- inner_join(df_first_appearence_2022, df_separated, by="artist.s._name") %>%
  group_by(artist.s._name,key,mode) %>% 
  summarise(number_of_songs=n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = number_of_songs, values_fill = 0)

## Odp. znajduje się w answer10

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

answer11 <- df %>% 
  filter(in_spotify_playlists>in_apple_playlists & (!is.na(in_apple_charts) & in_apple_charts>0) & (in_spotify_charts==0 | is.na(in_spotify_charts)) ) %>% 
  select(track_name)

## Odp. znajduje się w answer11


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

# liczę, że jeżli artysta ma piosenki tylko solo, a z innymi artystami żadnej wspólnej piosenki nie ma, to taki artysta nam pasuje.

df$streams = as.numeric(df$streams)
df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8")

streams_per_song_solo <- df %>% 
  filter(artist_count==1) %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_per_song_solo = mean(streams,  rm.na = TRUE))

streams_per_song_group <- df %>%  
  filter(artist_count>1) %>% 
  separate_rows(artist.s._name, sep=", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_per_song_group = mean(streams, rm.na = TRUE)) 

answer12 <- left_join(streams_per_song_solo, streams_per_song_group, by="artist.s._name") %>% 
  filter(streams_per_song_solo>streams_per_song_group | is.na(streams_per_song_group)) %>% 
  select(artist.s._name, streams_per_song_solo, streams_per_song_group)

## Odp. znajduje się w answer12

