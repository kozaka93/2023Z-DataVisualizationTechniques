library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')



#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>%
  mutate(streams = as.numeric(streams)) %>% 
  summarise(mean_streams = mean(streams, na.rm = TRUE)) %>% 
  pull(mean_streams)

## Odp. średnia liczba odtowrzeń to 216150568





#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

lq_than_two <- df %>% filter(artist_count <= 2) %>% 
  summarise(spotify_playlists = sum(in_spotify_playlists))

more_than_two <- df %>% filter(artist_count >2) %>% 
  summarise(spotify_playlists = sum(in_spotify_playlists))

more_than_two > lq_than_two


## Odp. Nie, piosenki stworzone przez 1 lub 2 artytstów są zawarte na większej liczbie playlist(4527593)
# gdzie liczba palylist na których zawarte są piosenki stworzone przez więcej niż
# 2 artystów to 428126





#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
install.packages("lubridate")
library(lubridate)

df %>%
  mutate(day_of_week = wday(ymd(paste(released_year, released_month, released_day, sep="-")), label = TRUE, week_start = 1)) %>% 
  group_by(day_of_week) %>% summarise(most_popular_day = length(day_of_week)) %>% 
  top_n(1,most_popular_day)

## Odp. Najpopularniejszy dzień tygodnia do wypuszczania piosenek to piątek





#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

artists <- df %>% filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(released_songs = n()) %>% 
  group_by(artist.s._name) %>%
  filter(n() > 1) %>%
  ungroup() 
  

artists_2021 <- artists %>% filter(released_year ==2021) %>% 
  rename(released_in_2021 = released_year) %>% 
  rename(released_songs_2021 = released_songs)

artists_2022 <- artists %>% filter(released_year == 2022) %>% 
  rename(released_in_2022 = released_year) %>% 
  rename(released_songs_2022 = released_songs)

artists <- left_join(artists_2021, artists_2022, by ="artist.s._name") %>% 
  mutate(percentage_increase = ((released_songs_2022-released_songs_2021)/released_songs_2021)*100) %>% 
  arrange(-percentage_increase) %>% top_n(1, percentage_increase) %>% 
  pull(artist.s._name)


## Odp.Największy procentowy wzrost zanotowała SZA- 1600%





#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% arrange(-danceability_.) %>% 
  slice(1:round(0.1*n())) %>% 
  mutate(streams_per_year = as.numeric(streams)/(2024 - released_year)) %>% 
  arrange(-streams_per_year) %>% head(1) %>% 
  pull(artist.s._name)

## Odp.Najwięcej odtowrzeń ma piosenka w wykonana przez duet Chencho Corleone i Bad Bunny






#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_streamed <- df %>% mutate(spotify_streams = as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(-spotify_streams) %>%  slice(1:round(0.2*n()))
mean_bpm <- top_streamed %>% summarise(mean_tempo = mean(bpm))
most_popular_mode <- top_streamed %>% group_by(mode) %>%  summarise( n = n())


## Odp.średnie tempo to 125,27 bpm, częściej występuje skala Minor(różnica o 1)





#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

get_season <- function(month) {
  if (month %in% 3:5) {
    return('Spring')
  } else if (month %in% 6:8) {
    return('Summer')
  } else if (month %in% 9:11) {
    return('Autumn')
  } else {
    return('Winter')
  }
}

seansons <- df %>% mutate(season = sapply(released_month, get_season)) %>% 
  group_by(season) %>% 
  summarise(
    mean_danceability = mean(danceability_.),
    mean_valence = mean(valence_.),
    mean_energy = mean(energy_.),
    mean_acousticness = mean(acousticness_.),
    mean_instrumentalness = mean(instrumentalness_.),
    mean_liveness = mean(liveness_.),
    mean_speechiness = mean(speechiness_.)
    
  )


## Odp.Najbardziej taneczne piosenki są wydawane w Lato, Najbardziej energiczne w lato
#a najmniej eneregiczne w zime, najwięcej słów mają te wiosenne

#season   mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechin…¹
#1 Autumn             65.3         46.2        62.2              26.7                 2.02           18.0           10.2 
#2 Spring             68.0         51.0        64.3              28.3                 1.4            18.4           11.0 
#3 Summer             69.2         51.2        65.8              23.7                 2.74           17.6            9.71
#4 Winter             65.7         56.1        64.7              28.5                 0.596          18.7            9.50






#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

most_popular_keymode_2022 <- df %>% filter(released_year==2022) %>% 
  mutate(key_mode = paste(key, "_", mode)) %>% 
  group_by(key_mode) %>% summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(10)


solo_artists <- df %>% filter(artist_count==1) %>% 
  mutate(key_mode = paste(key, "_", mode)) %>% 
  filter(key_mode %in% most_popular_keymode_2022$key_mode) %>% 
  group_by(key_mode) %>% 
  summarise(n=n()) %>% 
  top_n(1, n)

View(solo_artists)

## Odp.Najbardziej popularne pary key-mode to G_Major i ""_Major





#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  mutate(artists = strsplit(artist.s._name, ", ")) %>%
  unnest(cols = artists) %>% 
  select(artist = artists, streams) %>% 
  group_by(artist) %>% 
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  top_n(1, total_streams) %>% 
  pull(artist)



## Odp. Najwięcej oddtworzeń ma The Weekend 





#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debut_in_2022 <-df %>% 
  mutate(artists = strsplit(artist.s._name, ", ")) %>%
  unnest(cols = artists) %>% 
  select(artist = artists,released_year, mode, key) %>% 
  group_by(artist) %>% summarise(year = min(released_year)) %>% 
  filter(year == 2022) %>% 
  pull(artist)


modes <- df %>% 
  mutate(artists = strsplit(artist.s._name, ", ")) %>%
  unnest(cols = artists) %>% 
  select(artist = artists,released_year, mode) %>% 
  filter(artist %in% debut_in_2022) %>% 
  group_by(artist, mode) %>% 
  summarise(n= n()) %>% 
  pivot_wider(names_from = mode, values_from = n, values_fill = 0)


keys <- df %>% 
  mutate(artists = strsplit(artist.s._name, ", ")) %>%
  unnest(cols = artists) %>% 
  select(artist = artists,released_year, key) %>% 
  filter(artist %in% debut_in_2022) %>% 
  mutate(key = ifelse(key=="","_",key)) %>% 
  group_by(artist, key) %>% 
  summarise(n= n()) %>% 
  pivot_wider(names_from = key, values_from = n, values_fill = 0)


## Odp. ramki modes i keys





#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

tmp <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name)

## Odp.tmp






#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?


df12 <- df %>% 
  mutate(artists = strsplit(artist.s._name, ", ")) %>%
  unnest(cols = artists) %>% 
  select(artist = artists, artist_count, streams ) 

solo <- df12 %>% 
  filter(artist_count==1) %>% 
  group_by(artist) %>% 
  summarise(streams_solo = mean(as.numeric(streams),na.rm = TRUE))

with_others <- df12 %>% 
  filter(artist_count>1) %>% 
  group_by(artist) %>% 
  summarise(streams_with_others= mean(as.numeric(streams),na.rm = TRUE))

result12 <- inner_join(solo, with_others, by ="artist") %>% 
  filter(streams_solo>streams_with_others) %>% 
  select(artist)

## Odp.Jest 58 takich arystów, wszyscy w result12




