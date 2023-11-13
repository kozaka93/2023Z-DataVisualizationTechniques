library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')
View(df)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>% 
  summarise(mean_streams = mean(strtoi(streams)))

## Odp. 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
more_than_2_artists <- df %>% 
  filter(artist_count > 2) %>% 
  summarise(sum(in_spotify_playlists))

less_or_2_artists <- df %>% 
  filter(artist_count %in% c(1, 2)) %>% 
  summarise(sum(in_spotify_playlists))

more_than_2_artists > less_or_2_artists

## Odp. nie



#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(weekday = weekdays(as.Date(paste(released_year, released_month, released_day, sep = '-')))) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(1)

## Odp. piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby 
#### wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do 
#### utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1) %>% 
  filter(released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = released_year, values_from = n) %>% 
  filter(!is.na(`2021`) & !is.na(`2022`)) %>% 
  mutate(percentage_increase = (`2022` - `2021`) / `2021` * 100) %>% 
  arrange(desc(percentage_increase)) %>% 
  head(1)


## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, 
#### piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  arrange(desc(danceability_.)) %>% 
  head(round(nrow(df) * 0.10)) %>% 
  mutate(year_streams = (strtoi(streams)/ (2024 - released_year))) %>% 
  arrange(desc(year_streams)) %>% 
  select(artist.s._name) %>% 
  head(1)


## Odp. Chencho Corleone, Bad Bunny



#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') 
#### charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę 
#### playlist spotify?

df_bpm_and_mode <- df %>% 
  mutate(streams_by_spotify_playlists = strtoi(streams) / in_spotify_playlists) %>% 
  arrange(desc(streams_by_spotify_playlists)) %>% 
  head(round(nrow(df) * 0.20)) %>%
  select(bpm, mode)

average_bpm <- mean(df_bpm_and_mode$bpm)

most_frequent_mode <- df_bpm_and_mode %>% 
  group_by(mode) %>% 
  summarize(mode_count = n()) %>% 
  arrange(-mode_count) %>% 
  head(1)

average_bpm
most_frequent_mode

## Odp. średnie tempo: 125.2775, najczęstsza skala: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki 
#### publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when(
    
    ((released_month == 3 & released_day >= 21) | 
       (released_month %in% c(4, 5)) | 
       (released_month == 6 & released_day < 22)) ~ "spring",
    
    ((released_month == 6 & released_day >= 22) | 
       (released_month %in% c(7, 9)) | 
       (released_month == 9 & released_day < 23)) ~ "summer",
    
    ((released_month == 9 & released_day >= 23) | 
       (released_month %in% c(10, 11)) | 
       (released_month == 12 & released_day < 22)) ~ "autumn",
    
    ((released_month == 12 & released_day >= 22) | 
       (released_month %in% c(1, 2)) | 
       (released_month == 3 & released_day < 21)) ~ "winter"
    
  )) %>% 
  filter(!is.na(season)) %>% 
  group_by(season) %>% 
  summarise(danceability = mean(danceability_.),
            valence = mean(valence_.),
            energy = mean(energy_.),
            acousticness = mean(acousticness_.),
            instrumentalness = mean(instrumentalness_.),
            liveness = mean(liveness_.),
            speechiness = mean(speechiness_.),
            danceability = mean(danceability_.)) %>% 
  View()



## Odp. ramka danych do wywołania



#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą 
#### najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022) %>%
  count(key, mode, artist_count) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  filter(artist_count == 1) %>% 
  head(1)

## Odp. G-Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  mutate(artist.s._name = trimws(artist.s._name)) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(strtoi(streams))) %>% 
  arrange(desc(total_streams)) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp. Bad Bunny

  

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach 
#### ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o 
#### tych liczbach umieść w kolejnych kolumnach. 

df_with_separated_artists <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  mutate(artist.s._name = trimws(artist.s._name))

debut_2022_artists <- df_with_separated_artists %>% 
  group_by(artist.s._name) %>% 
  summarise(debut_year = min(released_year)) %>%
  filter(debut_year == 2022 & artist.s._name != "") %>% 
  select(artist.s._name)

artists_keys <- df_with_separated_artists %>% 
  filter(artist.s._name %in% debut_2022_artists$artist.s._name) %>% 
  group_by(artist.s._name, key) %>% 
  summarise(key_number = n()) %>% 
  filter(key != "") %>% 
  pivot_wider(names_from = key, values_from = key_number, values_fill = 0)

artists_modes <- df_with_separated_artists %>% 
  filter(artist.s._name %in% debut_2022_artists$artist.s._name) %>% 
  group_by(artist.s._name, mode) %>% 
  summarise(mode_number = n()) %>% 
  filter(mode != "") %>% 
  pivot_wider(names_from = mode, values_from = mode_number, values_fill = 0)
  

inner_join(artists_keys, artists_modes, by = "artist.s._name")
  


## Odp. ramka danych do wywołania



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się 
#### częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>% 
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, 
         in_apple_charts != 0) %>%
  select(track_name)

## Odp. 337 piosenek (ramka danych do wywołania)



#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy 
#### solo niż gdy tworzy z innymi artystami?

streams_per_artist_count <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  mutate(artist.s._name = trimws(artist.s._name)) %>% 
  group_by(artist.s._name, artist_count)

solo_songs <- streams_per_artist_count %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>%
  summarise(average_streams_solo = mean(strtoi(streams))) %>% 
  select(artist.s._name, average_streams_solo)

group_songs <- streams_per_artist_count %>% 
  filter(artist_count > 1) %>% 
  group_by(artist.s._name) %>%
  summarise(average_streams_group = mean(strtoi(streams))) %>% 
  select(artist.s._name, average_streams_group)

solo_better_artists <- inner_join(solo_songs, group_songs, by = "artist.s._name") %>% 
  mutate(diff_streams = average_streams_solo - average_streams_group) %>% 
  filter(diff_streams > 0) %>% 
  select(artist.s._name) %>% 
  filter(artist.s._name != "")

print(solo_better_artists)


## Odp. 52 artystów (ramka danych do wywołania)



