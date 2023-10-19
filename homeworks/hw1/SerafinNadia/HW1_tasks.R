library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv', stringsAsFactors = FALSE)


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df$streams <- as.numeric(df$streams)
df1 <- df %>% 
  filter(released_year == 2023) %>% 
  filter(released_month %in% c(1, 2, 3)) %>% 
  summarise(average_plays = mean(streams))

## Odp. 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df2 <- df %>%
  mutate(artist.s._number = ifelse(artist_count > 2, 'more than 2', '1 or 2')) %>% 
  group_by(artist.s._number) %>% 
  summarise(playlist.s._number = sum(in_spotify_playlists)) %>% 
  top_n(1, playlist.s._number) 


## Odp. nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df3 <- df %>% 
  # dodanie kolumny z dniem tygodnia
  mutate(date = as.Date(paste(df$released_year, df$released_month, df$released_day, sep = '-'))) %>% 
  mutate(week_day = weekdays(date)) %>% 
  group_by(week_day) %>% 
  summarise(num_of_releases = n()) %>% 
  top_n(1, num_of_releases) 

## Odp. Piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df4 <- df %>%
  filter(artist_count == 1) %>% 
  filter(released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(num_of_releases = n()) %>% 
  pivot_wider(names_from = released_year, values_from = num_of_releases) %>% 
  filter(!is.na(`2021`) & !is.na(`2022`)) %>% 
  mutate(perc_growth = `2022` / `2021` * 100) %>% 
  arrange(-(perc_growth)) %>% 
  head(1)

## Odp. SZA

  

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df5 <- df %>% 
  arrange(desc(danceability_.)) %>% 
  head(nrow(df) / 10) %>% 
  mutate(average_plays = streams / (2024 - released_year)) %>% 
  group_by(artist.s._name) %>% 
  summarise(max_average_plays = max(average_plays)) %>% 
  top_n(1, max_average_plays)

## Odp. Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df6 <- df %>% 
  top_n(nrow(df) / 5, in_spotify_playlists)

# srednie tempo
df6_a <- df6 %>% 
  group_by(bpm) %>% 
  summarise(num_of_bpm = n()) %>% 
  top_n(1, num_of_bpm) %>% 
  summarise(average_bpm = mean(bpm))
  
# najczesciej wystepujaca skala
df6_b <- df6 %>% 
  group_by(mode) %>% 
  summarise(num_of_modes = n()) %>% 
  top_n(1, num_of_modes)

## Odp.   tempo - 114.2857, skala - Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df7 <- df %>% 
  mutate(season = case_when(   # pory roku
    (released_month == 12 & released_day >= 22) | (released_month == 1) | (released_month == 2) | (released_month == 3 & released_day <= 20) ~ 'Winter',
    (released_month == 3 & released_day >= 21) | (released_month == 4) | (released_month == 5) | (released_month == 6 & released_day <= 21) ~ 'Spring',
    (released_month == 6 & released_day >= 22) | (released_month == 7) | (released_month == 8) | (released_month == 9 & released_day <= 22) ~ 'Summer',
    (released_month == 9 & released_day >= 23) | (released_month == 10) | (released_month == 11) | (released_month == 12 & released_day <= 21) ~ 'Autumn'
  ))


# srednia tanecznosc piosenek dla kazdej z por roku
df7_dancability <- df7 %>% 
  group_by(season) %>% 
  summarise(average_dancability = mean(danceability_.)) %>% 
  arrange(season)

# srednia energetyka piosenek dla kazdej z por roku
df7_energy <- df7 %>% 
  group_by(season) %>% 
  summarise(average_energy = mean(energy_.)) %>% 
  arrange(season)

# srednia wartosc pozytywna piosenek dla kazdej z por roku
df7_valence <- df7 %>% 
  group_by(season) %>% 
  summarise(average_valence= mean(valence_.)) %>% 
  arrange(-average_valence)

# srednia akustyka piosenek dla kazdej z por roku
df7_acousticness <- df7 %>% 
  group_by(season) %>% 
  summarise(average_acousticness= mean(acousticness_.)) %>% 
  arrange(-average_acousticness)

# srednie uzycie instrumentow w piosenkach dla kazdej pory roku
df7_instrumentalness <- df7 %>% 
  group_by(season) %>% 
  summarise(average_instrumentalness= mean(instrumentalness_.)) %>% 
  arrange(-average_instrumentalness)

# srednia wystapien live dla piosenek dla kazdej pory roku
df7_liveness <- df7 %>% 
  group_by(season) %>% 
  summarise(average_liveness= mean(liveness_.)) %>% 
  arrange(-average_liveness)

# srednie uzycie slow w piosenkach dla kazdej pory roku
df7_speechiness <- df7 %>% 
  group_by(season) %>% 
  summarise(average_speechiness= mean(speechiness_.)) %>% 
  arrange(-average_speechiness)

df7_f <- df7_dancability %>% 
  inner_join(df7_energy, by = "season") %>% 
  inner_join(df7_valence, by = "season") %>% 
  inner_join(df7_acousticness, by = "season") %>% 
  inner_join(df7_instrumentalness, by = "season") %>% 
  inner_join(df7_liveness, by = "season") %>% 
  inner_join(df7_speechiness, by = "season")

## Odp.
View(df7_f)


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
most_pop_key_mode <- df %>% 
  filter(released_year == 2022) %>% 
  filter(key != "" & mode != "") %>%
  mutate(key_mode = paste(key, mode, sep = " ")) %>%
  group_by(key_mode) %>% 
  summarise(key_mode_n = n()) %>% 
  top_n(10, key_mode_n)

df8 <- df %>% 
  filter(artist_count == 1) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  filter(key_mode %in% most_pop_key_mode$key_mode) %>% 
  group_by(key_mode) %>% 
  summarise(key_mode_n = n()) %>% 
  top_n(1, key_mode_n)

## Odp. G Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df9 <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>% 
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  summarise(num_of_plays = sum(streams)) %>% 
  top_n(1, num_of_plays)

## Odp. The Weeknd


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df10 <- df %>% 
  filter(released_year == 2022) %>% 
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>% 
  unnest(artist.s._name) %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(num_of_songs = n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = num_of_songs, values_fill = 0)

## Odp.
View(df10)
  

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df11 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name)

## Odp. 
View(df11)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
solo <- df %>%
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>%
  summarize(avg_solo_streams = mean(streams))

not_solo <- df %>% 
  filter(artist_count != 1) %>% 
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>% 
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>%
  summarize(avg_not_solo_streams = mean(streams))

df12 <- solo %>% 
  full_join(not_solo, by = "artist.s._name") %>% 
  mutate_all(~replace_na(.,0))
filter(mean_streams_solo > mean_streams_with_others) 

## Odp.  
View(df12)





