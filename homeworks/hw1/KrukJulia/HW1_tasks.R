library(tidyr)
library(dplyr)
library(tidyverse)

df <- read.csv('spotify-2023.csv')
str(df)
df$streams = as.numeric(df$streams)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023
#### w pierwszym kwartale?

df %>% 
  filter(released_year == 2023,
         !is.character(streams), released_month %in% c(1:3)) %>% 
  summarise(mean_streams = mean(streams, na.rm = T))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na 
#### większej liczbie playlist spotify niż piosenki stworzone przez 1 lub
#### 2 artystów?

df %>% 
  mutate(artist_count = ifelse(artist_count < 3, "1 or 2", "3 or more")) %>% 
  group_by(artist_count) %>% 
  summarise(n_of_playlist = sum(in_spotify_playlists)) %>% 
  top_n(1, n_of_playlist) %>% 
  select(artist_count)
  

## Odp. Nie, na wiekszej liczbie playlist sa piosenki tworzone przez
## 1 lub 2 artystow


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(date = as.Date(paste(released_year, released_month,
                              released_day, sep = "-"))) %>% 
  mutate(day_of_week = weekdays(date)) %>% 
  group_by(day_of_week) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(day_of_week)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby 
#### wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do
#### utworów w których był jeden wykonawca) Ogranicz się tylko do artystów,
#### którzy opublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1, released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = released_year, values_from = n) %>% 
  filter(!is.na(`2022`) & !is.na(`2021`)) %>% 
  mutate(percentage_increase = ((`2022` - `2021`) / `2021`) * 100) %>% 
  arrange(-percentage_increase) %>% 
  head(1) %>% 
  select(artist.s._name)


## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, 
#### piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


df %>% 
  arrange(desc(danceability_.)) %>% 
  head(0.1 * nrow(df)) %>% 
  mutate(mean_streams_per_year = streams / (2024 - released_year)) %>% 
  top_n(1, mean_streams_per_year) %>% 
  select(artist.s._name)
  
## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode')
#### charakteryzują się piosenki, które są w 20% najczęściej odtwarzanych
#### piosenek w przeliczeniu na liczbę playlist spotify?

df2 <- df %>% 
  mutate(score = streams / in_spotify_playlists) %>% 
  arrange(-score) %>% 
  head(0.2 * nrow(df))

df2 %>% 
  summarise(mean(bpm))


df2 %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(mode)

## Odp. mode Minor, mean bpm 125.2

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki 
#### publikowane w poszczególnych porach roku?

df_seasons <- df %>% 
  mutate(season = case_when(released_month %in% c(1,2)
                            |(released_month == 12 & released_day > 21)
                            |(released_month == 3 & released_day < 21) ~ "Winter",
        released_month %in% c(4, 5) | (released_month == 6 & released_day < 22)
        | (released_month == 3 & released_day > 20) ~ "Spring",
        released_month %in% c(7, 8) | (released_month == 6 & released_day > 21)
        | (released_month == 9 & released_day < 23) ~ "Summer",
        TRUE ~ "Autumn"))

res7 <- df_seasons %>%
  group_by(season) %>%
  summarise_at(vars(danceability_., valence_., energy_., 
                    acousticness_., instrumentalness_.,
                    liveness_., speechiness_.), mean)



## Odp. 
View(res7)


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę 
#### którą najczęściej tworzą artyści solowi.

top_key_mode <- df %>% 
  filter(released_year == 2022, key != "", mode != "") %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>%
  group_by(key_mode) %>% 
  summarise(key_mode_n = n()) %>% 
  top_n(10, key_mode_n)

df %>% 
  filter(artist_count == 1) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  filter(key_mode %in% top_key_mode$key_mode) %>% 
  group_by(key_mode) %>% 
  summarise(key_mode_n = n()) %>% 
  top_n(1, key_mode_n) %>% 
  select(key_mode)
  

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego 
#### piosenek w sumie?

df %>% 
  rowwise() %>%
  mutate(artist.s._name = strsplit(artist.s._name, ",\\s*")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(total_streams = sum(streams), .groups = "drop") %>% 
  pivot_wider(names_from = released_year, values_from = total_streams) %>% 
  rowwise() %>% 
  mutate(t_st = sum(c_across(2:last_col()), na.rm = T)) %>% 
  select(artist.s._name, t_st) %>% 
  arrange(-t_st) %>% 
  head(1) %>% 
  select(artist.s._name)
  
  
## Odp. The Weeknd  

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
#### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach
#### ('mode') i tonacji ('key'). W wyniku jeden wiersz powinien odpowiadać 
#### jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debuting_artists_2022 <- df %>%
  arrange(artist.s._name, released_year) %>%
  group_by(artist.s._name) %>%
  filter(first(released_year) == 2022) %>% 
  distinct(artist.s._name)

key_modes <- df %>% 
  filter(artist.s._name %in% debuting_artists_2022$artist.s._name) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(artist.s._name, key_mode) %>% 
  summarise(key_mode_n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = key_mode, values_from = key_mode_n) %>% 
  mutate_all(~replace_na(.,0))


## Odp.
View(key_modes)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się 
#### częściej na playlistach) na spotify niż na apple, nie zostały odnotowane
#### na zestawieniu spotify, ale w apple już tak?

songs <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, 
         in_apple_charts != 0) %>% 
  select(track_name)

## Odp. 
View(songs)



#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy
#### tworzy solo niż gdy tworzy z innymi artystami?

solo_artists <- df %>%
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>%
  summarize(mean_streams_solo = mean(streams, na.rm = TRUE))

with_others <- df %>%
  filter(artist_count != 1) %>% 
  rowwise() %>%
  mutate(artist.s._name = strsplit(artist.s._name, ",\\s*")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>%
  summarize(mean_streams_with_others = mean(streams, na.rm = TRUE))

res12 <- full_join(solo_artists, with_others, by = "artist.s._name") %>% 
  mutate_all(~replace_na(.,0)) %>% 
  filter(mean_streams_solo > mean_streams_with_others) %>% 
  select(artist.s._name)


## Odp. 
View(res12)
