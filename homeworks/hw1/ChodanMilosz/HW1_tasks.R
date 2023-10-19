library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

df <- df %>% 
  slice(-575) %>% 
  mutate(streams = as.numeric(streams))
View(df)
#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023) %>% 
  filter(released_month < 4) %>% 
  summarise(mean_s = mean(streams, na.rm = TRUE))

## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  mutate(artists_2 = artist_count <= 2) %>% 
  group_by(artists_2) %>% 
  summarise(sum_playlists = sum(in_spotify_playlists, na.rm = TRUE))


## Odp.Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(dates = as.Date(paste(released_year, released_month, released_day, sep = "/"))) %>% 
  mutate(weekday = weekdays(dates)) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  select(weekday) %>% 
  head(1)

## Odp.Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>% 
  filter(artist_count == 1) %>%
  filter(released_year == 2021 | released_year == 2022) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(sum_streams = sum(streams, na.rm = TRUE)) %>% 
  pivot_wider(names_from = released_year, values_from = sum_streams) %>%
  filter(!is.na(`2022`) & !is.na(`2021`)) %>%
  mutate(percentage_growth = (`2022` / `2021`) * 100) %>%
  arrange(-percentage_growth) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  arrange(-danceability_.) %>%
  head(dim(df)[1] / 10) %>%
  mutate(avg = streams / (2023 + 1 - released_year)) %>%
  arrange(-avg) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df1 <- df %>% 
  mutate(streams_per_playlist = as.numeric(streams) / in_spotify_playlists) %>%
  arrange(-streams_per_playlist) %>% 
  head(dim(df)[1] / 5)
  
df1 %>% 
  summarise(mean_bpm = mean(bpm))

df1 %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>%
  select(mode) %>% 
  head(1)

## Odp.125.2,Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
get_season <- function(month, day) {
  if (month == 1 | month == 2 | (month == 3 & day < 21) | (month == 12 & day >= 22)) {
    return("Winter") 
  }
  else if (month == 4 | month == 5 | (month == 3 & day >= 21) | (month == 6 & day < 22)) {
    return("Spring") 
  }
  else if (month == 7 | month == 8 | (month == 6 & day >= 22) | (month == 9 & day < 23)) {
    return("Summer") 
  }
  else if (month == 10 | month == 11 | (month == 9 & day >= 23) | (month == 12 & day < 22)) {
    return("Autumn") 
  }
}
df1 <- df %>% 
  mutate(seasons = mapply(get_season, released_month, released_day)) %>% 
  select(seasons, ends_with("_.")) %>%
  group_by(seasons) %>%
  summarise(across(ends_with("_."), mean))
## Odp.
df1

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(sum_s = sum(streams), solo = sum(artist_count == 1)) %>% 
  arrange(-sum_s) %>% 
  top_n(10) %>% 
  arrange(-solo) %>% 
  select(key, mode) %>% 
  head(1)

## Odp.G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(sum = sum(streams)) %>% 
  arrange(-sum) %>% 
  select(artist.s._name) %>% 
  head(1)

## Odp.The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df1 <- df %>%
  select(artist.s._name, released_year) %>%
  filter(released_year <= 2021) %>%
  distinct(artist.s._name)

df2 <- df %>%
  select(artist.s._name, released_year, mode, key) %>%
  filter(released_year == 2022, !(artist.s._name %in% df1$artist.s._name)) %>%
  group_by(artist.s._name, key, mode) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = c("key", "mode"), values_from = "n", values_fill = 0)

## Odp.
df2


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df1 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_apple_charts != 0 & in_spotify_charts == 0) %>%
  select(track_name)
## Odp.
df1

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df1 <- df %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  group_by(artist.s._name) %>%
  summarize(solo = mean(streams[artist_count == 1], na.rm = TRUE), feat = mean(streams[artist_count > 1], na.rm = TRUE)) %>%
  filter(solo > feat) %>% 
  select(artist.s._name)
## Odp.
df1


