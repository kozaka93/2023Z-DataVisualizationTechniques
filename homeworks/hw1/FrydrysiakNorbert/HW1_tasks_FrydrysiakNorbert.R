library(tidyr)
library(dplyr)
#setwd("/Users/fantasy2fry/Documents/informatyczne/iadstudia/twd/lab2")
df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year=='2023' & released_month %in% c('1','2','3')) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE))

## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  group_by(artist_count>2) %>% 
  summarise(sum_of_being_playlisted = sum(in_spotify_playlists, na.rm = TRUE), 
            mean_of_being_playlisted = mean(in_spotify_playlists, na.rm = TRUE)) 

## Odp. Nie, piosenki stworzone przez 1 lub 2 artystów są zawarte na większej liczbie playlist spotify

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(released_day_of_week=weekdays(as.Date(
    paste(released_year, released_month, released_day, sep = "-")))) %>% 
  group_by(released_day_of_week) %>%
  summarise(releases = n()) %>%
  arrange(desc(releases)) %>%
  top_n(1, releases) %>% 
  pull(released_day_of_week)

## Odp. Najpopularniejszy dzień tygodnia w wypuszczaniu piosenek to piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>% 
  filter(released_year %in% c('2021','2022') & artist_count==1) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(releases = n()) %>%
  pivot_wider(names_from = released_year, values_from = releases) %>%
  na.omit() %>%
  rename("y2021" = "2021", "y2022" = "2022") %>%
  mutate(percent_increase = (y2022/y2021)-1 ) %>%
  arrange(desc(percent_increase)) %>%
  head(1) %>% 
  pull(artist.s._name)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  filter(danceability_. > quantile(danceability_., 0.9)) %>%
  mutate(years_from_release = 2024 - released_year) %>%
  mutate(avg_streams_per_year = as.numeric(streams)/years_from_release) %>%
  arrange(desc(avg_streams_per_year)) %>%
  head(1) %>% 
  pull(artist.s._name)

## Odp."Chencho Corleone, Bad Bunny"

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>% 
  select('in_spotify_playlists', 'mode', 'bpm') %>%
  filter(in_spotify_playlists > quantile(in_spotify_playlists, 0.8)) %>%
  summarise(mean_bpm = mean(bpm, na.rm = TRUE), 
            most_frequent_mode = names(which.max(table(mode))))

## Odp. mean_bpm = 122.5, most_frequent_mode = "Major"

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(season=case_when(
    released_month %in% c('1','2') | (released_month=='12' & released_day>=22) | 
      (released_month=='3' & released_day<=20) ~ 'winter',
    released_month %in% c('4','5') | (released_month=='3' & released_day>20) | 
     (released_month=='6' & released_day<=20) ~ 'spring',
    released_month %in% c('7','8') | (released_month=='6' & released_day>20) |
      (released_month=='9' & released_day>='22')~ 'summer',
    TRUE ~ 'autumn')) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_., na.rm = TRUE), 
            mean_energy = mean(energy_., na.rm = TRUE), 
            mean_speechiness = mean(speechiness_., na.rm = TRUE), 
            mean_acousticness = mean(acousticness_., na.rm = TRUE), 
            mean_instrumentalness = mean(instrumentalness_., na.rm = TRUE), 
            mean_liveness = mean(liveness_., na.rm = TRUE), 
            mean_valence = mean(valence_., na.rm = TRUE), 
            mean_bpm = mean(bpm, na.rm = TRUE)) %>% View()

## Odp. Najbardziej taneczne są piosenki publikowane w lecie i na wiosnę,
# Najmniej energetyczne są piosenki publikowane jesienią, najmniej słów mówionych
# mają piosenki publikowane w zimie, najmniej istrumentalne są piosenki zimowe itd.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% mutate(km=paste(key,mode,sep = '-')) %>% filter(km %in%
(df %>% 
  select('key', 'mode', 'artist_count') %>% #filter(artist_count==1) %>%
  group_by(key, mode) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(key_mode = paste(key, mode, sep = '-')) %>%
  head(10) %>% pull(key_mode)), artist_count==1) %>% group_by(km) %>% 
  summarise(solo_artists = n()) %>%
  top_n(1, solo_artists) %>% pull(km)
  
## Odp. "-Major"  "G-Major"

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% filter(artist_count==1) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE)) %>%
  arrange(desc(total_streams)) %>% head(1) %>% pull(artist.s._name)

## Odp."The Weeknd"

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df %>% separate_rows(artist.s._name, sep = ', ') %>% 
  filter(artist.s._name %in% 
                (df %>% separate_rows(artist.s._name, sep = ', ') %>% 
                   group_by(artist.s._name) %>%
                   summarise(debiut = min(released_year)) %>%
                   filter(debiut==2022) %>% pull(artist.s._name))) %>%
  group_by(artist.s._name, key, mode) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = count, values_fill = 0) %>% 
  View()
## Odp.

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>% filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_spotify_charts == 0) %>%
  filter(in_apple_charts > 0) %>%
  select('track_name', 'artist.s._name', 'in_spotify_playlists', 'in_apple_playlists',
         'in_spotify_charts', 'in_apple_charts') %>%
  arrange(desc(in_spotify_playlists)) %>% select('track_name','artist.s._name') %>% 
  View()
## Odp.

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df2 <- df %>% filter(artist_count>1) %>%
  separate_rows(artist.s._name, sep = ', ') %>% 
  group_by(artist.s._name) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>%
  mutate(collab = mean_streams) %>%
  select('artist.s._name', 'collab')

df %>% filter(artist_count==1) %>%
  group_by(artist.s._name) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>%
  mutate(solo = mean_streams) %>%
  select('artist.s._name', 'solo') %>%
  left_join(df2, by = 'artist.s._name' ) %>%
  mutate(diff = solo - collab) %>%
  arrange(desc(diff)) %>% View()
## Odp. John Legend

