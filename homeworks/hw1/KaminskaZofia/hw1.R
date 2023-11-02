library(tidyr)
library(dplyr)

df <- read.csv("spotify-2023.csv")

colnames(df)
head(df)
View(df)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month %in% 1:3) %>% 
  summarise(mean = mean(as.numeric(streams)))
  
  ## Odp.   216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  filter(artist_count <= 2) %>% 
  summarise(suma = sum(in_spotify_playlists, na.rm = TRUE)) -> jeden_dwa
df %>% 
  filter(artist_count  > 2) %>% 
  summarise(suma = sum(in_spotify_playlists, na.rm = TRUE)) -> reszta
jeden_dwa < reszta

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, sep = "/"), format = "%Y/%m/%d")) %>% 
  mutate(weekday = weekdays(release_date)) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  top_n(1, n)

## Odp. piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2022 i w 2021.
df %>% 
  filter(artist_count == 1, released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = released_year, values_from = n, values_fill = 0) %>%
  filter(`2021` > 0 & `2022` > 0) %>% 
  mutate(percent_increase = (`2022` - `2021`) / `2021` * 100) %>%
  arrange(desc(percent_increase)) %>% 
  head(1) %>% 
  select(artist.s._name, percent_increase) 

## Odp. SZA - przyrost 1600%


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  arrange(-danceability_.) %>% 
  head(nrow(df)*0.1) %>% 
  mutate(years = 2024 - released_year) %>%
  mutate(streams_per_year = as.numeric(streams)/years) %>% 
  top_n(1, streams_per_year) %>% 
  select(artist.s._name, track_name)

## Odp. Chencho Corleone, Bad Bunny i  ich piosenka "Me Porto Bonito"


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
  mutate(streams_per_playlist = strtoi(streams) / in_spotify_playlists) %>%
  arrange(-streams_per_playlist) %>% 
  head(nrow(df) * 0.2) -> df_top_20
df_top_20 %>% 
  summarise(mean_bpm = mean(bpm, na.rm = TRUE))
df_top_20 %>% 
  group_by(mode) %>%
  summarise(n = n()) %>% 
  top_n(1, n)

## Odp. średnie tempo: 125.2 bpm, najczestsza skala: minor


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when(
    released_month %in% c('4','5') | (released_month=='3' & released_day>=21) | 
      (released_month=='6' & released_day<22) ~ 'spring',
    released_month %in% c('7','8') | (released_month=='6' & released_day>=22) |
      (released_month=='9' & released_day<'23')~ 'summer',
    released_month %in% c('10','11') | (released_month=='9' & released_day>=23) | 
      (released_month=='12' & released_day<=21) ~ 'autumn',
    TRUE ~ 'winter')) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_.), 
            mean_valence = mean(valence_.), 
            mean_energy=mean(energy_.), 
            mean_acousticness=mean(acousticness_.), 
            mean_intrumentalness=mean(instrumentalness_.), 
            mean_liveness=mean(liveness_.), 
            mean_speechiness=mean(speechiness_.))
  
# Odp. 
#        season mean_danceability mean_valence mean_energy mean_acousticness mean_intrumentalness mean_liveness mean_speechiness

#  1 autumn              63.9         47.0        61.2              29.7                1.96           17.6             9.84
#  2 spring              68.6         49.5        64.2              27.7                1.67           18.3            11.0 
#  3 summer              68.5         51.7        66.0              24.4                2.26           17.8             9.76
#  4 winter              66.9         57.0        65.8              25.8                0.739          18.8             9.72



#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022, key != "") %>%
  mutate(key_mode = paste0(key, " ", mode)) %>%
  group_by(key_mode) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>% 
  arrange(-n)->top10
df %>%
  mutate(key_mode = paste0(key, " ", mode)) %>%
  filter(key_mode %in% top10$key_mode, artist_count == 1) %>%
  group_by(key_mode) %>%
  summarise(solo_number = n()) %>%
  arrange(-solo_number) %>% 
  head(1) %>% select(key_mode)
  
## Odp. G - major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
library(stringr)

df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  select(streams, artist) %>% 
  unnest(artist) %>% 
  group_by(artist) %>% 
  summarise(sum_streams = sum(as.numeric(streams), na.rm=TRUE)) %>%
  arrange(-sum_streams) %>%
  head(1) %>% 
  select(artist)
  
## Odp.The Weeknd


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debut_artists <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>% 
  group_by(artist) %>% 
  summarise(debut = min(released_year)) %>%
  filter(debut==2022) %>% 
  pull(artist)

atrists_key_mode_debut <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>% 
  filter(key != "", mode != "", artist %in% debut_artists, artist != "") %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(artist, key_mode) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = key_mode, values_from = n, values_fill = 0)

## Odp.: w zmiennej atrists_mode_key_debut
View(atrists_key_mode_debut)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
popularne_apple <- df %>% 
  filter(in_spotify_playlists>in_apple_playlists) %>% 
  filter(in_spotify_charts==0 & in_apple_charts!=0) %>% 
  select(track_name)

## Odp.: łacznie jest 337 takich piosenek - w zmiennej popularne_apple
View(popularne_apple)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo <- df %>% 
  filter(artist_count==1) %>%
  group_by(artist.s._name) %>% 
  summarise(mean_solo_streams = mean(as.numeric(streams))) 
collab <- df %>% 
  filter(artist_count>1) %>%
  mutate(artist.s._name=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist.s._name) %>%
  group_by(artist.s._name) %>% 
  summarise(mean_group_streams = mean(as.numeric(streams))) 
better_solo <- full_join(solo, collab, by='artist.s._name') %>% 
  filter(mean_solo_streams>mean_group_streams) %>% 
  select(artist.s._name)

## Odp.: artysci wypisani w zmiennej better_solo
View(better_solo)

