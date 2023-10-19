library(tidyr)
library(dplyr)
df <- read.csv('spotify-2023.csv')
df$streams <- as.numeric(df$streams)
View(df)
#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & (released_month == 1 | released_month == 2 | released_month == 3)) %>% 
  select(released_year,released_month,streams) %>% 
  filter(!is.na(streams)) %>%
  summarize(mean_views = mean(streams, na.rm = TRUE)) -> x1
  
## Odp. x1 (216150568)

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  select(artist_count, in_spotify_playlists) %>% 
  group_by(artist_count) %>% summarize(playlist_count = sum(in_spotify_playlists)) -> x2
small <- x2 %>% filter(artist_count == c(1,2)) %>% summarise(sum(playlist_count))
big <- x2 %>% filter(artist_count != c(1,2)) %>% summarise(sum(playlist_count))
x2 <- big > small

## Odp. x2 (NIE)


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
library(lubridate)
df %>%
  mutate(
    release_date = make_date(released_year, released_month, released_day),
    day_of_week = weekdays(release_date)
  ) %>% 
  group_by(day_of_week) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1) -> x3

## Odp. x3 (piątek)

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>% 
  filter(released_year == 2021 | released_year == 2022) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = (released_year), values_from = (n)) %>% na.omit %>% 
  rename(
    nowa_nazwa_2022 = `2022`,
    nowa_nazwa_2021 = `2021`
  ) %>% 
  mutate(percent = (nowa_nazwa_2022-nowa_nazwa_2021)*100) %>% 
  arrange(-percent) %>% 
  head(1) -> x4

## Odp. x4 (SZA, wzrost o 1600%)

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  slice_head(n = round(nrow(df) * 0.1)) %>% 
  select(track_name, artist.s._name, released_year, streams, danceability_.) %>% 
  mutate(years_passed = 2023 - released_year + 1, average = streams / years_passed) %>% 
  arrange(-average) %>% 
  head(1) -> x5

## Odp. x5 (Chencho Corleone, Bad Bunny)

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams_per_playlist = (streams / in_spotify_playlists)) %>% 
  arrange(-streams_per_playlist) %>% 
  slice_head(n = round(nrow(df) * 0.2)) %>% 
  summarise(mean_bpm = mean(bpm)) -> x6

df %>% 
  mutate(streams_per_playlist = (streams / in_spotify_playlists)) %>% 
  arrange(-streams_per_playlist) %>% 
  slice_head(n = round(nrow(df) * 0.2)) %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) -> y6

## Odp. srednie tempo = x6 (125.2775), najczesciej wystepujaca skala = y6 (Minor)

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when((released_month == "12" |released_month == "1" | released_month == "2")  ~ 'Winter',
                            (released_month == "3" |released_month == "4" | released_month == "5") ~ 'Spring',
                            (released_month == "6" |released_month == "7" | released_month == "8") ~ 'Summer',
                            (released_month == "9" |released_month == "10" | released_month == "11") ~ 'Autumn',
                            TRUE  ~ "Other")) %>% 
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.), mean_valence = mean(valence_.), mean_energy = mean(energy_.), mean_acoutsticness= mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.), mean_liveness = mean(liveness_.), mean_speechness = mean(speechiness_.)) -> x7

## Odp. x7 (Wiele obserwacji)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(10) -> most_popular_key_mode_values
df %>% 
  filter(artist_count == 1) %>% 
  group_by(key, mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) -> most_popular_key_mode_valuesamong_solo_artists
most_popular_key_mode_values %>% 
  inner_join(most_popular_key_mode_valuesamong_solo_artists, by=c("key","mode")) -> finished_dataset
colnames(finished_dataset)[3] <- "in_2022"
colnames(finished_dataset)[4] <- "used_by_solo_art"
finished_dataset %>% 
  arrange(-used_by_solo_art) %>% 
  head(2) -> x8

## Odp. x8 (G Major oraz " " Major wspólnie 47 użyć)

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(streams)) %>% 
  arrange(- sum_of_streams) %>% 
  head(1) -> x9

## Odp. x9 (The Weeknd)

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) -> all_artists_separately

all_artists_separately %>% 
  group_by(artist.s._name) %>% 
  summarise(debiut = min(released_year)) %>% 
  filter(debiut == 2022) -> debut_in_2022

all_artists_separately %>% 
  inner_join(debut_in_2022, by= "artist.s._name") -> all_artists_separately_with_data

all_artists_separately_with_data %>% 
  group_by(artist.s._name, mode) %>% 
  summarise(number_of_scales = n()) %>% 
  pivot_wider(names_from = mode, values_from = number_of_scales, values_fill = 0) -> modes

all_artists_separately_with_data %>% 
  group_by(artist.s._name, key) %>% 
  summarise(number_of_keys = n()) %>% 
  mutate(key = ifelse(key == "", "None", key)) %>% 
  pivot_wider(names_from = key, values_from = number_of_keys, values_fill = 0) -> keys

modes %>% 
  inner_join(keys, by = "artist.s._name") -> x10

## Odp. x10 (Wiele obserwacji)



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts != 0 ) -> x11

## Odp. x11 (Wiele obserwacji)

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?


df %>%
  filter(artist_count != 1) %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  group_by(artist.s._name) %>% 
  summarise(n = n(), sum_streams = sum(streams)) %>% 
  mutate(streams_per_song_with_others = sum_streams / n) %>% 
  select(artist.s._name, streams_per_song_with_others)-> with_others

df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(n = n(), sum_streams = sum(streams)) %>% 
  mutate(streams_per_song_alone = sum_streams / n) %>% 
  select(artist.s._name, streams_per_song_alone)-> alone
  
with_others %>% inner_join(alone, by = "artist.s._name") %>% 
  filter(streams_per_song_with_others < streams_per_song_alone) %>% 
  arrange(-streams_per_song_alone) -> x12


## Odp. x12 (Wiele obserwacji)

