library(tidyr)
library(dplyr)

df <- read.csv('./repo/homeworks/hw1/spotify-2023.csv')

df <- df %>%
  rename(artists = "artist.s._name",
         danceability = "danceability_.",
         energy = "energy_.",
         accousticness = "acousticness_.",
         valence = "valence_.",
         instrumentalness = "instrumentalness_.",
         speechiness = "speechiness_.",
         liveness = "liveness_.",

  ) %>%
  mutate(streams = as.numeric(streams))

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>%
  filter(released_year == 2023 & released_month < 4) %>%
  summarise(mean_streams = mean(streams))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  mutate(artists = strsplit(artists, ', ')) %>%
  mutate(artists_count = lengths(artists)) %>%
  mutate(artists_count = ifelse(artists_count > 2, 'more than 2', '1 or 2')) %>%
  group_by(artists_count) %>%
  summarize(in_spotify_playlist_total = sum(in_spotify_playlists, na.rm = TRUE))


## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

temp <- df %>%
  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = '-'))) %>%
  mutate(day_of_week = weekdays(released_date)) %>%
  group_by(day_of_week) %>%
  summarize(total_streams = n()) %>%
  arrange(desc(total_streams)) %>%
  head(1)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

temp <- df %>%
  filter(released_year %in% c(2021, 2022)) %>%
  mutate(artists = strsplit(artists, ', ')) %>%
  mutate(artists_count = lengths(artists)) %>%
  filter(artists_count == 1) %>%
  group_by(artists, released_year) %>%
  summarize(total_songs = n()) %>%
  spread(released_year, total_songs) %>%
  rename(songs_2021 = '2021', songs_2022 = '2022') %>%
  mutate(percent_change = (songs_2022 - songs_2021) / songs_2021 * 100) %>%
  arrange(desc(percent_change)) %>%
  head(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

temp <- df %>%
  filter(danceability > quantile(danceability, 0.9)) %>%
  mutate(mean_streams = streams / (2023 - released_year + 1)) %>%
  arrange(desc(mean_streams)) %>%
  select(track_name, artists, mean_streams) %>%
  head(1)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

temp <- df %>%
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>%
  filter(streams_per_playlist >= quantile(streams_per_playlist, 0.8, na.rm = TRUE)) %>%
  summarize(mean_tempo = mean(bpm, na.rm = TRUE),
            most_frequent_mode = names(which.max(table(mode))))

## Odp. 125.277, Minor


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

temp <- df %>%
  mutate(season = case_when(
    released_month %in% c(3, 4, 5) ~ 'spring',
    released_month %in% c(6, 7, 8) ~ 'summer',
    released_month %in% c(9, 10, 11) ~ 'autumn',
    released_month %in% c(12, 1, 2) ~ 'winter')) %>%
  group_by(season) %>%
  summarize(mean_danceability = mean(danceability),
            mean_energy = mean(energy),
            mean_accousticness = mean(accousticness),
            mean_valence = mean(valence),
            mean_instrumentalness = mean(instrumentalness),
            mean_speechiness = mean(speechiness),
            mean_liveness = mean(liveness)
  ) %>%
  arrange(season)

## Odp.
## season mean_danceability mean_energy mean_accousticness mean_valence mean_instrumentalness mean_speechiness mean_liveness
## 1 autumn              65.3        62.2               26.7         46.2                 2.02             10.2           18.0
## 2 spring              68.0        64.3               28.3         51.0                 1.4              11.0           18.4
## 3 summer              69.2        65.8               23.7         51.2                 2.74              9.71          17.6
## 4 winter              65.7        64.7               28.5         56.1                 0.596             9.50          18.7


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

temp <- df %>%
  filter(released_year == 2022) %>%
  mutate(artists = strsplit(artists, ', ')) %>%
  mutate(artists_count = lengths(artists)) %>%
  filter(artists_count == 1) %>%
  drop_na(key, mode) %>%
  group_by(key, mode) %>%
  summarize(song_count = n()) %>%
  arrange(desc(song_count)) %>%
  head(10)

## Odp. G major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

temp <- df %>%
  separate_rows(artists, sep = ', ') %>%
  group_by(artists) %>%
  summarize(total_streams = sum(streams, na.rm = T)) %>%
  arrange(desc(total_streams))

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debutants <- df %>%
  select(artists, streams, released_year) %>%
  separate_rows(artists, sep = ', ') %>%
  mutate(artists = as.character(artists)) %>%
  group_by(artists, released_year) %>%
  summarize(total_songs = n()) %>%
  spread(released_year, total_songs) %>%
  replace(is.na(.), 0) %>%
  mutate(before_2022 = sum(c_across('1930':'2021'))) %>%
  select(artists, before_2022, '2022') %>%
  filter(before_2022 == 0, '2022' > 0) %>%
  filter(artists != '') %>%
  select(artists)

df %>%
  separate_rows(artists, sep = ', ') %>%
  mutate(artists = as.character(artists)) %>%
  filter(artists %in% debutants$artists) %>%
  group_by(artists, mode, key) %>%
  summarize(total_songs = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = total_songs) %>%
  replace(is.na(.), 0)

## Odp. ↑


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

temp <- df %>%
  select(track_name, in_spotify_playlists, in_apple_playlists, in_apple_charts, in_spotify_charts) %>%
  filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_apple_charts > 0 & in_spotify_charts == 0) %>%
  select(track_name)

## Odp. Dużo ich: 337


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo_count <- df %>%
  filter(artist_count == 1) %>%
  group_by(artists) %>%
  summarize(mean_streams = mean(streams, na.rm = T)) %>%
  rename(solo_streams = mean_streams)

combo_count <- df %>%
  filter(artist_count > 1) %>%
  separate_rows(artists, sep = ', ') %>%
  group_by(artists) %>%
  summarize(mean_streams = mean(streams, na.rm = T)) %>%
  rename(combo_streams = mean_streams)

comparison <- solo_count %>%
  inner_join(combo_count, by = 'artists') %>%
  mutate(streams_diff = solo_streams - combo_streams) %>%
  na.omit() %>%
  filter(streams_diff > 0) %>%
  select(artists)

## Odp.
# 1	Agust D
# 2	Anitta
# 3	Ariana Grande
# 4	Beyoncï¿
# 5	BoyWithUke
# 6	Bruno Mars
# 7	Carin Leon
# 8	Central Cee
# 9	Chino Pacas
# 10	Chris Brown
# 11	Ckay
# 12	Coi Leray
# 13	Cris Mj
# 14	Dave
# 15	Doja Cat
# 16	Ed Sheeran
# 17	Eden Muï¿½ï
# 18	Eminem
# 19	Feid
# 20	Grupo Frontera
# 21	Imagine Dragons
# 22	J. Cole
# 23	JVKE
# 24	Jimin
# 25	John Legend
# 26	Juice WRLD
# 27	Justin Bieber
# 28	Kanye West
# 29	Kendrick Lamar
# 30	Kodak Black
# 31	LE SSERAFIM
# 32	Lana Del Rey
# 33	Lil Baby
# 34	Lil Uzi Vert
# 35	Lisa
# 36	Luciano
# 37	Manuel Turizo
# 38	Maroon 5
# 39	Morgan Wallen
# 40	Muni Long
# 41	NLE Choppa
# 42	Nicki Minaj
# 43	Oliver Tree
# 44	Paulo Londra
# 45	Playboi Carti
# 46	Polo G
# 47	RM
# 48	ROSALï¿½
# 49	Rauw Alejandro
# 50	Ryan Castro
# 51	Sam Smith
# 52	Sebastian Yatra
# 53	Sleepy hallow
# 54	Stephen Sanchez
# 55	Taylor Swift
# 56	Willow
# 57	XXXTENTACION
# 58	j-hope