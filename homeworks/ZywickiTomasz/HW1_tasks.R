library(tidyr)
library(dplyr)
library(hydroTSM)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>%
  summarise(mean_played = mean(strtoi(streams))) %>%
  select(mean_played)

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>%
  filter(artist_count %in% c(1, 2)) %>%
  summarise(one_or_two_playlist = sum(in_spotify_playlists)) %>%
  select(one_or_two_playlist) -> one_or_two

df %>%
  filter(artist_count >= 3) %>%
  summarise(more_than_two_playlist = sum(in_spotify_playlists)) %>%
  select(more_than_two_playlist) -> more_than_two

one_or_two < more_than_two

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>%
  mutate(weekday = weekdays(as.Date(
    paste(released_year, released_month, released_day, sep = "-")))) %>%
  group_by(weekday) %>%
  summarise(weekday_releases = n()) %>%
  select(weekday, weekday_releases) %>%
  arrange(-weekday_releases)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.
df %>%
  filter(artist_count == 1, released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name) %>%
  filter(any(released_year == 2021) & any(released_year == 2022)) %>%
  summarise(growth_percentage = 
           ((sum(released_year == 2022) - sum(released_year == 2021)) /
              (sum(released_year == 2021))) * 100) %>%
  arrange(-growth_percentage) %>%
  head(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  arrange(-danceability_.) %>%
  head(round(0.1 * nrow(df))) %>%
  mutate(years_since_release = 2024 - released_year) %>%
  mutate(average_streams_per_year = strtoi(streams) / years_since_release) %>%
  arrange(-average_streams_per_year) %>%
  select(artist.s._name) %>%
  head(1)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>%
  arrange(desc(strtoi(streams) / in_spotify_playlists)) %>%
  head(round(0.2 * nrow(df))) %>%
  summarise(average_tempo = mean(bpm),
            most_common_mode = names(which.max(table(df$mode))))
  
              

## Odp.Średnie tempo: 125.2775, Najczęstsza skala: Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>%
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = '-')),
         season = time2season(date, out.fmt = "seasons")) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_.),
            mean_energy = mean(energy_.),
            mean_valence = mean(valence_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.),
            mean_speechiness = mean(speechiness_.)) %>%
  arrange(-mean_danceability, -mean_energy, -mean_valence, -mean_instrumentalness,
          -mean_liveness, -mean_speechiness)
  

## Odp. Latem i wiosną zauważalnie większa taneczność a energia zauważalnie najmmniejsza jesienią itd

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>%
  filter(released_year == 2022, key != "") %>%
  group_by(key, mode) %>%
  summarise(solo_artists = sum(artist_count == 1)) %>%
  arrange(-solo_artists) %>%
  head(1)

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarise(all_streams = sum(strtoi(streams))) %>%
  arrange(-all_streams) %>%
  head(1)

## Odp. Taylor Swift

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df %>%
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>%
  group_by(artist.s._name) %>%
  mutate(min_year = min(released_year)) %>%
  filter(min_year == 2022, key != "") %>%
  ungroup %>%
  group_by(artist.s._name, key, mode) %>%
  summarise(songs_count = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = songs_count, values_fill = 0)

## Odp. Tak jak w tabeleczce :)



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  filter(in_spotify_playlists > in_apple_playlists,
         in_apple_charts > 0, in_spotify_charts == 0, ) %>%
  select(track_name)

## Odp. Też tak jak w tabeleczce :)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>%
  separate_rows(artist.s._name, sep = ', ') %>%
  filter(artist.s._name != "Edison Lighthouse") %>% # to usunięte bo wartość streams popsuta
  mutate(solo_or_not = ifelse(artist_count == 1, "solo", "colab")) %>%
  group_by(artist.s._name, solo_or_not) %>%
  summarise(mean_streams = mean(strtoi(streams), na.rm = TRUE)) %>%
  pivot_wider(names_from = solo_or_not, values_from = mean_streams, values_fill = 0) %>%
  filter(colab != 0, solo != 0, solo > colab)

  
  
## Odp. Jest ich 57