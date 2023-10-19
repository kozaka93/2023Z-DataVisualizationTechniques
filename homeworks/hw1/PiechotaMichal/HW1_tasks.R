library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')
df <- df %>% 
  mutate(streams = as.numeric(streams)) %>% 
  filter(!is.na(streams))


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month <= 3) %>% 
  summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  filter(artist_count > 2) %>% 
  summarise(sum_of_appearance = sum(in_spotify_playlists))

df %>% 
  filter(artist_count <= 2) %>% 
  summarise(sum_of_appearance = sum(in_spotify_playlists))

## Odp. Nie, piosenki stworzone przez wiecej niz 2 artystow pojawiaja sie tylko na 428126 playlistach, podczas
## gdy te stworzone przez co najwyzej dwoch pojawiaja sie na 4527593 playlistach


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(date = as.Date(
    paste(released_year, released_month, released_day),
    format = "%Y %m %d"),
    weekday = weekdays(date)
    ) %>% 
  group_by(weekday) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

## Odp. Piatek, 526 wypuszczonych piosenek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df2021 <- df %>% 
  filter(artist_count == 1, released_year == 2021) %>% 
  group_by(artist.s._name) %>% 
  summarise(n_2021 = n())

df2022 <- df %>% 
  filter(artist_count == 1, released_year == 2022) %>% 
  group_by(artist.s._name) %>% 
  summarise(n_2022 = n())

df_merged <- df2021 %>% 
  inner_join(df2022, by = "artist.s._name")

df_merged %>%
  mutate(p_increase = 100 * (n_2022 - n_2021)/n_2021) %>% 
  arrange(-p_increase) %>% 
  head(1)

## Odp. SZA, wzrost procentowy: 1600%

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  select(artist.s._name, track_name, danceability_., streams, released_year) %>% 
  top_frac(0.1, danceability_.) %>% 
  mutate(y_since_release = 2024 - released_year, avg_streams_year = streams / y_since_release) %>% 
  top_n(1, avg_streams_year)
  
## Odp. Piosenka artystow Chencho Corleone i Bad Bunny - Me Porto Bonito

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df6 <- df %>% 
  mutate(streams_p = streams / in_spotify_playlists) %>% 
  top_frac(0.2, streams_p)

df6 %>% 
  summarise(mean_bpm = mean(bpm))

df6 %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp.Srednie tempo: 125.2, najczesciej wystepujaca skala jest molowa: 96

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# nie został sprecyzowany typ podziału na pory roku - wybieram meteorologiczny

df7 <- df %>% 
  mutate(season = ifelse(released_month > 2 & released_month < 12, floor(released_month / 3), 4))  %>% # 1 - wiosna, 2 - lato, 3 - jesien, 4 - zima
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.),
            mean_speechiness = mean(speechiness_.))

df7$season = c("spring", "summer", "autumn", "winter")

## Odp. tabelka, pod zmienna df7

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

#najpopularniejsze kombinacje key-mode w 2022
df8_1 <- df %>% 
  filter(key != "", released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

#jaka kombinacje najczesciej tworza artysci solowi (ogolnie, nie tylko w 2022 - tak rozumiem polecenie)
df8_2 <- df %>% 
  filter(key != "", artist_count == 1) %>% 
  group_by(key, mode) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

df8_1 %>% 
  inner_join(df8_2, by = c("key", "mode"))

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(streams)) %>% 
  top_n(1, sum_of_streams)

## Odp. The Weeknd, 23929760757 odtworzen

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df_separated <- df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  filter(artist.s._name != "")

df_pre2021 <- df_separated %>% 
  filter(released_year <= 2021) %>% 
  select(artist.s._name)

df_2022 <- df_separated %>% 
  filter(released_year == 2022) %>% 
  select(artist.s._name)

debut_artists <- df_2022 %>% 
  setdiff(df_pre2021)
 
df10 <- debut_artists %>% 
  inner_join(df_separated, by = "artist.s._name") %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = count)

## Odp. tabelka, pod zmienna df10

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df11 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts > 0) %>% 
  select(track_name, in_spotify_playlists, in_apple_playlists, in_spotify_charts, in_apple_charts)

## Odp. Takich piosenek jest az 337, zawarte w df11


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df_solo <- df %>% 
  filter(artist_count > 1) %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(streams), count = n()) %>% 
  mutate(streams_per_song_col = sum_of_streams / count) %>% 
  select(artist.s._name, streams_per_song_col)
  

df_collab <- df %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(streams), count = n()) %>%
  mutate(streams_per_song_solo = sum_of_streams / count) %>% 
  select(artist.s._name, streams_per_song_solo)

df12 <- df_solo %>% 
  inner_join(df_collab, by = "artist.s._name") %>% 
  filter(streams_per_song_solo > streams_per_song_col)


## Odp. Jest 55 takich artystow, odpowiedz w df12