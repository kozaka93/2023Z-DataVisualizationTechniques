library(tidyr)
library(dplyr)
library(lubridate)

df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & released_month <= 3) %>% 
  summarise(mean_streams = mean(as.numeric(streams, na.rm = TRUE)))
      
## Odp. Średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale to 216150568.



#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

a = df %>% 
       filter(artist_count > 2) %>% 
       summarise(n_playlists = sum(in_spotify_playlists))

b = df %>% 
      filter(artist_count <= 2) %>% 
      summarise(n_playlists = sum(in_spotify_playlists))

a

b

if (a > b) {
  print("Piosenki stworzone przez więcej niż 2 artystów są zawarte w większej liczbie playlist spotify niż piosenki stworzone przez 1 lub 2 artystów.")
} else {
  print("Piosenki stworzone przez więcej niż 2 artystów nie są zawarte w większej liczbie playlist spotify niż piosenki stworzone przez 1 lub 2 artystów.")
}

## Odp. Piosenki stworzone przez więcej niż 2 artystów są zawarte w mniejszej liczbie playlist spotify niż piosenki stworzone przez 1 lub 2 artystów.



#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df3 <- df %>%
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(release_day_of_week = weekdays(release_date))

result3 <- df3 %>%
  group_by(release_day_of_week) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)

## Odp. Piątek

cat("Najbardziej popularnym dniem wypuszczania nowych piosenek jest",  result3$release_day_of_week)



#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

result4 <- df %>% 
  filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name) %>%
  mutate(songs_2021 = sum(released_year == 2021),
         songs_2022 = sum(released_year == 2022)) %>%
  ungroup() %>%
  filter(songs_2021 > 0, songs_2022 > 0) %>%
  mutate(percentage_increase = ((songs_2022 - songs_2021) / songs_2021) * 100) %>%
  arrange(desc(percentage_increase)) %>%
  head(1)

## Odp. Artystą o największym procentowym wzroście liczby utworów wydanych w latach 2021–2022 jest: SZA.

cat("Artystą o największym procentowym wzroście liczby utworów wydanych w latach 2021–2022 jest:", result4$artist.s._name, "\n")
cat("Wzrost procentowy:", result4$percentage_increase, "\n")



#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

result5 <- df %>%
  filter(!is.na(streams) & !grepl("[^0-9.]", streams)) %>%
  mutate(streams = as.numeric(streams),
         plays_per_year = streams / (2023 - released_year + 1)) %>%
  filter(danceability_. >= quantile(danceability_., 0.9)) %>%
  group_by(artist.s._name) %>%
  summarize(avg_plays_per_year = mean(plays_per_year, na.rm = TRUE)) %>%
  arrange(desc(avg_plays_per_year)) %>%
  head(1)

## Odp. Artystą z największą średnią ilością odtworzeń rocznie w przypadku 10% najbardziej tanecznych piosenek, uwzględniając także rok 2023, jest: Chencho Corleone, Bad Bunny.

cat("Artystą z największą średnią ilością odtworzeń rocznie w przypadku 10% najbardziej tanecznych piosenek, uwzględniając także rok 2023, jest:", result5$artist.s._name, "\n")
cat("Średnia liczba odtworzeń rocznie:", result5$avg_plays_per_year, "\n")



#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

result6 <- df %>%
  mutate(streams = as.numeric(streams, na.rm = TRUE)) %>%
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>% 
  summarize(avg_tempo = mean(bpm, na.rm = TRUE),
            most_common_mode = names(which.max(table(mode, useNA = "ifany"))))

## Odp. Tempo: 121.3194; skala (mode): Major.

cat("Średnie tempo 20% najczęściej odtwarzanych utworów:", result6$avg_tempo, "\n")
cat("Najczęściej spotykana skala (mode) spośród 20% najczęściej odtwarzanych utworów:", result6$most_common_mode, "\n")



#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

seasons <- function(month) {
  if (month %in% 3:5) {
    return("Spring")
  } else if (month %in% 6:8) {
    return("Summer")
  } else if (month %in% 9:11) {
    return("Fall")
  } else {
    return("Winter")
  }
}

df7 <- df %>%
  mutate(season = sapply(released_month, seasons))

result7 <- df7 %>%
  group_by(season) %>%
  summarize(
    mean_danceability = mean(danceability_., na.rm = TRUE),
    mean_valence = mean(valence_., na.rm = TRUE),
    mean_energy = mean(energy_., na.rm = TRUE),
    mean_acousticness = mean(acousticness_., na.rm = TRUE),
    mean_instrumentalness = mean(instrumentalness_., na.rm = TRUE),
    mean_liveness = mean(liveness_., na.rm = TRUE),
    mean_speechiness = mean(speechiness_., na.rm = TRUE)
  )

result7
## Odp.

#season   mean_danceability  mean_valence  mean_energy  mean_acousticness  mean_instrumentalness   mean_liveness   mean_speechiness
#<chr>        <dbl>              dbl>        <dbl>          <dbl>               <dbl>                  <dbl>            <dbl>
#1 Fall       65.3               46.2        62.2           26.7                2.02                    18.0            10.2 
#2 Spring     68.0               51.0        64.3           28.3                1.4                     18.4            11.0 
#3 Summer     69.2               51.2        65.8           23.7                2.74                    17.6            9.71
#4 Winter     65.7               56.1        64.7           28.5                0.596                   18.7            9.50



#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

result8 <- df %>%
  filter(released_year == 2022) %>%
  filter(artist_count == 1) %>%
  group_by(key, mode) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  arrange(desc(count)) %>%
  head(1)

## Odp. Para trybów kluczowych najczęściej tworzona przez artystów solowych spośród 10 najpopularniejszych par trybów kluczowych w 2022 r. to: G Major z liczbą 25.

cat("Para trybów kluczowych najczęściej tworzona przez artystów solowych spośród 10 najpopularniejszych par trybów kluczowych w 2022 r. to:",
    result8$key, result8$mode, "z liczbą", result8$count, "\n")



#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

result9 <- df %>%
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>%
  summarize(total_plays = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_plays)) %>% 
  top_n(1, total_plays) %>%
  select(artist.s._name, total_plays)

## Odp. Artysta, który ogółem odtworzył najwięcej swoich piosenek to: The Weeknd z całkowitą liczbą 23929760757 odtworzeń.

cat("Artysta, który ogółem odtworzył najwięcej swoich piosenek to:", result9$artist.s._name, 
    "z całkowitą liczbą", result9$total_plays, "odtworzeń.")



#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

result10 <- df %>%
  filter(released_year >= 2022) %>%
  group_by(track_name, artist.s._name, key, mode) %>%
  summarize(total_songs = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(track_name) %>%
  mutate(artists = ifelse(n() > 1, paste(artist.s._name, collapse = ", "), artist.s._name)) %>%
  ungroup() %>%
  mutate(artists = ifelse(grepl(",", artists), NA, artists)) %>%
  separate_rows(artists, sep = ", ") %>%
  group_by(artists, key, mode) %>%
  summarize(total_songs = sum(total_songs), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = c(key, mode), values_from = total_songs, values_fill = 0)


## Odp. Zestawienie wygląda następujaco:

result10

#    artists      E_Minor G_Major F_Major `C#_Major` `G#_Major` `_Major` D_Major `G#_Minor` `_Minor` A_Minor `A#_Major` B_Major
#     <chr>        <int>   <int>   <int>      <int>      <int>    <int>   <int>      <int>    <int>   <int>      <int>   <int>
#1    (G)I-DLE       2       0       0          0          0        0       0          0        0       0          0       0
#2     Agust D       0       1       0          0          0        0       0          0        0       0          0       0
#3   Anggi Marito    0       0       1          0          0        0       0          0        0       0          0       0
#4   Armani White    0       0       0          1          0        0       0          0        0       0          0       0
#5     BIGBANG       0       0       0          0          1        0       0          0        0       0          0       0
#6    BLACKPINK      0       1       0          0          0        2       0          0        0       0          0       0
#7       BTS         0       1       0          1          1        0       1          1        0       0          0       0
#8    Bad Bunny      0       0       1          1          2        1       0          0        1       1          2       1
#9    Bellakath      0       1       0          0          0        0       0          0        0       0          0       0
#10  Benson Boone    0       0       0          0          0        0       0          0        0       0          1       0
# ℹ 151 more rows



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

result11 <- df %>%
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts > 0) %>%
  select(track_name, artist.s._name)

## Odp.
result11


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

result12 <- df %>%
  separate_wider_delim(artist.s._name, delim = ",", names = c("a", "b", "c", "d", "e", "f", "g", "h"), too_few = "align_start") %>%
  pivot_longer(cols = a:h, names_to = "variable", values_to = "artist_solo", values_drop_na = TRUE) %>% 
  mutate(artist_solo_more = if_else(artist_count == 1, "solo", "more")) %>% 
  group_by(artist_solo, artist_solo_more) %>% 
  summarise(sum_streams = sum(as.numeric(streams))/n()) %>% 
  spread(artist_solo_more, sum_streams) %>% 
  group_by(artist_solo) %>% 
  filter(!is.na(solo), !is.na(more), solo > more) %>% 
  select(artist_solo)

## Odp. Artyści, którzy średnio generują więcej odtworzeń na piosenkę gdy tworzą solo niż gdy tworzą z innymi artystami to:
result12

#Jest 34 takich artystów.

