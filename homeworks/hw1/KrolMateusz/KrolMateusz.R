library(tidyr)
library(dplyr)
library(DescTools)

df <- read.csv('C:\\Users\\11mat\\Desktop\\spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>%
  filter(released_year == 2023, released_month <= 3) %>%
  summarise(srednia = mean(as.numeric(streams), na.rm = TRUE))

## Odp. Piosenki opublikowane w pierwszym kwartale 2023 odtwarzano średnio 216150568 razy.

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

wiecej <- df %>% 
  filter(artist_count > 2) %>% 
  summarise(n_playlists = sum(in_spotify_playlists))

mniej <- df %>% 
  filter(artist_count <= 2) %>% 
  summarise(n_playlists = sum(in_spotify_playlists))

wiecej - mniej # Odpowiedź na pytanie jest twierdząca, jeżeli wynik jest dodatni

## Odp. Piosenki stworzone przez więcej niż 2 artystów nie są zawarte na większej liczbie playlist spotify,
##      niż piosenki stworzone przez 1 lub 2 artystów - różnica wynosi 4099467


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%
  mutate(release_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(day_of_week = weekdays(release_date)) %>%
  group_by(day_of_week) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)

## Odp. Najwięcej nowych piosenek jest wydawanych w piątki - odnotowano 526 takich utworów.

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

z4a <- df %>% 
  filter(artist_count == 1) %>%
  filter(released_year == 2021 | released_year == 2022) %>% # Wybieram tylko utwory z lat 2021-2022
  group_by(artist.s._name) %>%
  filter(min(released_year) == 2021) %>%
  filter(max(released_year) == 2022) %>% # W ten sposób upewniam się, że wykonawca publikował w obu latach
  group_by(artist.s._name, released_year) %>%
  summarize(song_year = n()) 
  
z4result = data.frame(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("artist", "growth_ratio"))))

  for (i in seq(nrow(z4a)/2)) {
    z4result[nrow(z4result) + 1,] = c(z4a[2*i, 1], ((z4a[2*i, 3] - z4a[2*i - 1, 3])/z4a[2*i - 1, 3]))
    # W pierwszej kolumnie zapisuję nazwisko, w drugiej - średni przyrost (w %) między 2021 a 2022
  }

z4result %>%
  arrange(desc(growth_ratio)) %>%
  head(1)

## Odp. Największy procentowy wzrost odnotowała SZA - był to wzrost aż o 1600%.

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>%
  top_frac(0.1, danceability_.) %>%
  mutate(streams_per_year = as.numeric(streams)/((max(df$released_year) + 1) - released_year)) %>%
  arrange(desc(streams_per_year)) %>%
  head(1)

## Odp. Jest to piosenka autorstwa Chencho Corleone i Bad Bunny.

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
  mutate(streams = as.numeric(streams, na.rm = TRUE)) %>%
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>% 
  summarize(avg_bpm = mean(bpm, na.rm = TRUE), modemode = Mode(mode))

## Odp. Średnie tempo wynosi 121.3194, zaś najczęściej występującą skalą jest Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

z7a <- df %>% 
  mutate(season = case_when(
  released_month %in% c(4, 5) | (released_month == 3 & released_day >= 21) | (released_month == 6 & released_day < 22) ~ "1-spr",
  released_month %in% c(7, 8) | (released_month == 6 & released_day >= 22) | (released_month == 9 & released_day < 23) ~ "2-sum",
  released_month %in% c(10, 11) | (released_month == 12 & released_day >= 22) | (released_month == 3 & released_day < 21) ~ "3-fall",
  T ~ "4-wint" # Liczby przy nazwach, aby uniknąć alfabetycznego sortowania pór roku w tabeli
)) %>% 
  select('danceability_.', 'energy_.', 'acousticness_.', 'instrumentalness_.', 'liveness_.', season)

z7a %>%
  group_by(season) %>%
  summarise(avg_dance = mean(danceability_.), avg_energy = mean(energy_.), 
            avg_instr = mean(instrumentalness_.), avg_life = mean(liveness_.))


## Odp. Taneczność utworów jest największa wiosną i latem, a najmniejsza jesienią i zimą.
#       Energetyczność osiąga swój szczyt latem, zaś gwałtownie spada jesienią.
#       Instrumentalność jest największa w lecie, zaś najmniejsza w zimie.
#       Żywość wydawanych utworów jest najwyższa wiosną, a najniższa latem.

#### 8. Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

z8a <- df %>%
  filter(released_year == 2022) %>%
  group_by(key, mode) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

z8result = data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("key", "mode", "solos_count"))))

for (i in seq(10)) {
  z8b <- df %>%
    filter(key == as.character(z8a[i, 1])) %>%
    filter(mode == as.character(z8a[i, 2])) %>%
    filter(artist_count == 1) %>%
    summarize(count = n())
  
  z8result[nrow(z8result) + 1,] = c(z8a[i,1], z8a[i,2], z8b[1,1])
}

z8result %>%
  arrange(desc(solos_count)) %>%
  head(1)

## Odp. Spośród 10 najpopularniejszych par key-mode w 2022, artyści solowi najchętniej wybierają G Major.

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarize(total_plays = sum(na.omit(as.numeric(streams, na.rm = TRUE)))) %>%
  arrange(desc(total_plays)) %>%
  head(1)

## Odp. Najwięcej odtworzeń piosenek w sumie ma The Weeknd - aż 14185552870.

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 


## Odp.

df %>%
  group_by(artist.s._name) %>%
  filter(min(released_year) == 2022) %>% # Rok debiutu
  group_by(artist.s._name, key, mode) %>%
  summarize(total_songs = n()) 

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>%
  filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_spotify_charts == 0) %>%
  filter(in_apple_charts > 0) %>%
  select(track_name)

## Odp. Jest 337 takich utworów.


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>%
  separate_wider_delim(artist.s._name, delim = ",", names = c("Art1", "Art2", "Art3", "Art4", "Art5", "Art6", "Art7", "Art8"), 
                       too_few = "align_start") %>%
  pivot_longer(cols = Art1:Art8, names_to = "variable", values_to = "solo_art", values_drop_na = TRUE) %>% 
  mutate(solo_split = if_else(artist_count == 1, "solo", "team")) %>% 
  group_by(solo_art, solo_split) %>% 
  summarise(total_streams = sum(as.numeric(streams))/n()) %>% 
  spread(solo_split, total_streams) %>% 
  filter(!is.na(solo), !is.na(team)) %>%
  mutate(solo_team_ratio = solo/team) %>%
  arrange(desc(solo_team_ratio))

## Odp. Jest 65 takich artystów, z czego najwyższy stosunek odtworzeń solowych utworów do utworów nagrywanych z innymi
#       ma Carin Leon (aż 27990, w przypadku drugiego Johna Legend ten stosunek to już tylko 26.7)