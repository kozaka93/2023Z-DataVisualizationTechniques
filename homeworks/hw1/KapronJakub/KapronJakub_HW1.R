library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv') # trzeba bedzie zmienic


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>%
  mutate(streams = as.numeric(streams)) %>%
  summarise(mean(streams)) %>%
  unlist(use.names = FALSE)
  
## Odp. 216 150 568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
artists_one_or_two <- df %>%
  filter(artist_count <= 2) %>%
  summarise(sum(in_spotify_playlists)) %>%
  unlist(use.names = FALSE)

artists_three_or_more <- df%>%
  filter(artist_count > 2) %>%
  summarise(sum(in_spotify_playlists)) %>%
  unlist(use.names = FALSE)

artists_three_or_more > artists_one_or_two

## Odp. Nie.


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
dates <- paste(df$released_year,
               df$released_month,
               df$released_day,
               sep = "-")
dates <- as.Date(dates)

df %>%
  mutate(weekday = weekdays(dates)) %>%
  group_by(weekday) %>%
  summarise(n = n()) %>%
  top_n(1)

## Odp. Piatek.

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>%
  filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = released_year, values_from = n) %>%
  mutate(yearly_increase = (`2022`/`2021` - 1) * 100) %>%
  top_n(1, yearly_increase)

## Odp. SZA.

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  filter(danceability_. > quantile(danceability_., .9)) %>%
  mutate(years_from_release = 2024-released_year, 
         avg_streams = as.numeric(streams) / years_from_release) %>%
  top_n(1, avg_streams) %>%
  select(artist.s._name, track_name, avg_streams)

## Odp. Chencho Corleone & Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
bpm_and_mode <-  df %>%
  mutate(streams_per_playlists = as.numeric(streams)/in_spotify_playlists) %>%
  filter(streams_per_playlists > quantile(streams_per_playlists, .8, na.rm = T)) %>%
  select(bpm, mode)

table(bpm_and_mode$mode)
mean(bpm_and_mode$bpm)

## Odp. Skala -> Major & Minor (95:96), srednie tempo -> 125.2775

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>%
  mutate(season = case_when(
    released_month %in% c(4, 5) | (released_month==3 & released_day>=21) | (released_month==6 & released_day<22) ~ "spring",
    released_month %in% c(10, 11) | (released_month==9 & released_day>=23) | (released_month==12 & released_day<22) ~ "fall",
    released_month %in% c(12, 1, 2, 3) ~ "winter",
    TRUE ~ "summer"
  )) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.))

## Odp. Wiosna i lato -> przede wszystkim najbardziej taneczne, a takze stosunkowo energetyczne utwory
## Zima -> Najbardziej energetyczne, calkiem tanneczne
## Jesien -> najgorsza we wszystkich wskaznikach

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
popular_mode_key_pairs <- df %>%
  filter(released_year==2022) %>%
  group_by(mode, key) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  top_n(10, n) %>%
  select(mode, key)

df %>%
  filter(released_year==2022 & artist_count==1) %>%
  group_by(mode, key) %>%
  summarise(n = n()) %>%
  right_join(popular_mode_key_pairs, by = c('mode', 'key')) %>%
  ungroup() %>%
  top_n(1, n)

## Odp. G-Major.

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  select(artist.s._name, streams) %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(as.numeric(streams))) %>%
  top_n(1)

## Odp. The Weeknd.

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
artits_debut_2022 <-df %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>%
  summarise(debut_year = min(released_year)) %>%
  filter(debut_year == 2022 & artist.s._name != "") %>%
  pull(artist.s._name)

df %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name %in% artits_debut_2022) %>%
  group_by(artist.s._name, mode, key) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = c(mode, key), values_from = n, values_fill = 0)
  
## Odp. ^Wyzej.

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  filter(in_spotify_playlists > in_apple_playlists & 
           in_spotify_charts==0 & in_apple_charts > 0) %>%
  select(track_name, artist.s._name)

## Odp. ^Wyzej


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df %>%
  separate_rows(artist.s._name, sep = ", ") %>%
  mutate(solo = case_when(
    artist_count==1 ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(artist.s._name, solo) %>%
  summarise(mean_streams = mean(as.numeric(streams))) %>%
  pivot_wider(names_from = solo, values_from = mean_streams, values_fill = 0) %>%
  filter(artist.s._name!="" & `0`!=0 & `0`<`1`) %>%
  pull(artist.s._name)

## Odp. ^Wyzej. Np.: Ariana Grande, Central Cee, Kayne West, NLE Choppa, Playboi Carti.



