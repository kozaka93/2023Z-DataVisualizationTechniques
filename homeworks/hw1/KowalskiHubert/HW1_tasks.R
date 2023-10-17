library(tidyr)
library(dplyr)
library(stringr)
library(stringi)

df <- read.csv('./Data/spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

# df <- df %>%
#   mutate(streams = as.numeric(streams))

z1 <- df %>%
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>%
  summarise(mean_streams = mean(streams))
z1

## Odp. Srednia liczba odtworzen piosenek opublikowanych w roku 2023 w pierwszym kwartale wynosi: 216 150 568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

z2 <- df %>%
  mutate(how_many_artists = ifelse(artist_count > 2, 'more than 2', '1 or 2')) %>%
  group_by(how_many_artists) %>%
  summarise(mean_num_playlists = mean(in_spotify_playlists))

z2

## Odp. Nie, piosenki stworzone przez więcej niż 2 artystów są średnio zawarte na mniejszej liczbie playlist spotify


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

z3 <- df %>%
  mutate(released_date = as.POSIXlt(
    paste(released_day, released_month, released_year, sep = '/'), format = "%d/%m/%Y")) %>%
  mutate(weekday = weekdays(released_date)) %>%
  group_by(weekday) %>%
  summarise(num_releases = n()) %>%
  arrange(-num_releases) %>%
  head(1)
z3

## Odp. Najpopularniejszym dniem tygodnia w wypuszczaniu piosenek jest piątek.

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2022 i w 2021.

z4 <- df %>%
  filter(artist_count == 1, released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(num_releases = n()) %>%
  pivot_wider(names_from = released_year, names_prefix = "year_", values_from = num_releases, values_fill = 0) %>%
  filter(year_2021 > 0 & year_2022 > 0) %>%
  mutate(percent_increase = (year_2022 - year_2021) / year_2021 * 100) %>%
  arrange(-percent_increase) %>%
  head(1)

z4

## Odp. SZA, wzrost liczby piosenek o 1600% (z 1 do 17).

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

z5 <- df %>%
  arrange(-danceability_.) %>%
  slice_head(prop = 0.1) %>%
  mutate(num_years_present = 2023 - released_year + 1) %>%
  mutate(streams_per_year = streams / num_years_present) %>%
  arrange(-streams_per_year) %>%
  select(track_name, artist.s._name, num_years_present, streams_per_year) %>%
  head(1)

z5

## Odp. Piosenka "Me Porto Bonito" od artystów Chencho Corleone i Bad Bunny : 720378909 odtworzeń na rok.

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

z6 <- df %>%
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>%
  arrange(-streams_per_playlist) %>%
  slice_head(prop = 0.2)

z6 %>%
  summarise(mean_bpm = mean(bpm, na.rm = T))
  
z6 %>%
  group_by(mode) %>%
  summarise(n = n())
  
## Odp. Śr tempo: 125.2, najczęściej występująca skala: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# | (released_month == 12 & released_day > 22) | (released_month == 3 & released_day < 21) 

z7 <- df %>%
  mutate(season = case_when(
    released_month %in% c('1','2') | (released_month=='12' & released_day>=22) | 
      (released_month=='3' & released_day<=20) ~ 'winter',
  released_month %in% c('4','5') | (released_month=='3' & released_day>20) | 
     (released_month=='6' & released_day<=20) ~ 'spring',
  released_month %in% c('7','8') | (released_month=='6' & released_day>20) |
      (released_month=='9' & released_day>='22')~ 'summer',
  TRUE ~ 'autumn')) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.))
z7

## Odp. Piosenki publikowane latem są najbardziej taneczne.
## Z kolei te publikowane jesienią są najbardziej akustyczne.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

pairs <- df %>%
  filter(released_year == 2022, key != "") %>%
  mutate(key_mode = paste0(key, "-", mode)) %>%
  group_by(key_mode) %>%
  summarise(n = n()) %>%
  top_n(10, n)
  
z8 <- df %>%
  mutate(key_mode = paste0(key, "-", mode)) %>%
  filter(key_mode %in% pairs$key_mode, artist_count == 1) %>%
  group_by(key_mode) %>%
  summarise(n_solo = n()) %>%
  arrange(-n_solo)

z8

## Odp. Spośród takich par solowi artyści najczęściej tworzą: G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

z9 <- df %>%
  separate_wider_delim(artist.s._name, delim = ",", names_sep = "_", too_few = "align_start") %>% 
  pivot_longer(cols = c("artist.s._name_1", "artist.s._name_2", "artist.s._name_3", "artist.s._name_4", "artist.s._name_5", 
                        "artist.s._name_6", "artist.s._name_7", "artist.s._name_8"), 
               values_to = "artist_name") %>% 
  mutate(artist_name = str_replace_all(artist_name, replacement = "", pattern = " ")) %>%
  select(-name) %>%
  filter(!is.na(artist_name)) %>%
  group_by(artist_name) %>%
  summarise(sum_streams = sum(streams)) %>%
  arrange(-sum_streams) %>%
  head(1)

z9

## Odp. The Weekend, 23 929 760 757 odtworzeń

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debiut <- df %>%
  separate_wider_delim(artist.s._name, delim = ",", names_sep = "_", too_few = "align_start") %>% 
  pivot_longer(cols = c("artist.s._name_1", "artist.s._name_2", "artist.s._name_3", "artist.s._name_4", "artist.s._name_5", 
                        "artist.s._name_6", "artist.s._name_7", "artist.s._name_8"), 
               values_to = "artist_name") %>% 
  mutate(artist_name = str_replace_all(artist_name, replacement = "", pattern = " ")) %>%
  select(-name) %>%
  filter(!is.na(artist_name)) %>%
  group_by(artist_name) %>%
  summarise(debiut = min(released_year)) %>%
  filter(debiut == 2022) %>%
  select(artist_name) %>%
  simplify2array()

z10 <- df %>%
  separate_wider_delim(artist.s._name, delim = ",", names_sep = "_", too_few = "align_start") %>% 
  pivot_longer(cols = c("artist.s._name_1", "artist.s._name_2", "artist.s._name_3", "artist.s._name_4", "artist.s._name_5", 
                        "artist.s._name_6", "artist.s._name_7", "artist.s._name_8"), 
               values_to = "artist_name") %>% 
  mutate(artist_name = str_replace_all(artist_name, replacement = "", pattern = " ")) %>%
  select(-name) %>%
  filter(!is.na(artist_name), artist_name != "") %>%
  filter(artist_name %in% debiut) %>%
  group_by(artist_name, key, mode) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = n, values_fill = 0) %>%
  arrange(artist_name)

z10
## Odp. Zestawienie w zmienej z10, np. Alicia Keys ma 1 utwór w G# Minor

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

z11 <- df %>%
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts > 0) %>%
  select(track_name, artist.s._name, in_spotify_playlists, in_apple_playlists, in_spotify_charts, in_apple_charts)

z11

## Odp. W zmiennej z11 (łącznie 337 takich piosenek), np. "Hits different" Taylor Swift

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

sep_artists <- df %>%
  separate_wider_delim(artist.s._name, delim = ",", names_sep = "_", too_few = "align_start") %>% 
  pivot_longer(cols = c("artist.s._name_1", "artist.s._name_2", "artist.s._name_3", "artist.s._name_4", "artist.s._name_5", 
                        "artist.s._name_6", "artist.s._name_7", "artist.s._name_8"), 
               values_to = "artist_name") %>% 
  mutate(artist_name = str_replace_all(artist_name, replacement = "", pattern = " ")) %>%
  select(-name) %>%
  filter(!is.na(artist_name), artist_name != "")

z12 <- sep_artists %>%
  mutate(artist_count = if_else(artist_count == 1,"solo", ">1")) %>%
  group_by(artist_name, artist_count) %>%
  summarise(streams_per_song = mean(streams)) %>%
  pivot_wider(names_from = artist_count, values_from = streams_per_song, values_fill = 0) %>%
  filter(`>1` > 0 & solo > 0 & solo > `>1`)

## Odp. Jest 58 takich artystów, np. Ariana Grande, Bruno Mars, Eminem, Justin Bieber...