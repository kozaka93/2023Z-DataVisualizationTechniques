library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('./spotify-2023.csv')
df <- tibble(df)
df <- df[-575, ]  # Problem z linią w pliku (w col streams jakiś string)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale
df %>% 
  filter(released_year == 2023) %>%
  filter(released_month %in% c(1, 2, 3)) %>%
  summarise(mean_streams = mean(as.numeric(streams)))
## Odp. 216150568









#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
tmp <- df %>%
  group_by(artist_count) %>%
  summarise(number_of_playlist_in = sum(in_spotify_playlists))

one_or_two <- tmp[c(1, 2),] %>%
  summarise(number_of_playlist_in = sum(number_of_playlist_in))

more_than_two <- tmp[c(-1, -2),] %>%
  summarise(number_of_playlist_in = sum(number_of_playlist_in))

more_than_two > one_or_two
## Odp. Częściej są to piosenki stworzone przez 1 lub 2 artystów, zatem nie.











#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%
  transmute(formated_date = paste0(released_year, "-",released_month, "-", released_day)) %>%
  mutate(weekday = strftime(formated_date, "%A")) %>%
  group_by(weekday) %>%
  summarise(releases_in_weekdays = n()) %>%
  arrange(-releases_in_weekdays)

## Odp. Piątek








#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.
from_2021 <- df %>%
  filter(artist_count == 1) %>%
  filter(released_year == 2021) %>%
  group_by(artist.s._name) %>%
  summarise(released_songs = n())

from_2022 <- df %>%
  filter(artist_count == 1) %>%
  filter(released_year == 2022) %>%
  group_by(artist.s._name) %>%
  summarise(released_songs = n())

inner_join(from_2021, from_2022, by="artist.s._name") %>%
  mutate(songs_released_increase_prct = (released_songs.y/released_songs.x) * 100 - 100) %>%
  arrange(-songs_released_increase_prct)

## Odp. SZA








#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>%
  select(track_name, artist.s._name, released_year, streams, danceability_.) %>%
  arrange(-danceability_.) %>%
  head((1/10) * length(df$track_name)) %>% # 10% z danceability największym
  mutate(years_from_release = 2024 - released_year) %>%
  mutate(mean_streams_per_year = as.numeric(streams)/years_from_release) %>%
  arrange(-mean_streams_per_year) %>%
  select(track_name, artist.s._name, mean_streams_per_year)

## Odp. Byli to Chencho Corleone i Bad Bunny z piosenką Me Porto Bonito









#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
tmp <- df %>%
  select(track_name, artist.s._name, streams, in_spotify_playlists, bpm, mode) %>%
  mutate(streams_per_nplaylist = as.numeric(streams)/in_spotify_playlists) %>%
  arrange(-streams_per_nplaylist) %>%
  head(2/10 * length(df$track_name)) # 20% z największym streams_per_nplaylist

#Srednie Tempo
tmp %>%
  summarise(MeanBpm = mean(bpm))

#Najczęściej występujący mode
tmp %>%
  group_by(mode) %>%
  summarise(count = n()) %>%
  arrange(-count)

## Odp. Średnie tempo to 125, a najczęstszy mode w tej grupie to minor









#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# Funkcja zwraca porę roku (w postaci stringa) dla danego dnia
get_season <- function(date) {
  winter_start <- as.Date("2020-12-22", format = "%Y-%m-%d") 
  spring_start <- as.Date("2020-3-21",  format = "%Y-%m-%d") 
  summer_start <- as.Date("2020-6-22",  format = "%Y-%m-%d") 
  autumn_start <- as.Date("2020-9-23",  format = "%Y-%m-%d") 
  
  # Bierzemy 2020, bo był przestępny (Luty ma 29 dni, nigdy nie ma błedu)
  d <- as.Date(strftime(date, format="2020-%m-%d"))
  
  ifelse (d >= winter_start | d < spring_start, "Winter",
          ifelse (d >= spring_start & d < summer_start, "Spring",
                  ifelse (d >= summer_start & d < autumn_start, "Summer", "Autumn")))
}

df %>%
  mutate(formated_date = paste0(released_year, "-",released_month, "-", released_day)) %>%
  mutate(season = get_season(formated_date)) %>%
  group_by(season) %>%
  summarise(mean(danceability_.), mean(valence_.), mean(energy_.), mean(acousticness_.), mean(instrumentalness_.), mean(liveness_.), mean(speechiness_.))

## Odp.
# season `mean(danceability_.)` `mean(valence_.)` `mean(energy_.)` `mean(acousticness_.)` `mean(instrumentalness_.)` `mean(liveness_.)` `mean(speechiness_.)`
# <chr>                   <dbl>             <dbl>            <dbl>                  <dbl>                      <dbl>              <dbl>                 <dbl>
# 1 Autumn                   63.9              47.0             61.2                   29.7                      1.96                17.6                  9.84
# 2 Spring                   68.6              49.5             64.2                   27.7                      1.67                18.3                 11.0 
# 3 Summer                   68.6              51.5             65.8                   24.7                      2.17                18.0                 10   
# 4 Winter                   66.8              57.4             65.9                   25.8                      0.733               18.8                  9.58








#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę, którą najcześcej tworzą artyści solowi.

# 10 najpopularniejszych par key-mode 2022
my_groups <- df %>%
  filter(released_year == 2022) %>%
  group_by(key, mode) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(10) %>%
  group_by(key, mode) %>%
  group_keys()

# Tu, gdy mamy wybrać tę parę, którą najczęściej tworzą ogólnie artyści solowi(tzn nie w 2022, ale ogólnie)
option_1 <- df %>%
  filter(artist_count == 1) %>%
  group_by(key, mode) %>%
  summarise(count = n()) %>%
  arrange(-count)

# Tu gdyby chodziło o pary, które artyści solowi najczęściej tworzyli w 2022
option_2 <- df %>%
  filter(artist_count == 1 & released_year == 2022) %>%
  group_by(key, mode) %>%
  summarise(count = n()) %>%
  arrange(-count)

inner_join(my_groups, option_1, by = c("key", "mode")) %>%
  arrange(-count)

inner_join(my_groups, option_2, by = c("key", "mode")) %>%
  arrange(-count)

## Odp. Zakładając, że opcja nr 2 jest tą o którą chodziło, odpowiedzią jest G Major

  
  
  
  


  
  
  
#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

tmp <- df %>% 
  select(artist.s._name, streams) %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists) %>%
  select(artists, streams) %>%
  group_by(artists) %>%
  summarise(allStreams = sum(as.numeric(streams))) %>%
  arrange(-allStreams)

## Odp. TheWeeknd








#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach.

# Debiutanci 2022 roku
debuted_2022 <- df %>%
  select(artist.s._name, released_year) %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists) %>%                                 # Dzielimy artystów
  select(artists, released_year) %>%
  group_by(artists) %>%
  summarise(debuted_in_2022 = all(released_year %in% c(2022, 2023)) & any(released_year == 2022)) %>% #Bierzemy tylko tych którzy mają piosenki tylko z lat 2022 i 2023 oraz mają przynajmniej jedną z 2022(czyli zadebiutowali w 2022)
  filter(debuted_in_2022 == TRUE) %>%
  select(artists)

# Artyści rozdzieleni w utworach niegranych solowo
unnested_artists <- df %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists)

# Bierzemy tylko piosenki debiutantów
inner_join(debuted_2022, unnested_artists, by = "artists") %>%
  group_by(artists, mode, key) %>%
  summarise(count = n()) %>%
  pivot_wider(values_from = count, names_from = c(mode, key), values_fill = 0)
  
  
## Odp. wynik jest ostatnim inner_join





#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  select(track_name, artist.s._name, in_apple_playlists, in_apple_charts, in_spotify_playlists, in_spotify_charts) %>%
  filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_spotify_charts == 0 & in_apple_charts > 0)
## Odp. Jest ich dużo








#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę, gdy tworzy solo niż gdy tworzy z innymi artystami?

# How when solo
solo <- df %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists) %>% 
  select(track_name, artists, artist_count, streams) %>%
  filter(artist_count == 1) %>%
  group_by(artists) %>%
  summarise(meanStreamsSolo = mean(as.numeric(streams)))

# How when not solo
not_solo <- df %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists) %>% 
  select(track_name, artists, artist_count, streams) %>%
  filter(artist_count != 1) %>%
  group_by(artists) %>%
  summarise(meanStreamsNotSolo = mean(as.numeric(streams)))

# Bierzemy tylko tych, którzy są w obu grupach, by móc porównać
inner_join(solo, not_solo, by="artists") %>%
  filter(meanStreamsSolo > meanStreamsNotSolo)


## Odp. Wielu



