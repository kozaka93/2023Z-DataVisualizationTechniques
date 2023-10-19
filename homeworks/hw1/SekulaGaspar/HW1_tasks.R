library(tidyr)
library(dplyr)


df <- read.csv("spotify-2023.csv", encoding = "UTF-8")
# konieczne, bo nie wszytskie wartości w kolumnie 'streams' są liczbami
df <- df %>% mutate(streams = as.numeric(streams))


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

solution_1 <- df %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>% 
  select(streams) %>%
  mutate(streams_num = as.numeric(streams)) %>% 
  summarise(ave_streams = mean(streams_num))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

solution_2 <- df %>% 
  filter((!is.na(in_spotify_playlists)) & (!is.na(in_apple_playlists)) & (!is.na(in_deezer_playlists))) %>% 
  select(artist_count, in_spotify_playlists, in_apple_playlists, in_deezer_playlists) %>% 
  mutate(playlists = in_spotify_playlists + in_apple_playlists + as.numeric(gsub(",", "", in_deezer_playlists))) %>%
  select(artist_count, playlists) %>%
  mutate( number_of_artists = ifelse(artist_count > 2, "more", "leq")) %>% 
  group_by(number_of_artists) %>% 
  summarise( total = sum(playlists))
  

## Odp. NIE 


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
  
solution_3 <- df %>% 
  select(track_name, released_day, released_month, released_year) %>% 
  mutate(weekday = weekdays( as.Date( paste(released_year, released_month, released_day, sep="-")))) %>% 
  group_by(weekday) %>% 
  summarise(total_releases = n()) %>% 
  arrange(-total_releases)


## Odp. piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

solo_released_21 <- df %>%  
  select(artist.s._name, released_year, artist_count, track_name) %>% 
  filter( (artist_count == 1) & (released_year == 2021) ) %>% 
  group_by(artist.s._name) %>% 
  summarise(number_of_released_21 = n())

solo_released_22 <- df %>%  
  select(artist.s._name, released_year, artist_count, track_name) %>% 
  filter( (artist_count == 1) & (released_year == 2022) ) %>% 
  group_by(artist.s._name) %>% 
  summarise(number_of_released_22 = n())

solution_4 <- inner_join(solo_released_21, solo_released_22, by = "artist.s._name") %>% 
  mutate(growth = 100* (number_of_released_22 - number_of_released_21)/number_of_released_21) %>% 
  select(artist.s._name, growth) %>% 
  arrange(-growth) %>% 
  head(1)
  

## Odp. SZA (1600%)

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

solution_5 <- df %>% 
  top_frac(0.10, wt = danceability_.) %>% 
  group_by(artist.s._name, track_name) %>% 
  summarise( streams_by_year = streams / (2023 - released_year + 1) ) %>% 
  arrange(-streams_by_year) %>% 
  head(1)
  

## Odp. Chencho Corleone, Bad Bunny (tytuł: Me Porto Bonito)

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_20_percent <- df %>% 
  mutate(streams_playlists = as.numeric(streams) / in_spotify_playlists) %>% 
  top_frac(0.2, streams_playlists)

solution_6_a <- top_20_percent %>% 
  summarise( ave_bpm = mean(bpm))

solution_6_b <- top_20_percent %>%
  group_by(mode) %>%
  summarise( n = n() ) 
  

## Odp. średnie tempo: 125.2; dominująca skala: "Minor"

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# UWAGA: rozpatrywane są pory roku meteorologiczne 
solution_7 <- df %>% 
  select(track_name, released_month, danceability_.:speechiness_.) %>% 
  mutate( season = ifelse( released_month %in% c(1, 2, 12), "winter",
                           ifelse( released_month %in% c(3, 4, 5), "spring",
                                   ifelse( released_month %in% c(6, 7, 8), "summer","autumn")))) %>% 
  group_by(season) %>%
  summarise( danceability = mean(danceability_.),
             valence = mean(valence_.),
             energy = mean(energy_.),
             acousticness = mean(acousticness_.),
             instrumentalness = mean(instrumentalness_.),
             liveness = mean(liveness_.),
             speechiness = mean(speechiness_.))
  

## Odp. tabelka
View(solution_7)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

solution_8 <- df %>% 
  filter(released_year == 2022 & artist_count == 1) %>% 
  select(key, mode, streams) %>% 
  mutate( key_mode = paste0(key, mode)) %>%
  group_by(key_mode) %>%
  summarise(popularity = sum(as.numeric(streams))) %>%
  arrange(-popularity) %>%
  head(1)


## Odp. G# Major  

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams)) %>% 
  arrange(-total_streams) %>% 
  head(1)

## Odp. The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

total_number_of_artists <- df %>% 
  select(artist_count) %>% 
  max()

artists <- paste0("artist_", 1:total_number_of_artists)


debuts <- df %>% 
  separate(artist.s._name, into = artists, sep = ", ") %>% 
  pivot_longer(all_of(artists), names_to = NULL, values_to = "name") %>% 
  filter(!(is.na(name)) & (name !="")) %>% 
  group_by(name) %>% 
  mutate(debut_year = min(released_year)) %>% 
  filter(debut_year == 2022)

solution_10 <- df %>% 
  filter(artist.s._name %in% debuts$name) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(artist.s._name, key_mode) %>% 
  summarise(number_of_tracks = n()) %>% 
  pivot_wider(names_from = key_mode, values_from = number_of_tracks) %>% 
  mutate_all(~replace_na(.,0))
  
## Odp.
View(solution_10)



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

solution_11 <- df %>% 
  filter((in_spotify_playlists > in_apple_playlists) & (in_apple_charts != 0) & (in_spotify_charts == 0) ) %>% 
  select(track_name) %>% 
  arrange(track_name)


## Odp. 
View(solution_11)

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solution_12 <- df %>% 
  separate(artist.s._name, into = artists, sep = ", ") %>% 
  pivot_longer(all_of(artists), names_to = NULL, values_to = "name") %>% 
  filter(!(is.na(name)) & (name !="")) %>% 
  mutate(solo = ifelse(artist_count==1, "solo", "not_solo")) %>% 
  group_by(name, solo) %>% 
  mutate(ave_streams = mean(streams)) %>% 
  select(name, solo, ave_streams) %>% 
  distinct() %>% 
  pivot_wider(names_from = solo, values_from = ave_streams) %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  filter((solo != 0) & (not_solo != 0) & (solo > not_solo)) %>% 
  select(name) %>% 
  arrange(name)

## Odp. 
View(solution_12)

