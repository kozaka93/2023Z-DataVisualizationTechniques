library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')
df <- filter(df, streams != 'BPM110KeyAModeMajorDanceability53Valence75Energy69Acousticness7Instrumentalness0Liveness17Speechiness3') # usuwam błędny wiersz

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(released_year == 2023 & released_month %in% c(1, 2, 3)) %>%
  summarise(average_streams = mean(as.numeric(streams))) %>% pull(1)

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
tmp1 <- df %>%
  filter(artist_count == 1 | artist_count == 2) %>%
  summarise(in_spotify_playlists_1_or_2_artists = sum(in_spotify_playlists))
tmp2 <- df %>%
  filter(artist_count > 2) %>%
  summarise(in_spotify_playlists_more_than_2_artists = sum(in_spotify_playlists))
if_else(tmp1$in_spotify_playlists_1_or_2_artists - tmp2$in_spotify_playlists_more_than_2_artists > 0,
        'Piosenek stworzonych przez 1 lub 2 artystów jest zawartch na większej liczbie playlist spotify.',
        if_else(tmp1$in_spotify_playlists_1_or_2_artists - tmp2$in_spotify_playlists_more_than_2_artists < 0,
        'Piosenek stworzonych więcej niż 2 artystów jest zawartch na większej liczbie playlist spotify.',
        'Piosenek stworzonych przez 1 lub 2 artystów jest zawartch na takiej samej liczbie playlist spotify co piosenek stworzonych przez więcej niż 2 artystów.'))
rm(tmp1, tmp2)

## Odp. Nie, piosenek stworzonych przez 1 lub 2 artystów jest zawartch na większej liczbie playlist spotify.


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>%
  mutate(release_date = as.Date(paste(df$released_year, df$released_month, df$released_day, sep = '-'))) %>%
  mutate(day_of_week = weekdays(release_date)) %>%
  group_by(day_of_week) %>%
  summarise(number_of_released_songs = n()) %>%
  arrange(-number_of_released_songs) %>%
  head(1) %>% pull(1)

## Odp. piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>%
  filter(artist_count == 1 & (released_year == 2021 | released_year == 2022)) %>%
  select(artist.s._name, released_year) %>%
  group_by(artist.s._name) %>%
  summarise(song_count_in_2021 = sum(if_else(released_year == 2021, 1, 0)),
            song_count_in_2022 = sum(if_else(released_year == 2022, 1, 0))) %>%
  filter(song_count_in_2021 > 0 & song_count_in_2022 > 0) %>%
  mutate(increase_of_number_of_song_releases_in_percentages = (song_count_in_2022 / song_count_in_2021) * 100) %>%
  select(artist.s._name, increase_of_number_of_song_releases_in_percentages) %>%
  arrange(-increase_of_number_of_song_releases_in_percentages) %>%
  head(1) %>% pull(1)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  filter(danceability_. > quantile(danceability_., 0.9)) %>%
  group_by(artist.s._name, released_year, danceability_.) %>%
  summarise(average_yearly_streams = (as.numeric(streams) / (2024 - released_year))) %>%
  arrange(-average_yearly_streams) %>%
  head(1) %>% pull(1)

## Odp. Chencho Corleone, Bad Bunny  

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>%
  filter(in_spotify_playlists != 0) %>%
  mutate(streams_per_number_of_playlists_spotify = as.numeric(streams) / in_spotify_playlists) %>%
  filter(streams_per_number_of_playlists_spotify > quantile(streams_per_number_of_playlists_spotify, 0.8)) %>%
  summarise(average_bpm = mean(bpm), most_common_mode = names(which.max(table(mode)))) %>%
  select(average_bpm, most_common_mode) %>% pull(average_bpm, most_common_mode)

## Odp. bpm: 125.2775 mode: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
getSeason <- function(DATES) {
  # przyjąłem kalendarzowe pory roku
  WS <- as.Date("2012-12-22", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-22",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Fall Equinox
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))}
df %>%
  mutate(season = getSeason(as.Date(paste(df$released_year, df$released_month, df$released_day, sep = '-')))) %>%
  group_by(season) %>%
  summarise(mean(danceability_.), mean(valence_.), mean(energy_.), mean(acousticness_.),
            mean(instrumentalness_.), mean(liveness_.), mean(speechiness_.)) %>%
  View()
rm(getSeason)

## Odp. W lecie i na wiosnę piosenki są najbardziej taneczne, zaś w jesieni najmniej taneczne.
## Najbardziej pozytywne piosenki są zimą, a najmniej jesienią. Latem są najbardziej instrumentalne, zaś zimą najmniej.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
tmp1 <- df %>%
  mutate(pair_key_mode = if_else(key == '', paste(key, mode, sep = ''), paste(key, mode, sep = ' '))) %>%
  group_by(pair_key_mode) %>%
  summarise(count_of_pair_key_mode = n()) %>%
  arrange(-count_of_pair_key_mode) %>%
  head(10)
df %>%
  filter(artist_count == 1) %>%
  mutate(pair_key_mode = if_else(key == '', paste(key, mode, sep = ''), paste(key, mode, sep = ' '))) %>%
  group_by(pair_key_mode) %>%
  summarise(count_of_pair_key_mode_solo_artists = n()) %>%
  arrange(-count_of_pair_key_mode_solo_artists) %>%
  inner_join(tmp1, by = 'pair_key_mode')
rm(tmp1)

## Odp. G Major, Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
Sys.setlocale( 'LC_ALL','C' ) # gdyby nie działało
df %>% 
  separate_rows(artist.s._name, sep = ', ') %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(as.numeric(streams))) %>% # as.integer ma precyzję 32-bitową (wg dokumentacji) i przechowuje wartości +-2*10^9, a
  # as.numeric jako double mma precyzję 32-bitową i może przechowywać wartości do 2*10^308. Niektóre komórki w streams mają wartości niemieszczące się w integer.
  arrange(-total_streams) %>%
  head(1) %>% pull(1)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
Sys.setlocale( 'LC_ALL','C' ) # gdyby nie działało
tmp1 <- df %>%
  separate_rows(artist.s._name, sep = ', ') %>%
  group_by(artist.s._name) %>%
  summarise(debut_year = min(released_year)) %>%
  filter(debut_year == 2022)
df %>%
  separate_rows(artist.s._name, sep = ', ') %>%
  select(artist.s._name, released_year, key, mode) %>%
  inner_join(tmp1, by = 'artist.s._name') %>%
  group_by(artist.s._name, key, mode) %>%
  summarise(number_of_songs = n()) %>%
  pivot_wider(names_from = c(key, mode), values_from = number_of_songs, values_fill = 0) %>%
  View()
rm(tmp1)

## Odp. wynik tego, co jest wyżej

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts != 0) %>%
  select(track_name) %>%
  View()

## Odp. tabelka wynikowa powyższego kodu

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
Sys.setlocale( 'LC_ALL','C' ) # gdyby nie działało
tmp1 <- df %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(as.numeric(streams)), song_count = n(), average_streams_per_song_if_solo = sum(as.numeric(streams)) / n()) %>%
  select(artist.s._name, average_streams_per_song_if_solo)
tmp2 <- df %>%
  filter(artist_count > 1) %>%
  separate_rows(artist.s._name, sep = ', ') %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(as.numeric(streams)), song_count = n(), average_streams_per_song_if_group = sum(as.numeric(streams)) / n()) %>%
  select(artist.s._name, average_streams_per_song_if_group)
inner_join(tmp1, tmp2, by = 'artist.s._name') %>%
  filter(average_streams_per_song_if_solo > average_streams_per_song_if_group) %>%
  mutate(how_many_more_streams_if_solo_per_song_on_average = average_streams_per_song_if_solo - average_streams_per_song_if_group) %>%
  arrange(-how_many_more_streams_if_solo_per_song_on_average) %>% 
  select(artist.s._name, how_many_more_streams_if_solo_per_song_on_average) %>% pull(artist.s._name)
rm(tmp1, tmp2)

## Odp. "John Legend"     "J. Cole"         "Ed Sheeran"      "Juice WRLD"      "Manuel Turizo"   "Sam Smith"       "XXXTENTACION"   
#       "Lisa"            "Eminem"          "Bruno Mars"      "Willow"          "Imagine Dragons" "Kanye West"      "Cris Mj"        
#       "Chris Brown"     "Sleepy hallow"   "Anitta"          "Rauw Alejandro"  "JVKE"            "Playboi Carti"   "BoyWithUke"     
#       "Polo G"          "Lil Uzi Vert"    "Beyonco<bf>"     "Ryan Castro"     "Maroon 5"        "Taylor Swift"    "Sebastian Yatra"
#       "Justin Bieber"   "Kodak Black"     "Coi Leray"       "Eden Muo<bf>=o"  "Central Cee"     "Grupo Frontera"  "Ckay"           
#       "Nicki Minaj"     "LE SSERAFIM"     "Ariana Grande"   "Oliver Tree"     "Chino Pacas"     "Lana Del Rey"    "Stephen Sanchez"
#       "ROSALo<bf>="     "RM"              "Morgan Wallen"   "Carin Leon"      "Jimin"           "NLE Choppa"      "Muni Long"      
#       "Paulo Londra"    "Dave"            "j-hope"          "Doja Cat"        "Feid"            "Kendrick Lamar"  "Agust D"        
#       "Luciano"         "Lil Baby"