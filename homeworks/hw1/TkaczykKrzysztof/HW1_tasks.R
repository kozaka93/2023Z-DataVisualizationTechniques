library(tidyr)
library(dplyr)


df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter( released_year == 2023 & released_month < 4) %>% 
  summarise(mean_streams = mean(as.double(streams)))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  mutate(artist_count = ifelse(artist_count > 2,"MoreThan2", "OneOrTwo")) %>% 
  group_by(artist_count) %>% 
  summarise(in_spotify_playlists = sum(in_spotify_playlists)) %>% 
  as.data.frame() -> df1

df1[df1$artist_count == "MoreThan2", 
    "in_spotify_playlists"] > df1[df1$artist_count == "OneOrTwo", 
                                  "in_spotify_playlists"]

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  transmute(released_date = as.Date(paste(released_day,
                                       released_month,
                                       released_year, sep = "/"), "%e/%m/%Y"))  %>%
  transmute(day_of_week = strftime(released_date, "%A")) %>% 
  group_by(day_of_week) %>% 
  summarise(total_releases = n()) %>% 
  arrange(-total_releases) -> df1

df1[1,]

## Odp. Piątek 526 piosenek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count == 1 & released_year == 2021) %>% 
  select(artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_releases2021 = n()) %>% 
  as.data.frame()-> df2021

df %>% 
  filter(artist_count == 1 & released_year == 2022) %>% 
  select(artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_releases2022 = n()) %>% 
  as.data.frame() -> df2022

merge.data.frame(df2021,df2022, by = "artist.s._name" ) %>% 
  mutate(increase_pct = (total_releases2022 - total_releases2021)*100/total_releases2021) %>% 
  arrange(-increase_pct) %>% 
  head(1)

## Odp. SZA 1600%

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  head(as.integer(0.1*length(df$track_name))) %>% 
  mutate(elapsed_time = ISOdate(2023,12,31) - ISOdate(released_year,
                                                      released_month,
                                                      released_day)) %>% 
  mutate(elapsed_time = ifelse(elapsed_time < 1, 1, elapsed_time)) %>% 
  mutate(elapsed_time = as.double(elapsed_time)/365,
         streams_per_year = as.double(streams)/elapsed_time) %>% 
  select(track_name,artist.s._name,streams_per_year) %>% 
  arrange(-streams_per_year) %>% 
  head(1)

## Odp. Me Porto Bonito; Checo Corleone, Bad Bunny; 870656628 odtworzeń/rok

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams_per_playlist = as.double(streams)/in_spotify_playlists) %>% 
  arrange(-streams_per_playlist) %>% 
  head(0.2*length(df$track_name)) -> dfTop20

dfTop20 %>% 
  group_by(mode) %>% 
  summarise(mode_freq = n()) %>% 
  arrange(-mode_freq) %>% 
  head(1)

dfTop20 %>% 
  summarise(mean_bpm = mean(bpm))

## Odp. Minor: 96 utworów, średnie BPM = 125.2

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(released_date = ISOdate(released_year,
                                    released_month,
                                    released_day),
         season = case_when(released_date < ISOdate(released_year, 3, 21) |
                              released_date >= ISOdate(released_year, 12, 22) ~ "WINTER",
                            released_date >= ISOdate(released_year, 3, 21) &
                              released_date < ISOdate(released_year, 6, 22) ~ "SPRING",
                            released_date >= ISOdate(released_year, 6, 22) &
                              released_date < ISOdate(released_year, 9, 23) ~ "SUMMER",
                            TRUE ~ "AUTUMN")) %>%
  group_by(season) %>%
  summarise(danceability_. = mean(danceability_.),
            energy_. = mean(energy_.),
            valence_. = mean(valence_.),
            acousticness_. = mean(acousticness_.),
            instrumentalness_. = mean(instrumentalness_.),
            liveness_. = mean(liveness_.),
            speechiness_. = mean(speechiness_.)) %>%
  View()

## Odp. 
#season danceability_. energy_. valence_. acousticness_. instrumentalness_. liveness_. speechiness_.
#<chr>           <dbl>    <dbl>     <dbl>          <dbl>              <dbl>      <dbl>         <dbl>
#1 AUTUMN           63.9     61.2      47.0           29.7              1.96        17.6          9.84
#2 SPRING           68.6     64.2      49.5           27.7              1.67        18.3         11.0 
#3 SUMMER           68.6     65.8      51.5           24.7              2.17        18.0         10   
#4 WINTER           66.7     66.0      57.5           25.7              0.730       18.7          9.56
 

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  group_by(key,mode) %>% 
  summarise(freq = n(),
            solo_artists = sum(artist_count == 1)) %>% 
  arrange(-freq) %>% 
  head(10) %>% 
  arrange(-solo_artists) %>% 
  head(1) %>% 
  View()

## Odp. key: "G", mode: Major, 25 artystów, 33 wystąpień

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.double(streams))) %>% 
  arrange(-total_streams)  %>% 
  head(1) %>% 
  View()

## Odp. The Weekend 23929760757

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
## Odp.


df %>% 
  filter(artist_count  == 1) %>%
  group_by(artist.s._name) %>% 
  summarise(debut_year = min(released_year)) %>% 
  filter(debut_year == 2022 ) -> dfDebutes2022

df %>% 
  filter(is.element(artist.s._name,dfDebutes2022$artist.s._name)) %>% 
  group_by(artist.s._name, mode,key) %>% 
  summarise(songs = n()) %>% 
  pivot_wider(names_from = c(mode,key), values_from = songs, values_fill = 0) %>% 
  View()


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
## Odp.

df %>% 
  filter(in_spotify_playlists > in_apple_playlists
         & in_spotify_charts == 0 
         & in_apple_charts != 0) %>%
  View()


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
## Odp.

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  mutate(artist_count = ifelse(artist_count == 1, "SOLO", "TEAM")) %>% 
  group_by(artist.s._name, artist_count) %>% 
  summarise(total = sum(as.double(streams)),
            songs = n()) %>% 
  mutate(mean_streams = total/songs) %>% 
  select(artist.s._name,artist_count,mean_streams) %>% 
  pivot_wider(names_from = artist_count, values_from = mean_streams, values_fill = 0) %>% 
  filter(SOLO > TEAM & TEAM != 0) %>% 
  View()
