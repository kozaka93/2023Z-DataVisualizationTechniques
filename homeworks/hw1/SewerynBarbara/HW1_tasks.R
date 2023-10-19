library(tidyr)
library(dplyr)
setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych\\hw1")
df <- read.csv('spotify-2023.csv')

df <- df[-575,] # w wierszu nr 575 jest blad, ktory wywoluje mnostwo bledow i
#ostrzerzen w kolejnych zadaniach

df$streams<- as.numeric(df$streams) # kolumna streams jest typem character
# a bede na niej wykonywac operacje arytmetyczne, wiec trzeba ja zmienic na numeryczna


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% filter(released_year == 2023 & released_month %in% c(1,2,3))  %>%
  summarise(mean_streams = mean(streams, na.rm = T))

## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% filter(artist_count > 2) %>%
  summarise(sum = sum(in_spotify_playlists, na.rm = T)) -> x #ile piosenek tworzomych przez >2 artystow jest na playliscie
df %>% filter(artist_count %in% c(1,2)) %>%
  summarise(sum = sum(in_spotify_playlists, na.rm = T)) -> y #ile piosenek tworzomych przez 1 lub 2 artystow jest na playliscie
x > y

## Odp.NIE


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

library(lubridate) # do wyznaczania dnia tygodnia

df %>% transmute('date' = make_date(year = released_year, month = released_month, day = released_day))  ->df1
x <- as.Date(df1$date)
sort(table(wday(x, week_start = 1)), decreasing = T)[1]

## Odp.PIATEK

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df %>%  filter(released_year == 2021, artist_count == 1) %>%
  select(artist.s._name, released_year) %>% group_by(artist.s._name) %>% 
  summarise(cnt_2021 = n())-> a
df %>%  filter(released_year == 2022, artist_count == 1) %>%
  select(artist.s._name, released_year) %>% group_by(artist.s._name) %>% 
  summarise(cnt_2022 = n())-> b
inner_join(a,b) %>% 
  mutate(increase_percentage = 100*(cnt_2022/cnt_2021)) %>%
  arrange(-increase_percentage) %>% head(1) %>% select(artist.s._name)

## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
h <- as.integer(nrow(df)/10) # 10% piosenek w daych
current_year = 2024
df %>% arrange(-danceability_.) %>% head(h) %>% 
  mutate(age = current_year - released_year) %>%
  select(track_name, artist.s._name, age, streams) %>%
  mutate(streams_per_year = streams/age) %>%
  arrange(-streams_per_year) %>% head(1) %>% select(artist.s._name)

## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
h2 <- as.integer(nrow(df)/5)

df %>% filter(!is.na(in_spotify_playlists) & in_spotify_playlists != 0) %>%
  mutate(streams_per_spotify_playlists = streams/in_spotify_playlists) %>%
  arrange(-streams_per_spotify_playlists) %>% head(h2) %>% select(bpm, mode) -> e
mean(e$bpm)
sort(table(e$mode), decreasing = T)[1]


## Odp.srednia: 125.2, najczestsza skala mode: minor - 96

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% mutate(season = 
                case_when((released_month == 4 | 
                          released_month == 5 | 
                          (released_month == 6 & released_day < 22) |
                          (released_month == 3 & released_day >=21)) ~ 'spring',
              (released_month == 7 | 
              released_month == 8 | 
              (released_month == 6 & released_day >= 22) |
              (released_month == 9 & released_day < 23)) ~ 'summer',
        (released_month == 10 | 
        released_month == 11 |
        (released_month == 12 & released_day < 22) |
        (released_month == 9 & released_day >= 23)) ~ 'fall', T ~ 'winter')) %>%
  select(season, 18:24) %>% group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.), mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.), 
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.), mean_speechiness = mean(speechiness_.))


## Odp.
# season mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechiness
# fall                63.9         47.0        61.2              29.7                 1.96           17.6             9.84
# spring              68.6         49.5        64.2              27.7                 1.67           18.3            11.0 
# summer              68.6         51.5        65.8              24.7                 2.17           18.0            10   
# winter              66.7         57.5        66.0              25.7                 0.730          18.7             9.56

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% filter(released_year == 2022) %>% 
  select(key, mode, artist_count) %>%
  group_by(key, mode) %>%
  summarise(cnt = n(),
            solo = sum(ifelse(artist_count == 1,1,0)))%>%
  arrange(-cnt) %>%
  head(10)  %>% arrange(-solo) %>% head(1) %>% select(key,mode)


## Odp.G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
# df %>% filter(artist_count == 1) %>% select(artist.s._name, streams) %>%
#   group_by(artist.s._name) %>% 
#   summarise(sum_streams = sum(as.integer(streams))) %>% 
#   arrange(-sum_streams) %>% head(1) %>% select(artist.s._name)

df %>% separate_longer_delim(artist.s._name,", ") %>%
  group_by(artist.s._name) %>%
  summarise(stream_sum = sum(streams)) %>%
  arrange(desc(stream_sum)) %>% head(1)


## Odp.The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

separate_longer_delim(df, artist.s._name, ", ") -> df_artists_sep

df_artists_sep %>% select(artist.s._name, released_year) %>%
  group_by(artist.s._name) %>% summarise(debut = min(released_year)) %>%
  filter(debut == 2022)-> pom

pom1 <- pom$artist.s._name

df_artists_sep %>% 
  filter(artist.s._name %in% pom1) %>% select(artist.s._name, mode, key) -> df10

df10 %>% select(-key) %>% table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = mode, values_from = Freq) -> mode_stats
  

df10 %>% select(-mode) %>% table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = key, values_from = Freq, names_prefix = "_") -> key_stats

merge(mode_stats, key_stats) -> res10
res10
 
## Odp. res10 (wyżej)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
 

df %>% filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts != 0) 

## Odp. tych artystow jest 337

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df_artists_sep %>% select(artist.s._name, artist_count, streams) %>%
  mutate(solo = ifelse(artist_count == 1, "solo", "not_solo")) %>%
  group_by(artist.s._name, solo) %>% summarise(streams_mean = mean(streams, na.rm=T)) %>%
  pivot_wider(names_from = solo, values_from = streams_mean, values_fill = 0) %>%
  filter(not_solo < solo)


## Odp. tych artystow jest 259


