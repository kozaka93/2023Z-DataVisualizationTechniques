library(tidyr)
library(dplyr)

#df <- read.csv('C:/Users/domin/Documents/Studia/TWD/hw1/spotify-2023.csv')
df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month==1 | released_month==2 | released_month==3) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE))

## Odp. 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  mutate(artist1or2 = ifelse(artist_count==1 | artist_count==2, in_spotify_playlists, 0),
         artist_more = ifelse(artist_count>2, in_spotify_playlists, 0)) %>% 
  summarise(sum1or2 = sum(artist1or2), sum_more = sum(artist_more)) %>% 
  transmute(sum_more > sum1or2)

## Odp. Nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(release_date = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>% 
  group_by(release_date) %>% 
  summarise(count = n()) %>% 
  top_n(1) %>% 
  select(release_date)

## Odp. piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>% 
  filter(artist_count == 1, released_year == 2022 | released_year == 2021) %>% 
  mutate(released2021 = ifelse(released_year==2021, 1, 0),
         released2022 = ifelse(released_year==2022, 1, 0)) %>% 
  select(artist.s._name, released2021, released2022) %>% 
  group_by(artist.s._name) %>% 
  summarise(count2021 = sum(released2021), count2022 = sum(released2022)) %>% 
  filter(count2022!=0, count2021!=0) %>% 
  mutate(percentage = (count2022 / count2021 *100 )) %>% 
  top_n(1) %>% 
  select(artist.s._name)

## Odp. SZA


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  arrange(-danceability_.) %>% 
  head(0.1*nrow(df)) %>% 
  mutate(streams_per_year = as.numeric(streams)/(2023-released_year+1)) %>% 
  top_n(1, streams_per_year) %>% 
  select(artist.s._name)

## Odp.Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
table6 <- df %>% 
  mutate(streams_per_playlists = as.numeric(streams, na.rm=TRUE)/in_spotify_playlists) %>% 
  arrange(-streams_per_playlists) %>% 
  head(0.2*nrow(df))

mean(table6$bpm) 

table6 %>% 
  group_by(mode) %>% 
  summarise(count = n()) %>% 
  top_n(1, count) %>% 
  select(mode)

## Odp. charakteryzują się średnim tempem 125.2, a najczęściej występująca skala to Minor


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(pora_roku = case_when(3<=released_month & released_month<=5 ~ "wiosna",
                               6<=released_month & released_month<=8 ~ "lato",
                               9<=released_month & released_month<=11 ~ "jesien",
                               TRUE ~ "zima")) %>% 
  group_by(pora_roku) %>% 
  summarise(mean_dancebility = mean(danceability_.,),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.),
            mean_speechiness = mean(speechiness_.))

## Odp.
#pora_roku  mean_dancebility  mean_valence  mean_energy  mean_acousticness mean_instrumentalness  mean_liveness  mean_speechiness
#jesien                65.3         46.2        62.2              26.7                 2.02           18.0            10.2 
#lato                  69.2         51.2        65.8              23.7                 2.74           17.6             9.71
#wiosna                68.0         51.0        64.3              28.3                 1.4            18.4            11.0 
#zima                  65.7         56.1        64.7              28.5                 0.596          18.7             9.50


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year==2022) %>%
  mutate(if_single_artist = ifelse(artist_count==1, 1, 0)) %>% 
  group_by(key, mode) %>% 
  summarise(count = n(), sum_single = sum(if_single_artist)) %>%
  arrange(-count) %>% 
  head(10) %>% 
  arrange(-sum_single) %>% 
  head(1) %>% 
  select(key, mode)

## Odp.G major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  top_n(1,sum_streams) %>% 
  select(artist.s._name)

## Odp. The Weekend


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
debiutanci2022<- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(debiut = min(released_year)) %>%
  filter(debiut == 2022) %>% 
  select(artist.s._name) %>% 
  pull(artist.s._name)

debiutanci_key_mode <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8")) %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name %in% debiutanci2022) %>% 
  group_by(artist.s._name, mode, key) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = c(mode, key), values_from = count, values_fill = 0)

## Odp.zestawienie w debiutanci_key_mode


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
table11 <- df %>% 
  filter(in_spotify_playlists >in_apple_playlists, in_spotify_charts==0, in_apple_charts>0) %>% 
  select(track_name, artist.s._name)

## Odp.Zestawienie tych piosenek jest w table11 (takich piosenek jest 337)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
table12 <- df %>% 
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8"),
         if_solo = ifelse(artist_count==1, 'solo', 'more_than_1')) %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name, if_solo) %>% 
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>% 
  pivot_wider(names_from = if_solo, values_from = mean_streams, values_fill = 0) %>% 
  filter(solo > more_than_1, solo!=0, more_than_1!=0) %>% 
  select(artist.s._name)

## Odp. lista tych artystów jest w zestawieniu table12 (jest ich 55)




