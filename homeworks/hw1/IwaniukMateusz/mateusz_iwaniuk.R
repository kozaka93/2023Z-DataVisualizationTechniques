
#install.packages('tidyr')
# install.packages('dplyr')
# install.packages('lubridate')
library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')
head(df)


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
zad1 <- df %>%
  select(track_name,released_year,released_month,streams)%>%
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>%
  summarise(avg_streams = mean(streams))

## Odp.	
#216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

#nie ma sensu sumowac po playlisty moga byc wspolne, wiec przyrownuje srednio dla >2 i dla 1
zad_2_multi<- df%>%
  select(track_name,artist_count,in_spotify_playlists)%>%
  filter(artist_count>2)%>%
  summarise(avg_playlists = mean(in_spotify_playlists))

zad_2_solo<- df%>%
  select(track_name,artist_count,in_spotify_playlists)%>%
  filter(artist_count==1)%>%
  summarise(avg_playlists = mean(in_spotify_playlists))

## Odp.multi: 	
#3822.554
#solo:
#5807.611



#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
zad3<-  df %>%
  select(track_name,released_month,released_year,released_day)%>%
  mutate(weekday = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>%
  count(weekday) %>%
  arrange(desc(n)) 

## Odp.piątek
#526

#### 4.Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
zad4_a <- df %>%
  select(artist.s._name,artist_count,released_year) %>%
  filter(artist_count==1, released_year %in% c(2021, 2022))%>%
  group_by(artist.s._name, released_year)%>%
  summarise(n_songs = n()) %>%
  spread(released_year, n_songs) %>%
  filter(!is.na(`2021`), !is.na(`2022`)) %>%
  mutate(growth = (`2022` - `2021`) / `2021` * 100) %>%
  arrange(desc(growth))




## Odp. SZA 1600

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

zad5 <- df %>%
  select(track_name,artist.s._name, danceability_., streams,released_year) %>%
    filter(danceability_. >= quantile(danceability_., 0.9)) %>%
    mutate(avaliable_years=2024-released_year)%>%
  mutate(avg_streams_per_year = streams/avaliable_years) %>%
  arrange(desc(avg_streams_per_year)) 
## Odp.
# Me Porto Bonito
# Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

zad6<- df %>%
  select(bpm, mode, streams, in_spotify_playlists) %>%
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>%
  filter(streams_per_playlist >= quantile(streams_per_playlist, 0.8)) %>%
  summarise(avg_bpm = mean(bpm)) %>%
  bind_cols(
    df %>%
      select(mode, streams, in_spotify_playlists) %>%
      mutate(streams_per_playlist = streams / in_spotify_playlists) %>%
      filter(streams_per_playlist >= quantile(streams_per_playlist, 0.8)) %>%
      count(mode) %>%
      top_n(1, wt = n)
  )


## Odp.125.2775
#Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

zad7 <- df %>%
  select(released_month, danceability_., energy_.) %>%
  mutate(season = case_when(
    released_month %in% c(12, 1, 2) ~ 'Zima',
    released_month %in% c(3, 4, 5) ~ 'Wiosna',
    released_month %in% c(6, 7, 8) ~ 'LAto',
    TRUE ~ 'Jesien'
  )) %>%
  group_by(season) %>%
  summarise(
    avg_danceability = mean(danceability_.),
    avg_energy = mean(energy_.),
  )

## Odp. Za duze zestawienie aby zmiescic tutaj


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
zad8 <- df %>%
  select(released_year, artist_count, key, mode) %>%
  filter(released_year == 2022, artist_count == 1) %>%
  count(key, mode) %>%
  arrange(desc(n)) 

## Odp. G
#Major
#25

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
zad9 <-df %>% select(artist.s._name, streams) %>%
  mutate(artists = str_split(artist.s._name, ", ")) %>%
  unnest(artists) %>%
  select(artists, streams) %>%
  group_by(artists) %>%
  summarise(total_streams = sum(as.numeric(streams))) %>%
  arrange(-total_streams)

## Odp.
# The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

zad10_dist <- df %>%
  select(artist.s._name,released_year) %>%
  filter(released_year<=2021)%>%
  select(artist.s._name)%>%
  distinct

zad10 <- df %>%
  select(artist.s._name,released_year,mode,key)%>%
  filter(released_year==2022,!(artist.s._name%in%zad10_dist))%>%
  group_by(artist.s._name,key,mode)%>%
  summarise(count=n()) %>%
  unite("key_mode", key, mode, sep = "_") %>%
  spread(key = key_mode, value =count, fill = 0)
  

## Odp. Odp. Za duze zestawienie aby zmiescic tutaj



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

zad11 <- df %>%
  select(track_name, in_spotify_playlists, in_apple_playlists, in_spotify_charts, in_apple_charts) %>%
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts==0 & in_apple_charts!=0)
## Odp. Odp. Za duze zestawienie aby zmiescic tutaj


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
## Odp.
zad12_multi <- df %>% filter(artist_count>1) %>%
  separate_rows(artist.s._name, sep = ', ') %>% 
  group_by(artist.s._name) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>%
  mutate(multi_mean= mean_streams) %>%
  select('artist.s._name', 'multi_mean')

zad12<-df %>% filter(artist_count==1) %>%
  group_by(artist.s._name) %>%
  summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>%
  mutate(single_mean = mean_streams) %>%
  select('artist.s._name', 'single_mean') %>%
  left_join(zad12_multi, by = 'artist.s._name' ) %>%
  filter(single_mean>multi_mean)

## Odp. Odp. Za duze zestawienie aby zmiescic tutaj

