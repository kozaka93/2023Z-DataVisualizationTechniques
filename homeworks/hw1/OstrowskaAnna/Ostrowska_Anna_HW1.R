library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df1 <- df%>% 
  filter(released_year==2023 & released_month %in% c(1,2,3)) 

result1 <- mean(as.integer(df1$streams))
result1
## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df2 <- df %>% 
  select(artist_count, in_spotify_playlists) %>% 
  mutate(artist_count=ifelse(artist_count>2, '>2', '<=2'))  %>% 
  group_by(artist_count) %>% 
  summarise(playlists_count = sum(in_spotify_playlists))

df2 %>% 
  filter(artist_count=='>2') %>% 
  select(playlists_count)>
  df2 %>% 
  filter(artist_count=='<=2') %>% 
  select(playlists_count)

## Odp. nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df3 <- df %>% 
  mutate(date = paste0(released_month, '/', released_day, '/', released_year)) %>% 
  mutate(weekday = strftime(as.Date(date, "%m/%d/%Y"), "%A"))

df3 %>% 
  group_by(weekday) %>% 
  summarise(release_count = n()) %>% 
  top_n(1,release_count) %>% 
  select(weekday)
  

## Odp.piatek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(artist_count==1 & released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name) %>% 
  summarise(count_2021 = sum(released_year == 2021),
            count_2022 = sum(released_year == 2022)) %>% 
  filter(count_2021>0 & count_2022>0) %>% 
  mutate(wzrost_procentowy = ((count_2022 - count_2021) / count_2021) * 100) %>% 
  top_n(1,wzrost_procentowy) %>% 
  select(artist.s._name)


  
## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  head(nrow(df)*0.1) %>% 
  mutate(mean_streams_per_year = as.integer(streams)/(2023-as.integer(released_year)+1)) %>% 
  top_n(1, mean_streams_per_year) %>% 
  select(artist.s._name)

## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df6 <- df %>% 
  filter(!is.na(streams) & in_spotify_playlists != 0) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  mutate(most_streams_per_playlist = streams/in_spotify_playlists) %>% 
  arrange(-most_streams_per_playlist) %>% 
  head(nrow(df)*0.2) 

mean(df6$bpm)

df6 %>% 
  group_by(mode) %>% 
  summarise(count_mode = n()) %>% 
  top_n(1, count_mode) %>% 
  select(mode)



## Odp. srednie tempo: 125.2, najczesciej wystepujaca skala: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
install.packages("hydroTSM")    #pakiety, dzieki ktorym latwiej bedzie stwierdzic pore roku                   
library("hydroTSM")

df %>% 
  mutate(date = as.Date(paste0(released_month, '/', released_day, '/', released_year), "%m/%d/%Y")) %>% 
  mutate(season = time2season(date, out.fmt = "seasons")) %>% 
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.), mean_valence = mean(valence_.), mean_energy=mean(energy_.), mean_acousticness=mean(acousticness_.), mean_intrumentalness=mean(instrumentalness_.), mean_liveness=mean(liveness_.), mean_speechiness=mean(speechiness_.))

## Odp. średnie wybranych cech piosenek dla każdej pory roku:
##season mean_danceability mean_valence mean_energy mean_acousticness mean_intrumentalness mean_liveness
                                   
##autumm         65.3         46.2        62.2              26.7                2.02           18.0
##spring         68.0         51.0        64.3              28.3                1.4            18.4
##summer         69.2         51.2        65.8              23.7                2.74           17.6
##winter         65.7         56.1        64.7              28.5                0.596          18.7

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
#1. liczac key=""
most_popular_key_mode <- df %>% 
  filter(released_year==2022) %>% 
  group_by(key, mode) %>% 
  summarize(count=n()) %>% 
  mutate(key_mode=paste(key,mode)) %>% 
  arrange(-count) %>% 
  head(10)

df %>% 
  filter(artist_count==1) %>% 
  filter(paste(key, mode) %in% most_popular_key_mode$key_mode) %>% 
  group_by(key,mode) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% 
  head(1) %>% 
  select(key,mode)

#2. nie liczac key=""

most_popular_key_mode2 <- df %>% 
  filter(released_year==2022, key!="") %>% 
  group_by(key, mode) %>% 
  summarize(count=n()) %>% 
  mutate(key_mode=paste(key,mode)) %>% 
  arrange(-count) %>% 
  head(10)

df %>% 
  filter(artist_count==1) %>% 
  filter(paste(key, mode) %in% most_popular_key_mode2$key_mode) %>% 
  group_by(key,mode) %>% 
  summarize(count=n()) %>% 
  arrange(-count) %>% 
  head(1) %>% 
  select(key,mode)
## Odp.licząc puste wartosci w kolumnie 'key':"" Major, nie liczac pustych wartosci "key": G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
library(stringr)

df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  select(streams, artist) %>% 
  unnest(artist) %>% 
  group_by(artist) %>% 
  summarise(streams_sum = sum(as.numeric(streams), na.rm=T)) %>% 
  top_n(1,streams_sum) %>% 
  select(artist)


## Odp.The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debut_2022 <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>% 
  group_by(artist) %>% 
  summarise(debut = min(released_year)) %>%
  filter(debut==2022) %>% 
  pull(artist)

key_count <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>% 
  filter(artist %in% debut_2022) %>% 
  group_by(artist, key) %>%
  summarise(title_count = n()) %>% 
  mutate(key=ifelse(key=='', ' ', key)) %>% 
  pivot_wider(names_from=key, values_from=title_count, values_fill=0)
  
mode_count <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>% 
  filter(artist %in% debut_2022) %>% 
  group_by(artist, mode) %>%
  summarise(title_count = n()) %>% 
  pivot_wider(names_from=mode, values_from=title_count, values_fill=0)

## Odp.:
View(full_join(key_count, mode_count, by='artist'))


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df11 <- df %>% 
  filter(in_spotify_playlists>in_apple_playlists) %>% 
  filter(in_spotify_charts==0 & in_apple_charts>0) %>% 
  select(track_name)

## Odp.:
View(df11)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
solo_count <- df %>% 
  filter(artist_count==1) %>%
  mutate(artist=artist.s._name) %>% 
  group_by(artist) %>% 
  summarise(mean_solo_streams = mean(as.numeric(streams), na.rm = TRUE)) 

group_count <- df %>% 
  filter(artist_count>1) %>%
  mutate(artist=str_split(artist.s._name, pattern = ", "))%>% 
  unnest(artist) %>%
  group_by(artist) %>% 
  summarise(mean_group_streams = mean(as.numeric(streams), na.rm = TRUE)) 

result <- inner_join(solo_count, group_count, by='artist') %>% 
  filter(mean_solo_streams>mean_group_streams) %>% 
  select(artist)
## Odp.:
View(result)



