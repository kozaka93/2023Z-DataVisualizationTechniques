library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>%
  filter(released_year==2023 & released_month %in% c(1,2,3))%>%
  mutate(streams_num=as.numeric(streams))%>% 
  summarise(mean_stream=mean(streams_num, na.rm = TRUE))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  filter(artist_count<=2) %>%
  summarise(sum_playlist_spotify=sum(in_spotify_playlists, na.rm=TRUE))

df %>%
  filter(artist_count>2) %>% 
  summarise(sum_playlist_spotify=sum(in_spotify_playlists, na.rm=TRUE))

## Odp. Nie, stw. przez 1 lub 2: 4527593, przez więcej: 428126


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%
  mutate(days_week= weekdays(as.Date(paste(released_year, released_month,
                                   released_day, sep = "-")))) %>%
  group_by(days_week) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(1)

## Odp. piątek, liczba piosenek: 526

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>%
  filter(released_year %in% c(2021,2022), artist_count==1)%>%
  group_by(artist.s._name, released_year) %>%
  summarise(n=n()) %>% 
  pivot_wider(names_from=released_year, values_from=n) %>%
  mutate(per=`2022`/`2021`) %>%
  arrange(desc(per))%>%
  head(1)

## Odp. SZA, 1700% wzrost

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(desc(danceability_.)) %>%
  head(nrow(df)*0.1) %>%
  mutate(per_year=as.numeric(streams)/(2024-released_year)) %>%
  arrange(desc(per_year)) %>%
  head(1)

## Odp.Chencho Corleone, Bad Bunny , streams per year: 720378909

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams_per_playlist=as.numeric(streams)/in_spotify_playlists) %>%
  arrange(desc(streams_per_playlist)) %>%
  head(round(nrow(df)*0.2)) %>% 
  group_by(mode) %>%
  summarise(n=n())

df %>% 
  mutate(streams_per_playlist=as.numeric(streams)/in_spotify_playlists) %>%
  arrange(desc(streams_per_playlist))%>%
  head(round(nrow(df)*0.2)) %>% 
  summarise(mean_temp=mean(bpm, na.rm=TRUE))

## Odp. Mode: Minor, średni temp: 125.2775

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

season <- df %>%
  mutate(season = case_when(released_month %in% c(1,2) | (released_month==12 & released_day >21) | (released_month==3 & released_day<21) ~ "winter",
    released_month %in% c(4,5) | (released_month==3 & released_day >20) | (released_month==6 & released_day<21) ~ "spring",
    released_month %in% c(7,8) | (released_month==6 & released_day >20) | (released_month==9 & released_day<23) ~ "summer",
    T ~ "autumn")) %>% 
  select(season, danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  group_by(season) %>% 
  summarise(mean_dancebility = mean(danceability_.,  rm.na = TRUE),
            mean_valence = mean(valence_.,  rm.na = TRUE),
            mean_energy = mean(energy_.,  rm.na = TRUE),
            mean_acousticness = mean(acousticness_.,  rm.na = TRUE),
            mean_instrumentalness = mean(instrumentalness_.,  rm.na = TRUE),
            mean_liveness = mean(liveness_.,  rm.na = TRUE),
            mean_speechiness = mean(speechiness_.,  rm.na = TRUE))

## Odp. season data
                     

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>%
  filter(released_year==2022) %>%
  group_by(mode, key) %>%
  summarise(n=n())%>%
  arrange(desc(n)) %>%
  head(10)

df %>%
  filter(released_year==2022 & artist_count==1)%>%
  group_by(mode, key) %>%
  summarise(n=n())%>%
  arrange(desc(n)) %>%
  head(10)

## Odp. Major "G" 

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>%
  group_by(artist.s._name) %>%
  summarise(streams_artists=sum(as.numeric(streams))) %>%
  arrange(desc(streams_artists)) %>% 
  head(1)

df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>%
  group_by(artist.s._name) %>%
  summarise(streams_artists=sum(as.numeric(streams), na.rm=TRUE)) %>%
  arrange(desc(streams_artists)) %>%
  head(1)


## Odp.The Weeknd  streams:  23929760757

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debute_2022 <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>%
  summarise(debute_year=min(released_year)) %>% 
  filter(debute_year==2022) 

ans_10 <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>% 
  filter(artist.s._name %in% debute_2022$artist.s._name) %>%
  group_by(artist.s._name, mode, key) %>%
  summarise(n=n()) %>% 
  pivot_wider(names_from = c(mode, key), values_from = n, values_fill=0)

## Odp. ans_10



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

ans_11 <- df %>%
  filter(in_spotify_playlists>in_apple_playlists & in_spotify_charts==0 & in_apple_charts>0) %>%
  select(track_name)

## Odp. ans_11


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

count_12_solo <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>%
  filter(artist_count==1) %>%
  summarise(solo_mean=mean(as.numeric(streams[artist_count==1]), na.rm = TRUE))

count_12_more <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>%
  filter(artist_count>1) %>%
  summarise(many_mean=mean(as.numeric(streams[artist_count>1]), na.rm = TRUE))

ans_12 <- left_join(count_12_solo, count_12_more, "artist.s._name") %>%
  select(artist.s._name, solo_mean, many_mean) %>%
  mutate(solo_mean = ifelse(is.na(solo_mean), 0, solo_mean),
         many_mean = ifelse(is.na(many_mean), 0, many_mean)) %>%
  filter(solo_mean>many_mean)


## Odp. ans_12









