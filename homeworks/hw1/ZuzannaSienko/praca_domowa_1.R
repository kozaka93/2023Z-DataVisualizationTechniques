library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')

#porozdzielane multi artists utwory na pojedynczych artystow
df_filtered_artist<- df %>% 
  mutate(artist.s._name = str_split(artist.s._name, ", ")) %>% 
  unnest(artist.s._name)%>% 
  relocate(artist.s._name,.before=track_name)

#--------------------------------------------------------------------------------------------------

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

mean_streams_2023<-df %>% 
  filter(released_year==2023 & released_month<4) %>% 
  summarise(mean=mean(as.numeric(streams),na.rm=TRUE))
  
## Odp. 216 150 568

#--------------------------------------------------------------------------------------------------

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

single_multiple_artists_songs<-df %>% 
  mutate(category=ifelse(artist_count<=2,"single/duet","multiple")) %>% 
  group_by(category) %>% 
  summarise(sum(in_spotify_playlists,na.rm=TRUE))

## Odp. nie, piosenki stworzone przez 1/2 artystow sa zawarte w wiekszej ilosci playlist spotify.

#-------------------------------------------------------------------------------------------------

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

most_popular_day<-df %>% 
  mutate(day_of_week=weekdays(as.Date(paste(released_year,released_month,released_day,sep="-")))) %>% 
  group_by(day_of_week) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)

## Odp. piątek

#------------------------------------------------------------------------------------------------

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df_songs_released_per <- df %>%
  filter(artist_count==1) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(count=n())%>%
  pivot_wider(names_from=released_year,values_from=count,values_fill=0)%>% 
  select(artist.s._name,`2021`,`2022`) %>% 
  filter(`2021`>=1&`2022`>=1) %>%
  mutate(increase_per=(`2022`-`2021`)/`2021`*100) %>% 
  ungroup() %>% 
  top_n(1,increase_per)
  
## Odp. SZA

#------------------------------------------------------------------------------------------------

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

dance_songs<-df %>% 
  top_frac(0.1,danceability_.) %>% 
  summarise(artist_name=artist.s._name,
    mean_streams_year=(as.numeric(streams,na.rm=TRUE))/(2024-released_year)) %>%
  top_n(1,mean_streams_year)

## Odp.Chencho Corleone, Bad Bunny

#------------------------------------------------------------------------------------------------

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_streamed_songs_per_spotify_playlist<-df %>% 
  mutate(streams_per_spotify_playlist=as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(desc(streams_per_spotify_playlist)) %>% 
  top_frac(0.2)

mean_bpm_mode<-top_streamed_songs_per_spotify_playlist %>% 
  summarise(mean_bpm=mean(bpm,na.rm=TRUE),
            most_popular_mode=names(which.max(table(mode))))

## Odp. srednie bpm=125.2, mode=Minor

#------------------------------------------------------------------------------------------------

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df_seasons<-df %>% 
  mutate(season = case_when(
    (released_month==3 & released_day>=21) | released_month %in% c(4,5) | (released_month==6 & released_day<=22) ~ "Spring",
    (released_month==6 & released_day>22) | released_month %in% c(7,8) | (released_month==9 & released_day<23) ~ "Summer",
    (released_month==9 & released_day>=23) | released_month %in% c(10,11) | (released_month==12 & released_day<22) ~ "Autumn",
    TRUE ~ "Winter")) %>%
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_., na.rm = TRUE),
    mean_valence = mean(valence_., na.rm = TRUE),
    mean_energy = mean(energy_., na.rm = TRUE),
    mean_acousticness = mean(acousticness_., na.rm = TRUE),
    mean_instrumentalness = mean(instrumentalness_., na.rm = TRUE),
    mean_liveness = mean(liveness_., na.rm = TRUE),
    mean_speechiness = mean(speechiness_., na.rm = TRUE))

## Odp. wiosna: tanecznosc = 68.7, pozytywnosc = 49.9, energia = 64.4,.......
##      lato: tanecznosc = 68.4, pozytywnosc = 50.9, energia = 65.6,.......
##      jesien: tanecznosc = 63.9, pozytywnosc = 47.0, energia = 61.2,.......
##      zima: tanecznosc = 66.7, pozytywnosc = 57.5, energia = 66.0,.......

#------------------------------------------------------------------------------------------------

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

key_mode_pairs<-df %>% 
  filter(released_year==2022) %>% 
  group_by(key,mode) %>% 
  summarise(count=n(),
            solo_artist_count=sum(artist_count==1)) %>% 
  ungroup() %>% 
  top_n(10,count) %>% 
  top_n(1,solo_artist_count)

## Odp. key: G, mode: Major

#------------------------------------------------------------------------------------------------

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

#czy interesuja nas te featured????????/

most_streamed_artist<- df_filtered_artist%>% 
  filter(artist_count==1) %>% 
  group_by(artist.s._name) %>% 
  summarise(stream_sum=sum(as.numeric(streams),na.rm=TRUE)) %>%
  top_n(1,stream_sum)

## Odp. The Weeknd  

#------------------------------------------------------------------------------------------------

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

#Zakladam, ze debiut nie tylko solowy, moze tez byc feat!!

df_2022_debut<-df_filtered_artist %>% 
  group_by(artist.s._name) %>% 
  mutate(min_released_year=min(released_year)) %>% 
  filter(min_released_year==2022)

df_2022_debut_key_mode<-df_2022_debut %>% 
  select(artist.s._name,mode,key) %>% 
  group_by(artist.s._name, mode, key) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(mode, key), values_from = count, values_fill = 0)
  
## Odp. df_2022_debut_key_mode

#------------------------------------------------------------------------------------------------

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df_spotify_apple<-df %>% 
  filter(in_spotify_playlists>in_apple_playlists,
         in_spotify_charts==0,
         in_apple_charts>0)

## Odp. Hits Different, All The Way Live (Spider-Man: Across the Spider-Verse),Karma (feat. Ice Spice),AMERICA HAS A PROBLEM (feat. Kendrick Lamar) itd....

#------------------------------------------------------------------------------------------------

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

streams_per_song_solo_featured<-df_filtered_artist %>% 
  group_by(artist.s._name, solo_featured = ifelse(artist_count==1, 'solo', 'featured')) %>% 
  summarise(mean_streams=mean(as.numeric(streams), na.rm=TRUE)) %>% 
  pivot_wider(names_from=solo_featured, values_from=mean_streams, values_fill=0) %>%  
  mutate(difference = solo-featured) %>% 
  filter(difference>0) %>% 
  arrange(desc(difference)) %>% 
  View()

## Odp. Tones and I, Glass Animals, Queen, Hozier, Vance Joy, John Legend,...........

#------------------------------------------------------------------------------------------------

