library(tidyr)
install.packages("tidyr")
library(rlang)
install.packages("rlang")
library(dplyr)
install.packages("lubridate")
library(lubridate)
df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
tab_1<-filter(df,released_year==2023 & released_month<=3)
tab_1$streams<-as.numeric(tab_1$streams)
tab_1 %>% summarise(mean_streams=mean(streams, na.rm=TRUE)) %>% select(mean_streams)

## Odp. średnia liczba odtworzeń:  216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?


songs_1_2<- df %>%
  filter(artist_count<=2) %>%
  summarise(sum_in_playlists=sum(in_spotify_playlists,na.rm = TRUE))
songs_3_more<- df %>%
  filter(artist_count>=3) %>%
  summarise(sum_in_playlists=sum(in_spotify_playlists,na.rm = TRUE))
songs_1_2<songs_3_more
## Odp. Nie są.

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df_3<-df
df_3 %>%
  mutate(date=make_date(year=released_year,month=released_month,day=released_day)) %>% 
  mutate(released_day_of_week=wday(date, week_start = 1)) %>% 
  group_by(released_day_of_week) %>%
  summarise(realesed_count=n()) %>%
  top_n(1,realesed_count)
## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df_4a<-df %>% filter(artist_count==1 & released_year==2021) %>%
  group_by(artist.s._name) %>% 
  summarise(songs_in_2021=n())
df_4b<-df %>% filter(artist_count==1 & released_year==2022) %>%
  group_by(artist.s._name) %>% 
  summarise(songs_in_2022=n())
df_4<-merge(df_4a,df_4b,by="artist.s._name")
df_4<-df_4 %>% 
  mutate(wzrost_proc=((songs_in_2022-songs_in_2021)/songs_in_2021)*100) %>% 
  top_n(1,wzrost_proc) %>% 
  select(artist.s._name)
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df_5<-df
df_5$streams<-as.numeric(df_5$streams)
df_5 %>%
  top_frac(0.1,danceability_.) %>%
  mutate(years_available=2023-released_year+1) %>% 
  mutate(mean_streams_yearly=streams/years_available) %>% 
  arrange(-mean_streams_yearly) %>% 
  head(1) %>% 
  select(artist.s._name)
## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df_6<-df
df_6$streams<-as.numeric(df_6$streams)
df_6<-df_6 %>% 
  mutate(streams_per_playlists=streams/in_spotify_playlists) %>% 
  top_frac(0.2,streams_per_playlists) 
df_6%>% 
  summarise(mean_bpm=mean(bpm))
df_6 %>% 
  group_by(mode) %>% 
  summarise(mode_count=n()) %>% 
  top_n(1,mode_count)

## Odp.Średnie tempo: 125.2 , najczęściej występująca skala: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df_seasons<-df
df_seasons<- df_seasons%>% 
  mutate(season= case_when(
    (released_month==3 & released_day>22) ~"spring",
    (released_month==4 |released_month==5) ~"spring",
    (released_month==6 & released_day<22) ~"spring",
    (released_month==6 & released_day>=22) ~"summer",
    (released_month==7 | released_month==8) ~"summer",
    (released_month==9 & released_day<23) ~"summer",
    (released_month==9 & released_day>22) ~"autumn",
    (released_month==10 | released_month==11) ~"autumn",
    (released_month==12 & released_day<22) ~"autumn",
    TRUE ~ "winter"
  )) %>%
  group_by(season) %>% 
  summarise(mean_danceability=mean(danceability_.,na.rm = TRUE),mean_valence=mean(valence_.,na.rm = TRUE),mean_energy=mean(energy_.,na.rm = TRUE),mean_acousticness=mean(acousticness_.,na.rm = TRUE),mean_instrumentalness=mean(instrumentalness_.,na.rm = TRUE),mean_liveness=mean(liveness_.,na.rm = TRUE),mean_speechness=mean(speechiness_.,na.rm=TRUE)) 
 

## Odp. season mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechness
##      autumn        63.9         47.0        61.2              29.7                 1.96            17.6            9.84
##      spring        68.7         49.5        64.2              27.7                 1.52            18.4           10.9
##      summer        68.6         51.5        65.8              24.7                 2.17            18.0           10
##      winter        66.6         57.4        65.9              25.7                 0.900           18.7            9.61

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df_8_1<-df %>%
  mutate(key_mode=paste(key,mode,sep=" ")) %>%
  group_by(key_mode) %>%
  summarise(artist_count= n())
df_8_2<-df %>%
  filter(artist_count==1) %>% 
  mutate(key_mode=paste(key,mode,sep=" ")) %>%
  group_by(key_mode) %>%
  summarise(solo_artist_count= n())  
df_8<-merge(df_8_1,df_8_2,by="key_mode")
df_8 %>% 
  top_n(10,artist_count) %>% 
  arrange(-solo_artist_count) %>% 
  head(1) %>% 
  select(key_mode)
## Odp.key="" mode=Major.

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df_9<-df
df_9$streams<-as.numeric(df_9$streams)
df_9 %>%
  separate_longer_delim(artist.s._name,delim=",") %>% 
  group_by(artist.s._name) %>% 
  summarise(number_of_streams=sum(streams)) %>% 
  top_n(1,number_of_streams)



## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
artist_debut<-df %>% 
  separate_longer_delim(artist.s._name,delim=", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(debut_year=min(released_year),key_mode=paste(mode,key,sep=" ")) %>% 
  filter(debut_year==2022) %>% 
  group_by(artist.s._name,key_mode) %>% 
  summarise(key_mode_num=n()) %>% 
  pivot_wider(names_from=key_mode,values_from = key_mode_num,values_fill =0)

 

## Odp. Zbyt dugie zestawienie, by je tu umieścić



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>% 
  filter(in_spotify_playlists>in_apple_playlists & in_spotify_charts==0 & in_apple_charts>0) %>% 
  select(track_name)


## Odp. Uzyskane zestawienie jest zbyt duże, żeby je tu wkleić.


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df_12<-df
df_12$streams<-as.numeric(df_12$streams)
solo_mean_streams<-df_12 %>% 
  filter(artist_count==1) %>% 
  group_by(artist.s._name) %>% 
  summarise(solo_streams=sum(streams,na.rm=TRUE),solo_songs=n(),solo_mean=solo_streams/solo_songs)
group_mean_streams<-df_12 %>%
  separate_longer_delim(artist.s._name,delim=",") %>% 
  filter(artist_count>1) %>% 
  group_by(artist.s._name) %>% 
  summarise(gr_streams=sum(streams,na.rm=TRUE),gr_songs=n(),gr_mean=gr_streams/gr_songs)
df_12<-mean_streams<-merge(solo_mean_streams,group_mean_streams,by="artist.s._name")
df_12<-mean_streams %>% 
  filter(solo_mean>gr_mean) %>% 
  select(artist.s._name)
## Odp. ramka df_12



