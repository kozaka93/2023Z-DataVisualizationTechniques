library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv("spotify-2023.csv")

df[575,"streams"] <- NA #pozbycie się będnych danych

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

ODP1 <- df %>%
  filter(released_year==2023,released_month %in% c(1,2,3))%>%
  summarize(mean_streams = mean(as.numeric(streams),na.rm = TRUE))%>%
  pull(mean_streams)%>%
  head(1)

## Odp.216 150 568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

ODP2<- df %>%
  mutate(is_more = ifelse(artist_count>2,TRUE,FALSE))%>%
  group_by(is_more)%>%
  summarize(playlists = sum(in_spotify_playlists))

  
## Odp. nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

ODP3 <- df %>%
  mutate(released_date =paste(as.character(released_year),as.character(released_month),as.character(released_day),sep="-"))%>%
  mutate(released_wday = weekdays(as.Date(released_date)))%>%
  group_by(released_wday)%>%
  summarise(n = n())%>%
  arrange(-n)%>%
  head(1)%>%
  pull(released_wday)

## Odp. piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

releases_2021 <- df %>%
  filter(released_year == 2021) %>%
  group_by(artist.s._name) %>%
  summarize(n2021 = n())

releases_2022 <- df %>%
  filter(released_year == 2022) %>%
  group_by(artist.s._name) %>%
  summarize(n2022 = n())

ODP4 <- releases_2021 %>%
  inner_join(releases_2022,by= "artist.s._name") %>%
  mutate(rise = (n2022/n2021)*100)%>%
  arrange(-rise)%>%
  head(1)%>%
  pull(artist.s._name)


## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

top10p <- quantile(df$danceability_.,0.9)

ODP5 <- df %>%
  filter(danceability_. >= top10p) %>%
  mutate(mean_per_year = as.numeric(streams)/(2023-as.numeric(released_year)+1))%>%
  arrange(-mean_per_year)%>%
  select(track_name,artist.s._name)%>%
  head(1)
  

## Odp.piosenka "Me Porto Bonito" artystów Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

ODP6_MODE <- df %>%
  mutate(streams_per_playlist = as.numeric(streams)/as.numeric(in_spotify_playlists))%>%
  arrange(-streams_per_playlist)%>%
  head(round(length(df$track_name)*0.2))%>%
  group_by(mode)%>%
  summarize(most_often_mode = n())%>%
  top_n(1,most_often_mode)

ODP6_TEMPO <- df %>%
  mutate(streams_per_playlist = as.numeric(streams)/as.numeric(in_spotify_playlists))%>%
  arrange(-streams_per_playlist)%>%
  head(round(length(df$track_name)*0.2))%>%
  summarize(mean_tempo = mean(bpm))
  
## Odp. 125.3 Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

ODP7 <- df %>%
  mutate(season = case_when(
    released_month %in% c('1','2') | (released_month=='12' & released_day>=22) | 
      (released_month=='3' & released_day<=20) ~ 'winter',
    released_month %in% c('4','5') | (released_month=='3' & released_day>20) | 
      (released_month=='6' & released_day<=20) ~ 'spring',
    released_month %in% c('7','8') | (released_month=='6' & released_day>20) |
      (released_month=='9' & released_day>='22')~ 'summer',
    TRUE ~ 'autumn'))%>%
  group_by(season)%>%
  summarize(mean_danceability = mean(danceability_.,na.rm=T),
            mean_energy = mean(energy_.,na.rm= T), 
            mean_liveness = mean(liveness_.,na.rm=T),
            mean_valance = mean(valence_.,na.rm=T),
            mean_liveness = mean(liveness_.,na.rm=T),
            mean_accousticnes = mean(acousticness_.,na.rm=T),
            mean_instrumentalness = mean(instrumentalness_.,na.rm=T),
            mean_speechiness = mean(speechiness_.,na.rm=T))

## Odp. Najbardziej taneczne i instrumentalne są letnie piosenki, najbardziej akustyczne są jesienne

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

pairs <- df%>%
  filter(released_year == 2022 & key != "")%>%
  mutate(key_mode = paste(key,"_",mode))%>%
  group_by(key_mode)%>%
  summarize(n=n())%>%
  top_n(10,n)

ODP8 <- df %>%
  filter(released_year == 2022 & key != "")%>%
  mutate(key_mode = paste(key,"_",mode))%>%
  filter(artist_count==1 & key_mode %in% pairs$key_mode)%>%
  group_by(key_mode)%>%
  summarise(n = n())%>%
  top_n(1,n)
  

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

ODP9 <- df %>%
  mutate(artists = str_replace_all(artist.s._name,"<",""))%>% 
  separate_rows(artists, sep=", ")%>%
  group_by(artists)%>%
  summarize(streams_sum = sum(as.numeric(streams),na.rm=T))%>%
  top_n(1,streams_sum)
  

## Odp. The Weeknd  23 929 760 757

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debiut2022<-  df%>%
  mutate(artists = str_replace_all(artist.s._name,"<",""))%>% # nie wiem co to robi ale z tym działa
  separate_rows(artists, sep=", ")%>%
  group_by(artists)%>%
  summarize(debiut = min(released_year))%>%
  filter(debiut==2022)
  
ODP10 <- df %>%
  mutate(artists = str_replace_all(artist.s._name,"<",""))%>% # nie wiem co to robi ale z tym działa
  separate_rows(artists, sep=", ")%>%
  inner_join(debiut2022)%>%
  mutate(mode_key=paste(mode,key))%>%
  group_by(artists,mode_key)%>%
  summarize(n = n())%>%
  pivot_wider(names_from = "mode_key",values_from = "n",values_fill = 0)%>%
  group_by(artists)%>%
  summarize(across(everything(), sum))
  

## Odp. zmienna ODP10


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

ODP11 <- df %>%
  filter(in_spotify_playlists>in_apple_playlists)%>%
  filter(in_spotify_charts==0 & !is.na(in_apple_charts) & in_apple_charts>0)%>%
  select(track_name)

## Odp. zmienna ODP11


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

ODP12 <- df %>%
  mutate(artists = str_replace_all(artist.s._name,"<",""))%>% # nie wiem co to robi ale z tym działa
  separate_rows(artists, sep=", ")%>%
  mutate(n = if_else(artist_count==1,"solo","feat"))%>%
  group_by(artists,n) %>%
  summarize(mean_streams = mean(as.numeric(streams),na.rm=TRUE))%>%
  pivot_wider(names_from = n,values_from = mean_streams)%>%
  filter(!is.na(solo) & !is.na(feat))%>%
  filter(solo>feat)

## Odp. zmienna ODP12

