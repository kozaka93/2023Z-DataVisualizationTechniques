library(tidyr)
library(dplyr)
library(stringr) 
install.packages("tidyverse")

df <- read.csv("C:/Users/Agata Krawczyk/Desktop/New folder/spotify-2023.csv")


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
head(df)
df %>% 
  filter(released_month<=3 & released_year==2023) %>% 
  summarise("srednia"=mean(as.integer(streams) , na.rm = TRUE))
## Odp.
  #216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  mutate("n_of_artists"=ifelse(artist_count>2,"wiecej niz 2","1 lub 2")) %>% 
  group_by(n_of_artists) %>% 
  summarise("suma"=sum(in_spotify_playlists))
## Odp.
  #nie

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate("data_pelna"=as.Date(str_c(released_year,released_month,released_day,sep="-"))) %>% 
  mutate("dzien_tyg"=weekdays(data_pelna)) %>% 
  group_by(dzien_tyg) %>% 
  summarise("ile"=n())%>% 
  top_n(1,ile)
## Odp.
  #piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(released_year==2022 | released_year==2021) %>% 
  filter(artist_count==1) %>% 
  mutate("year"=ifelse(released_year==2021,"a","b")) %>% 
  select(year,artist.s._name) %>% 
  group_by(artist.s._name,year)%>% 
  summarise("liczba_piosenek"=n())%>% 
  pivot_wider(names_from=year,values_from = liczba_piosenek,values_fn=sum,values_fill = 0)%>% 
  mutate("przyrost"=ifelse(a==0,0,(b-a)/a)*100) %>% 
  arrange(desc(przyrost)) %>% 
  head(1)
## Odp.
  #SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
obs <- nrow(df) 
df %>% 
  arrange(desc(danceability_.) )%>% 
  filter(row_number() < obs * 0.1) %>% 
  select(released_year,artist.s._name,streams,track_name) %>% 
  mutate("how_many_years"=2024-released_year) %>% 
  mutate("streams_per_year"=as.integer(streams)/how_many_years) %>% 
  top_n(1,streams_per_year)

## Odp.
  #Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
obs <- nrow(df)
piosenki<-df %>% 
  mutate("a"=as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(desc(a)) %>% 
  filter(row_number() < obs * 0.2) 
  
piosenki %>% 
  group_by(mode) %>% 
  summarise("najczesciej_skala"=n()) 

piosenki %>% 
  summarise("mean"=mean(bpm))




## Odp.
   #minor
   #125.2
  
#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate("pora_roku"=ifelse(released_month<=3 & released_day<=21,"zima",
                            ifelse(released_month<=6 & released_day<=21,"wiosna",
                                   ifelse(released_month<=9 & released_day<=22,"lato",
                                          ifelse(released_month<=12 & released_day<=21,"jesien","zima"))))) %>% 
  select(pora_roku,danceability_., valence_., energy_. ,acousticness_. ,instrumentalness_., liveness_.,speechiness_.) %>% 
  group_by(pora_roku) %>% 
  summarise(mean_danceability=mean(danceability_.),
            mean_valence=mean(valence_.),
            mean_energy=mean(energy_.),
            mean_acousticness=mean(acousticness_.),
            mean_instrumentalness=mean(instrumentalness_.),
            mean_liveness=mean(liveness_.),
            mean_speechiness=mean(speechiness_.))

## Odp.

#pora_roku mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechiness
#<chr>                 <dbl>        <dbl>       <dbl>             <dbl>                 <dbl>         <dbl>            <dbl>
#1 jesien                 63.7         46.4        59.7              31.8                  1.51          17.4            10.2 
#2 lato                   67.3         51.5        66.0              23.8                  1.48          17.9            10.3 
#3 wiosna                 69.0         49.1        63.1              29.2                  1.84          17.8            11.6 
#4 zima                   67.2         54.6        66.2              25.2                  1.52          18.9             9.35

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
tmp<-df %>%  
  group_by(key, mode) %>% 
  filter(key!="") %>% 
  summarise("n"=n()) %>% 
  arrange(desc(n)) %>% 
  mutate("key-mode"=str_c(key,mode,sep=" ")) %>% 
  head(10)
df %>% 
  filter(artist_count==1,released_year==2022) %>% 
  mutate("key_mode"=str_c(key,mode,sep=" ")) %>% 
  filter(key_mode%in%(tmp$`key-mode`) ) %>% 
  group_by(key_mode) %>% 
  summarise("n"=n())%>% 
  top_n(1,n)
  
## Odp.
#"G Major"  
#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  separate_longer_delim(artist.s._name,", ") %>% 
  group_by(artist.s._name) %>% 
  mutate("streams_int"=as.numeric(streams)) %>% 
  select(streams_int) %>% 
  summarise("suma"=sum(streams_int,na.rm = TRUE)) %>% 
  arrange(desc(suma)) %>% 
  head(1)

## Odp.
  #The Weekend
#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
debiutanci<-df %>% 
  separate_longer_delim(artist.s._name,", ") %>% 
  group_by(artist.s._name) %>% 
  summarise("rok_debiutu"=min(released_year)) %>% 
  filter(rok_debiutu==2022) %>% 
  select(artist.s._name)
d1 <- df %>% 
  separate_longer_delim(artist.s._name,", ") %>% 
  filter(artist.s._name%in%debiutanci$artist.s._name) %>% 
  select(artist.s._name,key) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = key,values_from =Freq,names_prefix = ".") 
d2 <- df %>% 
  separate_longer_delim(artist.s._name,", ") %>% 
  filter(artist.s._name%in%debiutanci$artist.s._name) %>% 
  select(artist.s._name,mode) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = mode,values_from =Freq,names_prefix = ".") 
merge(d1,d2,by = intersect(names(d1), names(d2)))
## Odp.


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>% 
  filter(in_spotify_playlists>in_apple_playlists) %>% 
  filter(in_spotify_charts==0 & in_apple_charts!=0) %>% 
  select(track_name)
  
## Odp.
    #337 ich jest, za duzo zeby wypisac

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df %>% 
  separate_longer_delim(artist.s._name,", ") %>% 
  mutate("ile"=ifelse(artist_count==1,"solo","z_innymi")) %>% 
  group_by(ile,artist.s._name) %>% 
  summarise("odtw_per_piosenka"=mean(as.numeric(streams),na.rm = TRUE)) %>% 
  pivot_wider(names_from = ile,values_from = odtw_per_piosenka,values_fill = 0) %>% 
  filter(solo>z_innymi)
  
## Odp.
  #jest ich 259, za duzo



