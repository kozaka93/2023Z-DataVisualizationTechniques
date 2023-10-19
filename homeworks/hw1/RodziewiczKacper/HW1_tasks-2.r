


library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv') # usuwam ten wiersz bo jest bledna dana w views!
df <- df[-575,]
df <- mutate(df,streams=as.numeric(streams))

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale? g

df %>% filter(released_month<4 & released_year==2023)%>% mutate(streams = as.numeric(streams)) %>% summarise(average = mean(streams, na.rm = TRUE))

## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?g

MORETHAN2 <- filter(df,artist_count>2)
NOMORETHAN2 <- filter(df,artist_count<=2)

PLAYLISTMORE <- sum(MORETHAN2$in_spotify_playlists)
PLAYLISTNOMORE <- sum(NOMORETHAN2$in_spotify_playlists)



## Odp.wiecej jest playlist z piosenkami stworzonymi przez 1 lub 2 artystowg


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

DF3<- mutate(df,date=as.Date(paste(released_year,released_month,released_day,sep="-")))
DF3 <- mutate(DF3,day=weekdays(date))

group_by(DF3,day) %>% summarise(count=n())

## Odp. piatek 
#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

DF4<-filter(df,(artist_count==1 & (released_year==2021 | released_year==2022)))

DF2021 <- filter(DF4,released_year==2021)
DF2022 <- filter(DF4,released_year==2022)

UNIQUE2021 <- unique(DF2021$artist.s._name)
UNIQUE2022 <- unique(DF2022$artist.s._name)

intersect <- intersect(UNIQUE2021,UNIQUE2022)

DF4 <- filter(DF4,artist.s._name %in% intersect)


DF42021 <- filter(DF4,released_year==2021)
DF42022 <- filter(DF4,released_year==2022)

group_by(DF42021,artist.s._name) %>% summarise(count=n()) -> DF2021
group_by(DF42022,artist.s._name) %>% summarise(count=n()) -> DF2022


merge <- inner_join(DF2021, DF2022, by = "artist.s._name")
merge <- mutate(merge,growth=(count.y-count.x)/count.x)



## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

 arrange(df,desc(danceability_.)) %>% slice(1:(0.1*n())) %>% mutate(how_many_years=2024-released_year) %>%
   mutate(how_many_views_per_year=as.numeric(streams)/how_many_years) %>% arrange(desc(how_many_views_per_year)) %>% head(1)

## Odp. Me Porto Bonito       Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
 mutate(df,streams_playlist=streams/in_spotify_playlists) %>% arrange(desc(streams_playlist)) %>% slice(1:(0.2*n())) -> DF6
 DF6 %>% summarise(mean_bpm=mean(bpm)) 
 DF6 %>% group_by(mode) %>% summarise(n=n())
 
 

## Odp. srednie tempo: 125.2 bpm , najczesciej wystepuja skala: minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

 DF7 <- mutate(df, date = as.Date(paste(released_year, released_month, released_day, sep = "-")))
 
 pora_roku <- function(data) {
   miesiac <- as.numeric(format(data, "%m"))
   dzien <- as.numeric(format(data, "%d"))
   
   if ((miesiac == 12 && dzien >= 22) || miesiac <= 2 || (miesiac == 3 && dzien < 21)) {
     return("Winter")
   } else if ((miesiac == 3 && dzien >= 21) || miesiac <= 5 || (miesiac == 6 && dzien < 22)) {
     return("Spring")
   } else if ((miesiac == 6 && dzien >= 22) || miesiac <= 8 || (miesiac == 9 && dzien < 23)) {
     return("Summer")
   } else {
     return("Autumn")
   }
 }
 
 DF7 <- mutate(DF7, season = sapply(DF7$date, pora_roku))
 
 result <- DF7 %>%
   group_by(season) %>%
   summarise(
    meandanceability = mean(danceability_., na.rm = TRUE),
    meanenergy = mean(energy_., na.rm = TRUE),
    meanliveness = mean(liveness_., na.rm = TRUE)
   )
 
 print(result)
 
  
  
 

## Odp. wynik w result

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
  top_key_modes <- df %>%
    filter(released_year == 2022) %>%
    count(key, mode, sort = TRUE) %>%
    slice_head(n = 10)
  
  wynik <- df %>%
    filter(released_year == 2022, artist_count == 1) %>%
    count(key, mode) %>%
    semi_join(top_key_modes, by = c("key", "mode")) %>%
    arrange(desc(n)) %>%
    slice_head(n = 1)
  
  print(wynik)
  

## Odp. G,Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% filter(artist_count==1) %>% group_by(artist.s._name) %>% summarise(totalviews=sum(as.numeric(streams))) %>% arrange(desc(totalviews))

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

wynik <- filter(df,artist_count=="1") %>%
  group_by(artist.s._name) %>%
  filter(min(released_year) == 2022) %>%
  
  group_by(artist.s._name, key, mode) %>%
  summarise(LiczbaUtworow = n()) %>%
  
  pivot_wider(names_from = c(key, mode), 
              values_from = LiczbaUtworow, 
              values_fill = 0)

  print(wynik)


## Odp. wyswietla sie przy print(wynik)



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

 filter(df,in_spotify_playlists>in_apple_playlists) %>% filter(in_spotify_charts==0 & in_apple_charts>0) -> DF11
 
 ODP11 <- DF11$track_name
 
## Odp. zawarte w ODP11



#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
 
 DF121 <- df %>%
   filter(artist_count == 1) %>%
   group_by(artist.s._name) %>%
   summarise(average_views = mean(as.numeric(streams)))
 
 artists <- DF121$artist.s._name
 vector <- numeric(length(artists))
 vector_index <- 1
 
 for (each in artists) {
   sum_of_views = 0
   number_of_songs = 0
   
   for (row in 1:nrow(df)) {
     artists2 = df[row, "artist.s._name"]
     views = as.numeric(df[row, "streams"])
     
     if (str_detect(artists2, each) & df[row, "artist_count"] > 1) {
       sum_of_views = sum_of_views + views
       number_of_songs = number_of_songs + 1
     }
   }
   
   if (number_of_songs == 0) {
     vector[vector_index] = 0
   } else {
     vector[vector_index] = sum_of_views / number_of_songs
   }
   
   vector_index <- vector_index + 1
 }
 
 
  DF121 = mutate(DF121,avg_on_feat=vector)
 
  DF121 %>% filter(average_views>avg_on_feat & avg_on_feat>0) -> DF121
  
  DF121$artist.s._name

 
## Odp. ODP W DF121$artist.s._name




















