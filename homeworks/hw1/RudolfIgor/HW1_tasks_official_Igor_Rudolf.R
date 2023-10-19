library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% filter(released_year==2023 & released_month<=3) %>% summarise(srednia= mean(as.numeric(streams)))

## Odp. #srednia liczba odtworzen piosenk opublikowanych w roku 2023 w
## pierwszym kwartale wynosila 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df1<- df %>% filter(artist_count>2) %>% summarise(lacznie=sum(in_spotify_playlists))
df2<- df %>% filter(artist_count %in% c(1,2)) %>% summarise(lacznie=sum(in_spotify_playlists))

df1$lacznie
df2$lacznie

ifelse(df1$lacznie>df2$lacznie, "Tak", "Nie")

## Odp. Odpowiedź na powyższe pytanie brzmi Nie. Tych pierwszych jest 428126
#podczas gdy drugich jest 4527593



#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% transmute(date= as.Date(paste(released_year, released_month, released_day, sep = "-")),
                 dzien_tygodnia = format(date, "%A")) %>% 
  group_by(dzien_tygodnia) %>% summarise(lacznie= n()) %>% arrange(desc(lacznie)) %>% 
  slice(1)

## Odp.Najpopularniejszy dzien tygodnia w wypuszczaniu piosenek to piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

data_gatherd <- df %>%
  group_by(artist.s._name) %>%
  filter(all(c(2021, 2022) %in% released_year))

dataA<-data_gatherd %>% filter(released_year==2021) %>%  summarise(rok_2021= n()) %>%  arrange(artist.s._name)

dataB<-data_gatherd %>% filter(released_year==2022) %>% summarise(rok_2022= n()) %>%  arrange(artist.s._name)

wypuszczone_utwory<-dataA %>% mutate(rok_2022= dataB$rok_2022) %>% mutate(procentowa_zmiana=  ((rok_2022-rok_2021)/rok_2021)*100    ) %>% 
  arrange(desc(procentowa_zmiana)) %>% top_n(1)

wypuszczone_utwory

## Odp. Artystom tym był SZA, jej wzrost wyniósł ponad 1600%

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


ileWierszy<-floor(nrow(df)/10)

most_dancable_songs<- df %>% arrange(desc(danceability_.)) %>% head(ileWierszy)

most_dancable_songs<- most_dancable_songs %>% mutate(srednia_na_rok=as.numeric(streams)/(2024-released_year))

most_dancable_songs %>% arrange(desc(as.numeric(srednia_na_rok))) %>% top_n(1) %>% select(artist.s._name)

## Odp. Była to piosenka artysty Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

ileWierszy2<-floor(nrow(df)*0.2)

data_extracted<- df %>% filter(track_name!= "Love Grows (Where My Rosemary Goes)") %>% 
  top_n(ileWierszy2, wt=as.numeric(streams)/in_spotify_playlists)

avg_bpm<- data_extracted %>% summarise(mean(bpm))
most_popular_mode<-sort(table(data_extracted$mode),decreasing = TRUE)[1]

avg_bpm
most_popular_mode

## Odp. Średnie tempo wynosi avg_bpm, zaś najpopularniejszą występującą skalą jest most_popular_mode


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

pory_roku_dane<-df %>%
  mutate(seasons = case_when(
    released_month==12 | released_month %in% c(12, 1, 2) ~ "Winter",
    released_month>=3 & released_month<=5 ~ "Spring",
    released_month>=6 & released_month<=8 ~ "Summer",
    .default = "Autumn"
  ))

pory_roku_dane %>% group_by(seasons) %>% 
  summarise(mean_danceability= mean(danceability_.), 
            mean_valence= mean(valence_.),
            mean_energy= mean(energy_.),
            mean_acousticness= mean(acousticness_.),
            mean_instrumentalness= mean(instrumentalness_.),
            mean_liveness= mean(liveness_.),
            mean_speechiness= mean(speechiness_.))

## Odp.  

#seasons mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness mean_speechiness
#<chr>               <dbl>        <dbl>       <dbl>             <dbl>                 <dbl>         <dbl>            <dbl>
#  1 Autumn               65.3         46.2        62.2              26.7                 2.02           18.0            10.2 
#  2 Spring               68.0         51.0        64.3              28.3                 1.4            18.4            11.0 
#  3 Summer               69.2         51.2        65.8              23.7                 2.74           17.6             9.71
#  4 Winter               65.7         56.1        64.7              28.5                 0.596          18.7             9.50


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

solo_art_key_mode<-df %>% filter(artist_count==1 & released_year==2022) %>%
  group_by(key,mode) %>% summarise(amount= n()) %>%
  arrange(desc(amount)) %>% head(1)

solo_art_key_mode


## Odp. Jest to G Major, odpowiedz w solo_art_key_mode

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% filter(track_name!= "Love Grows (Where My Rosemary Goes)") %>%
  separate_longer_delim(artist.s._name, delim=", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(lacznie_wys= sum(as.numeric(streams))) %>% 
  arrange(desc(lacznie_wys)) %>% head(1)

## Odp. Wykonawcą tym jest The Weeknd, mają  14185552870 odtworzeń utworów

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

#wyciagnijmy tylko te wiersze, ktore zawieraja artystow, ktorzy zadebiutowali
#w 2022 roku

seperated<-df %>% separate_longer_delim(artist.s._name, delim=", ")

condition<-seperated %>% 
  select(artist.s._name) %>% unique() %>% mutate(initial=0)

for (i in 1:nrow(seperated)){
  art<- seperated$artist.s._name[i]
  year<- seperated$released_year[i]
  
  row_index<- which(condition$artist.s._name == art)
  
  val_initiated<- condition[row_index, "initial"]
  
  if(year<2022){
    condition[row_index, "initial"]= -1
  }else if(year==2022 & val_initiated==0 ){
    condition[row_index, "initial"]= 1
  }
}

only_debutants<-condition %>% filter(initial==1)

#mamy 265 takich artystow, ktorzy zadebiutowali w 2022

debutants<- seperated %>% 
  group_by(artist.s._name, mode, key) %>% filter(artist.s._name %in% only_debutants$artist.s._name ) %>% 
  summarise(amount= n())

minor <- debutants %>%
  filter(mode == "Minor") %>%
  mutate(key = ifelse(key == "", "sth", key)) %>%
  pivot_wider(names_from = key, values_from = amount)

major <- debutants %>%
  filter(mode == "Major") %>%
  mutate(key = ifelse(key == "", "sth", key)) %>%
  pivot_wider(names_from = key, values_from = amount)


minor<- replace(minor, is.na(minor), 0) %>% select(-mode) %>% 
  rename_with(~paste0(.,"_minor"), everything()) %>% 
  rename(artist.s._name=artist.s._name_minor)


major<- replace(major, is.na(major), 0) %>% select(-mode)  %>% 
  rename_with(~paste0(.,"_major"), everything()) %>% select(-mode_major) %>% 
  rename(artist.s._name=artist.s._name_major)

mode_key_comp<-full_join(minor,major, by= "artist.s._name")

mode_key_comp<-mode_key_comp %>% group_by(artist.s._name) %>% 
  select(-mode_minor, -mode_major) %>%   mutate_all(~replace(., is.na(.), 0))


mode_key_comp

## Odp. Odpowiedź znajduje się w tabelce mode_key_comp


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

most_popular_songs<-df %>% filter(in_spotify_playlists> in_apple_playlists & 
                                    in_spotify_charts==0 & in_apple_charts>0) %>% select(track_name)

most_popular_songs

## Odp. Wszystkie utwory zostały podane w tabelce most_popular_songs

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo_artists<-df %>% filter(artist_count==1 & track_name!= "Love Grows (Where My Rosemary Goes)") %>% 
  group_by(artist.s._name) %>%
  summarise(srednia_1= mean(as.numeric(streams))) 

more_artists<- df %>% filter(artist_count>=2) %>% group_by(artist.s._name) %>%
  
  separate_longer_delim(artist.s._name, delim=", ") %>% 
  group_by(artist.s._name) %>% summarise(srednia_2= mean(as.numeric(streams)))

joined_artists<- inner_join(solo_artists,more_artists, by= c("artist.s._name"= "artist.s._name" ) )

arts<-joined_artists %>% group_by(artist.s._name) %>% filter(srednia_1> srednia_2) %>% select(artist.s._name)

arts

## Odp. artysci Ci znajduja sie w tabelce arts

