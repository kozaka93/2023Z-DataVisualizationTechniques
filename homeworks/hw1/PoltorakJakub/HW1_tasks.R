library(tidyr)
library(dplyr)

df <- read.csv('D:\\R\\spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year=="2023" & (released_month=="1" | released_month=="2" | released_month=="3")) %>%
  mutate(streams2=as.numeric(streams)) %>% summarise(mean_streams=mean(streams2))

## Odp. Średnia wynosi 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
artist_count_groups<-df %>% group_by(artist_count) %>% summarise(playlists=sum(in_spotify_playlists))

one_or_two_artist<-artist_count_groups %>% filter(artist_count=="1" | artist_count=="2") %>%
  summarise(summarised_playlists=sum(playlists))

two_or_more_artist<-artist_count_groups %>% filter(artist_count!="1" & artist_count!="2") %>%
  summarise(summarised_playlists=sum(playlists))

two_or_more_artist > one_or_two_artist

## Odp. Nie, nie są


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
pasted_data <- df %>% 
  mutate(month_two_digit=ifelse(nchar(released_month)==1,paste("0",released_month,sep=""),released_month),day_two_digit=ifelse(nchar(released_day)==1,paste("0",released_day,sep=""),released_day)) %>% 
  mutate(combined_date=paste(released_year,month_two_digit,day_two_digit,sep="-"))

pasted_data %>% mutate(dayOfWeek=strftime(combined_date,"%A")) %>% 
  group_by(dayOfWeek) %>% summarise(count_of_songs=length(dayOfWeek)) %>% top_n(1,count_of_songs)

## Odp. Tym dniem jest piątek, wypuszczono w nim 526 piosenek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% filter(artist_count=="1") %>% group_by(artist.s._name,released_year) %>% 
  summarise(count=length(released_year)) %>% filter(released_year=="2021" | released_year=="2022") %>% 
  pivot_wider(names_from = released_year,values_from = count) %>% filter(!is.na(`2022`) & !is.na(`2021`)) %>%
  mutate(percent_change=(`2022`-`2021`)/`2021`*100) %>% arrange(-percent_change) %>% head(1)

## Odp. Artysta - SZA, wzrost: 1600%

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% arrange(-danceability_.) %>% head(floor(nrow(df)*0.1)) %>% 
  mutate(streams_per_year=as.numeric(streams)/(2023-released_year+1)) %>% arrange(-streams_per_year) %>% head(1) %>% select(artist.s._name)

## Odp. Tymi artystami są Chencho Corleone, Bad Bunny. Piosenka: Me Porto Bonito

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_twenty_percent_streams_per_playlists<-df %>% filter(artist.s._name!="Edison Lighthouse") %>% 
  mutate(stream_ratio=as.numeric(streams)/as.numeric(in_spotify_playlists)) %>%
  arrange(-stream_ratio) %>% head(round(nrow(df)*0.2))

top_twenty_percent_streams_per_playlists %>% mutate(mean_pace=as.character(mean(as.numeric(bpm)))) %>%
  group_by(mode,mean_pace) %>% summarise(count=length(mode)) %>% arrange(-count) %>% head(1) 

## Odp. Średnie tempo : 125.3 ; Najczęściej występująca tonacja: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% mutate(season=case_when(((released_month==12 & released_day>=22) | released_month %in% c("1","2") | (released_month==3 & released_day<21)) ~ "Zima",
                               ((released_month==3 & released_day>=21) | released_month %in% c("4","5") | (released_month==6 & released_day<22)) ~ "Wiosna", 
                               ((released_month==6 & released_day>=22) | released_month %in% c("7","8") | (released_month==9 & released_day<23)) ~ "Lato",
                               ((released_month==9 & released_day>=23) | released_month %in% c("10","11") | (released_month==12 & released_day<22)) ~ "Jesień" )) %>%
  group_by(season) %>% summarise(mean_danceability= mean(danceability_.),
                                 mean_valence= mean(valence_.), mean_energy=mean(energy_.),
                                 mean_acousticness= mean(acousticness_.), mean_instrumentalness= mean(instrumentalness_.), 
                                 mean_liveness= mean(liveness_.), mean_speechiness= mean(speechiness_.))

## Odp. Charakterystyka jest w ramce

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

most_popular_key_mode_2022<-df %>% filter(released_year=="2022") %>% group_by(key,mode) %>% 
  summarise(amount_of_songs_2022=length(streams)) %>% arrange(-amount_of_songs_2022) %>% head(10)

key_mode_alone <- df %>% filter(artist_count==1) %>% group_by(key,mode) %>% 
  summarise(amount_of_songs_alone=length(streams))

most_popular_key_mode_2022 %>% merge(key_mode_alone,by=c("key","mode")) %>% top_n(1,amount_of_songs_alone)

## Odp. Artyści solowi tworzą najczęściej w tonacji G Major i "" Major spośród 10 najpopularniejszych par key-mode w 2022 roku. Ilość piosenek: 47

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% separate_longer_delim(2,delim = ", ") %>% filter(artist.s._name!="Edison Lighthouse") %>% group_by(artist.s._name) %>%
  summarise(summarised_streams=sum(as.numeric(streams),na.rm = TRUE))  %>% arrange(-summarised_streams) %>% head(1)

## Odp. The Weeknd, 23929760757 odtworzeń

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

filtered_data<-df %>% separate_longer_delim(2,delim = ", ") %>% group_by(artist.s._name,released_year,mode,key) %>%
  summarise(count=length(artist.s._name)) %>% mutate(debuted_in_2022=NA)

artist_name<-" "
debut_year<-0
for(x in 1:nrow(filtered_data)){ 
      if(artist_name!=filtered_data$artist.s._name[x]) {
          artist_name<-filtered_data$artist.s._name[x]
          debut_year<-filtered_data$released_year[x]
          ifelse(debut_year==2022,filtered_data$debuted_in_2022[x]<-TRUE,filtered_data$debuted_in_2022[x]<-FALSE)
      }
      else{
        ifelse(debut_year==2022,filtered_data$debuted_in_2022[x]<-TRUE,filtered_data$debuted_in_2022[x]<-FALSE)
      }
}
filtered_data<- filtered_data %>% filter(debuted_in_2022==TRUE) %>% mutate(mode_key= paste(key,mode)) 

filtered_data %>% group_by(artist.s._name,mode_key) %>% summarise(count=sum(count)) %>% 
  pivot_wider(names_from=mode_key,values_from=count,values_fill = 0)

## Odp. Zestawienie zawiera 265 wierszy i 25 kolumn



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% filter(in_spotify_playlists>in_apple_playlists & in_spotify_charts==0 & in_apple_charts!=0) %>% 
  select(track_name)

## Odp. Zestawienie zawiera 337 wierszy


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?


alone_frame <- df %>% filter(artist_count==1) %>% group_by(artist.s._name) %>% 
  summarise(mean_streams_alone=sum(as.numeric(streams))/length(streams)) %>% 
  filter(!is.na(mean_streams_alone))

multi_frame <- df %>% filter(artist_count!=1) %>% separate_longer_delim(artist.s._name,delim = ", ") %>% group_by(artist.s._name) %>% 
  summarise(mean_streams_multi=sum(as.numeric(streams))/length(streams)) %>% filter(!is.na(mean_streams_multi))

alone_frame %>% inner_join(multi_frame,by="artist.s._name") %>% filter(mean_streams_alone>mean_streams_multi)


## Odp. Zestawienie znajduje się w ramce danych