library(tidyverse)

df <- read_csv('../spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
stm <-df %>%
  filter(released_year==2023,released_month %in% c(1,2,3)) %>% 
  pull(streams)
mean(as.numeric(stm))


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

isp1 <- df %>% filter(artist_count<=2) %>% pull(in_spotify_playlists)
isp2 <- df %>% filter(artist_count>2) %>% pull(in_spotify_playlists)
sum(isp1) < sum(isp2)
mean(isp1) < mean(isp2)

## Odp.nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

data <- as.Date(paste(df$released_year,df$released_month,df$released_day,sep="-"))

df %>% mutate(week_day=weekdays(data)) %>%
  group_by(week_day) %>%
  summarise(n_of_songs = n()) %>%
  top_n(1,n_of_songs) %>%
  pull(week_day)

## Odp.piatek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% filter(artist_count==1) %>% 
  group_by(`artist(s)_name`,released_year) %>%
  summarise(songs_in_year = n(), .groups="drop") %>%
  filter(released_year %in% c(2021,2022)) %>%
  pivot_wider(names_from = released_year,values_from = songs_in_year) %>%
  na.omit() %>%
  mutate(increase = (`2022`/`2021`-1)*100) %>%
  top_n(1,increase) %>%
  pull(`artist(s)_name`)

## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
q <- quantile(df$`danceability_%`,0.9)
df1 <- df %>% select(track_name,`artist(s)_name`,`danceability_%`,released_year,streams)
df1 %>% filter(`danceability_%` >= q) %>%
  mutate(years_since_release = 2024-released_year) %>%
  mutate(avg_year_streams = as.numeric(streams)/years_since_release) %>%
  top_n(1,avg_year_streams) %>%
  pull(`artist(s)_name`)



## Odp."Chencho Corleone, Bad Bunny"

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
q <- quantile(df$`in_spotify_playlists`,0.8)
df1 <- df %>% select(`in_spotify_playlists`,bpm,mode)
bpm <- df1 %>% filter(in_spotify_playlists>=q) %>% pull(bpm)
mode <- df1 %>% filter(in_spotify_playlists>=q) %>% pull(mode)
table(mode)
mean(bpm)

## Odp. bmp = 121.3194 , mode = Major

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df1 <- df %>% mutate(season = case_when(
  released_month %in% c(1,2) | (released_month==12 & released_day >=22) | (released_month==3 & released_day<21) ~ "winter",
  released_month %in% c(4,5) | (released_month==3 & released_day >=21) | (released_month==6 & released_day<22) ~ "spring",
  released_month %in% c(7,8) | (released_month==6 & released_day >= 22) | (released_month==9 & released_day<23) ~ "summer",
  T ~ "autumn"
  )) %>% 
  select(`danceability_%`,`energy_%`,`valence_%`,season)
  
df2 <- df1 %>%group_by(season)%>%
  summarise(mean_dance = mean(`danceability_%`),mean_energy = mean(`energy_%`), mean_valence = mean(`valence_%`))
  
  df1 %>% ggplot(aes(x=`danceability_%`,col = season)) + geom_density()
  df1 %>% ggplot(aes(x=`energy_%`,col = season)) + geom_density()
  df1 %>% ggplot(aes(x=`valence_%`,col = season)) + geom_density()
  

## Odp.w lato i wiosne bardziej taneczne, w jesien malo energicze, w zime bardziej pozytywne

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
  
topkm <- df %>% filter(!is.na(key) & !is.na(mode))%>%
  group_by(key,mode) %>%
  summarise(k_m_count = n(), .groups = "drop") %>%
  top_n(10,k_m_count)
topkm <- paste(topkm$key,topkm$mode,sep="-")

df %>% mutate(km = paste(key,mode,sep="-"))%>%
  filter(km %in% topkm) %>%
  group_by(km) %>%
  summarise(solo=mean(artist_count==1)) %>%
  top_n(1,solo) %>%
  pull(km)
  


## Odp. "F-Major"

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% mutate(streams = as.numeric(streams))%>%
  filter(!is.na(streams))%>%
  group_by(`artist(s)_name`) %>% 
  summarise(all_streams = sum(streams)) %>%
  top_n(1,all_streams) %>%
  pull(`artist(s)_name`)


## Odp."The Weeknd"

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach.

debutants <- df %>% group_by(`artist(s)_name`) %>% 
  summarise(debut = min(released_year)) %>%
  filter(debut == 2022) %>%
  pull(`artist(s)_name`)

df1<-df %>% filter(`artist(s)_name` %in% debutants) %>%
  group_by(`artist(s)_name`,key,mode) %>% summarise(n_of_songs=n())


## Odp.



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% filter(`in_spotify_playlists`>`in_apple_playlists`) %>%
  filter(in_spotify_charts==0 & in_apple_charts>0) %>%
  pull(track_name)
  

## Odp.


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

artists <- df %>% 
  mutate(`artist(s)_name` = str_split(`artist(s)_name`,", ")) %>%
  unnest(`artist(s)_name`) %>%
  group_by(`artist(s)_name`, solo = as.factor(artist_count==1)) %>%
  summarise(avg_st = mean(as.numeric(streams),na.rm=T)) %>%
  pivot_wider(names_from = solo,values_from = avg_st) %>%
  filter(!is.na(`TRUE`)&!is.na(`FALSE`)) %>%
  mutate(diff = `TRUE`-`FALSE`) %>%
  filter(diff>0) %>%
  pull(`artist(s)_name`)
  

## Odp.


