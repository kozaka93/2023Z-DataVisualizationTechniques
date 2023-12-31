library(tidyr)
library(dplyr)
library(stringi)
library(stringr)

df <- read.csv('./TWD/spotify-2023.csv')
View(df)


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% filter(released_year==2023 & 1<=released_month & released_month<=3) %>%
  summarise(mean = mean(as.numeric(streams)))

## Odp.W 1 kwartale 2023 roku średnia liczba odtworzeń to 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% filter(artist_count>=3) %>%
  summarise(mean = mean(as.numeric(in_spotify_playlists),na.rm = TRUE))

df %>% filter(artist_count<=2) %>%
  summarise(mean = mean(as.numeric(in_spotify_playlists),na.rm = TRUE))

## Odp. Nie, średnio utwory stworzone przez >=3 artystów są na 3822.554 playlistach spotyfie,a utwory stworzone przez 1 lub 2 artystów są
# na 5383.583 playlistach spotyfie 


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% transmute(wday = as.POSIXlt(paste(released_year,released_month,released_day,sep = "-"))$wday) %>%
  group_by(wday) %>% summarise(n = n()) %>% top_n(1,n)

## Odp. Najpopularniejsza jest sobota

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych
# piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.
artistsDebiut <- df %>%
  filter(artist_count==1) %>%
  select(artist.s._name,released_year) %>%
  group_by(artist.s._name) %>%
  filter(any(released_year==2021) & any(released_year==2022)) %>%
  filter(released_year==2021 | released_year==2022) %>%
  group_by(artist.s._name,released_year) %>%
  summarise(n = n()) %>% 
  mutate(released_year = ifelse(released_year==2021,"Year2021","Year2022"))

artistsDebiut2 <- artistsDebiut %>% 
  pivot_wider(names_from = released_year,values_from = n)

artistsDebiut2 %>% summarise(ratio = Year2022/Year2021) %>% top_n(1,ratio)
## Odp. SZA ma największy wzrost piosenek wypuszczanych w 2022 względem 2021

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% top_frac(0.1,danceability_.) %>%
  transmute(x = artist.s._name,y = as.numeric(streams)/(2024 - released_year)) %>% top_n(1,y)

## Odp. Chencho Corleone,Bad Bunny na najwięcej (720378909) wyświetleń na rok

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>% 
  top_frac(0.2,in_spotify_playlists) %>%
  group_by(mode) %>%
  summarise(n = n()) %>%
  top_n(1,n)

df %>% 
  top_frac(0.2,in_spotify_playlists) %>%
  summarise(mean = mean(bpm))
  
## Odp. Najczęstrzy mode = Major,średnie bpm = 121,4842

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
season <- function(x){
  # przyjmuje wektor dat w formacie %Y-%m-%d zamienia rok na 2020 (przestępny) i zwraca string pory roku
  x2 <- as.Date(paste(2020, strftime(as.Date(x,"%Y-%m-%d"),"%m-%d"),sep = "-"), format = "%Y-%m-%d")
  
  SP <- as.Date("2020-3-20", format = "%Y-%m-%d") # Spring 
  SM <- as.Date("2020-6-21",  format = "%Y-%m-%d") # Summer 
  AU <- as.Date("2020-9-22",  format = "%Y-%m-%d") # Fall 
  WT <- as.Date("2020-12-21",  format = "%Y-%m-%d") # Winter
  ifelse(x2 < SP | x2 >= WT,"Winter",
         ifelse(x2 >= SP & x2 < SM, "Spring",
                ifelse(x2 >= SM & x2 < AU, "Summer","Fall")))
}
df %>%
  mutate(season = season(paste(released_year,released_month,released_day,sep = "-"))) %>% 
  group_by(season) %>%
  summarise(meanDanceability = mean(danceability_.,na.rm = TRUE),meanEnergy = mean(energy_.,na.rm = TRUE))


## Odp. Największa taneczność Wiosna , największa energia zima

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>%
  filter(artist_count == 1 & released_year==2022) %>%
  group_by(key,mode) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% head(1)
  

## Odp.Najczęstrza para key-mode w 2022 dla artysów solowych to G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  filter(artist_count==1) %>%
  group_by(artist.s._name) %>%
  summarise(suma = sum(as.numeric(streams),na.rm = TRUE)) %>%
  arrange(-suma) %>% head(1)

## Odp. The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df1 <- df %>%
  mutate(artist.s._name = str_remove(artist.s._name,pattern = '<[A-Za-z]{2}>')) %>%
  group_by(artist.s._name) %>%
  filter(min(released_year)==2022) %>% 
  select(artist.s._name,key) %>%
  group_by(artist.s._name,key) %>%
  summarise(n = n())

df2 <- df %>% 
  mutate(artist.s._name = str_remove(artist.s._name,pattern = '<[A-Za-z]{2}>')) %>%
  group_by(artist.s._name) %>%
  filter(min(released_year)==2022) %>%
  select(artist.s._name,mode) %>%
  group_by(artist.s._name,mode) %>%
  summarise(m = n())

df1$key[df1$key==""] = "none"

new_df1 <- df1 %>% 
  pivot_wider(names_from = key,values_from = n,values_fill = 0)

new_df2 <- df2 %>% 
  pivot_wider(names_from = mode,values_from = m,values_fill = 0)

merged_dfs <- merge(new_df1,new_df2,by = "artist.s._name")

## Odp. w merged_dfs


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  filter(in_spotify_playlists>in_apple_playlists & in_spotify_charts==0 & in_apple_charts!=0) %>% 
  select(track_name)

## Odp. powyższy df
  

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
allArtists <- df %>%  
  mutate(artist.s._name = str_remove(artist.s._name,pattern = '<[A-Za-z]{2}>')) %>%
  filter(artist_count>1) %>% 
  select(artist.s._name,streams) %>%
  mutate(artist.s._name = as.character(artist.s._name),streams = as.numeric(streams)) %>%
  separate_rows(artist.s._name, sep = ",\\s*") %>%
  group_by(artist.s._name) %>%
  summarise(meanInGroup = mean(streams,na.rm = TRUE))

oneArtistMean <- df %>%
  mutate(artist.s._name = str_remove(artist.s._name,pattern = '<[A-Za-z]{2}>')) %>%
  filter(artist_count==1) %>% 
  select(artist.s._name,streams) %>% 
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>% 
  summarise(mean = mean(streams,na.rm = TRUE))

final_comparisene <- inner_join(allArtists2,oneArtistMean) %>% filter(meanInGroup>mean)
final_comparisene %>% select(artist.s._name)


# Odp. powyższy df






