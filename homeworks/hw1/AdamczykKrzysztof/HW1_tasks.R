library(tidyr)
library(dplyr)

df <- read.csv('D:/Studia/Semestr 3/TWD/HW1/spotify-2023.csv')
df <- df %>%  slice(-575)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(!is.na(released_month) & is.numeric(released_month) & as.numeric(released_month) < 4 & !is.na(released_year) & released_year == 2023) %>% 
  summarise(srd = tryCatch(mean(as.numeric(streams), na.rm = TRUE), error = function(e) NA))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  group_by(artist_count) %>% 
  filter(artist_count <3) %>% 
  summarise(playlists = sum(in_spotify_playlists, na.rm = TRUE))

## Odp. count 1 : 3406137, count 2: 1121456, nie

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(day_of_week =  weekdays(as.Date(paste(df$released_year, df$released_month, df$released_day, sep = '-')))) %>% 
  group_by(day_of_week) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Odp. piatek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.


x <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  filter(released_year == 2021) %>% 
  summarise(sum21 = tryCatch(sum(as.numeric(streams), na.rm = TRUE), error = function(e) NA)) 
y <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  filter(released_year == 2022) %>% 
  summarise(sum22 = tryCatch(sum(as.numeric(streams), na.rm = TRUE), error = function(e) NA)) 

ex4 <- inner_join(x,y, by= join_by(artist.s._name == artist.s._name))
ex4 %>% 
  mutate(inc = (sum22 / sum21 )*100) %>% 
  select(c(artist.s._name, inc )) %>% 
  arrange(-inc)

## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  arrange(-danceability_.) %>% 
  slice_head(n= floor(0.1 * nrow(df))) %>% 
  select(track_name, artist.s._name, released_year, danceability_., streams) %>% 
  mutate(srd = as.numeric(streams) / (2023 - released_year +1 )) %>% 
  select(artist.s._name,  srd) %>% 
  arrange(-srd)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


z <-df %>% 
  group_by(artist.s._name, track_name) %>% 
  mutate(spp = as.numeric(streams) / in_spotify_playlists) %>% 
  arrange(-spp) %>% 
  head(floor(0.2 * nrow(df)))
  
# srednie tempo - 125.2
mean(z$bpm, na.rm = TRUE)
# najczesciej powtarzany mode Minor: 96, Major: 94
table(z$mode)

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# daty ktore uznaje w danej porze roku:
# wiosna: 21 marca - 21 czerwca
# lato: 22 czerwca - 22 wrzesnia
# jesien: 23 pazdziernika - 21 grudnia
# zima: 22 grudnia - 20 marca

df$pora_roku <- ifelse (df$released_month %in% c(4,5) | (df$released_month == 3 & df$released_day >20) | (df$released_month == 6 & df$released_day < 22), yes = 'wiosna', no = 
 ifelse (df$released_month %in% c(7,8) | (df$released_month == 6 & df$released_day >21) | (df$released_month == 9 & df$released_day < 23), yes = 'lato', no=  
 ifelse (df$released_month %in% c(10,11) | (df$released_month == 9 & df$released_day >22) | (df$released_month == 12 & df$released_day < 22), yes = 'jesien', no= 'zima')))

df %>% 
  group_by(pora_roku) %>% 
  summarise(energy_mean = mean(energy_., na.rm = TRUE),  danceability_mean = mean(danceability_., na.rm = TRUE), valence_mean = mean(valence_., na.rm = TRUE))
## Odp. 1 jesien           61.2              63.9         47.0
##      2 lato             65.8              68.6         51.5
##      3 wiosna           64.2              68.6         49.5
##      4 zima             66.0              66.7         57.5
#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.


df %>% 
  filter(released_year == 2022 & artist_count == 1) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(key_mode) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

## Odp. "G Major"     25

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?


df %>% 
  mutate(artist.s._name = strsplit(artist.s._name, ",")) %>%
  tidyr::unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  top_n(n = 1)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 


debutants <- df %>% 
  group_by(artist.s._name) %>% 
  summarise(debut = min(released_year)) %>%
  filter(debut == 2022) %>%
  select(artist.s._name)

df1<-df %>% 
  group_by(artist.s._name,key,mode) %>% 
  summarise(n_of_songs=n())
ret <- left_join(debutants, df1, by= join_by("artist.s._name" == "artist.s._name"))

#Odp:
ret

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

 df %>% 
   group_by(track_name) %>% 
   filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts > 0) %>% 
   select(track_name, in_spotify_playlists, in_apple_playlists, in_spotify_charts, in_apple_charts)

 ## Odp. Hits Different -  in spotify playlists: 547 , in apple playlists: 15, in spotify charts: 0, in apple charts: 15
#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
 
df2 <- df %>%
  mutate(artist.s._name = strsplit(artist.s._name, ",")) %>%
  tidyr::unnest(artist.s._name)
a <- df2 %>% 
  group_by(artist.s._name) %>% 
  filter(artist_count == 1) %>% 
  summarise(srd_solo = mean(as.numeric(streams), na.rm = TRUE))
b <- df2 %>% 
  group_by(artist.s._name) %>% 
  filter(artist_count > 1) %>% 
  summarise(srd_non_solo = mean(as.numeric(streams), na.rm = TRUE))
joined <- inner_join(a,b, by= join_by("artist.s._name" == "artist.s._name"))
joined %>% 
  filter(srd_solo > srd_non_solo) %>% 
  mutate(roz = srd_solo - srd_non_solo) %>% 
  arrange(-roz) %>% 
  select(artist.s._name, roz)
## Odp. John Legend