library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year=='2023' , released_month %in% c('1','2','3')) %>% 
  summarise(avg_streams=mean(as.numeric(streams), na.rm=TRUE)) -> zad1

## Odp. bardzo niemiła, miewa antysemickie odpały (216150568)

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  filter(artist_count>2) %>% 
  summarise(ponad_dwa=sum(in_spotify_playlists))
df %>% 
  filter(artist_count<2) %>% 
  summarise(nie_ponad_dwa=sum(in_spotify_playlists))

## Odp.Reasumując wszystkie aspekty kwintesencji tematu dochodzę do fundamentalnej konkluzji: POLSKA GU   znaczy nie.


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
df %>% 
  mutate(dzien=weekdays(as.Date(paste(released_year,released_month,released_day, sep='-')))) %>% 
  group_by(dzien) %>% 
  summarise(zliczenie=n()) %>% 
  arrange(-zliczenie) %>% 
  head(1)

## Odp.Dies irae dies illa czyli dzień po czwartku i przed sobotą

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
df %>% 
  filter(artist_count==1, released_year %in% c('2021', '2022')) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(liczba=n()) %>% 
  pivot_wider(names_from = released_year, values_from = liczba) %>%
  na.omit() %>% 
  rename("pierwyj"="2021", "wtoryj"="2022") %>% 
  mutate(przyrost=(wtoryj-pierwyj)/pierwyj) %>% 
  arrange(-przyrost) %>% 
  head(1)

## Odp. cicho SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  arrange(-danceability_.) %>% 
  head(length(df$track_name)%/%10) %>% 
  mutate(srok=as.numeric(streams)/(2024-as.numeric(released_year))) %>% 
  arrange(-srok) %>% 
  head(1) %>% 
  pull(artist.s._name)

## Odp. Zawsze dwóch ich jest Chencho Corleone i Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
df %>% 
  arrange(-in_spotify_playlists) %>% 
  head(length(df$track_name)%/%5) %>% 
  summarise(srtempo=mean(bpm, na.rm=TRUE), tonacja=names(which.max(table(mode)))) -> xd

## Odp. tempo wcale nie srednie: 121,4842 częściej mamy skalę dur-ową.

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  filter(released_month %in% c('1','2','3')) -> zima

df %>% 
  filter(released_month %in% c('4','5','6')) -> wiosna

df %>% 
  filter(released_month %in% c('7','8','9')) -> lato

df %>% 
  filter(released_month %in% c('10','11','12')) -> jesien

zima %>% 
  summarise(sr_tanecznosc=mean(danceability_.),sr_niewiemco=mean(valence_.), sr_dzwiekowosc=mean(acousticness_.), sr_energia=mean(energy_.), sr_instrumentalnosc=mean(instrumentalness_.), sr_wymownosc=mean(speechiness_.), sr_zywotnosc=mean(liveness_.)) -> staty_zima

wiosna %>% 
  summarise(sr_tanecznosc=mean(danceability_.),sr_niewiemco=mean(valence_.), sr_dzwiekowosc=mean(acousticness_.), sr_energia=mean(energy_.), sr_instrumentalnosc=mean(instrumentalness_.), sr_wymownosc=mean(speechiness_.), sr_zywotnosc=mean(liveness_.)) -> staty_wiosna

lato %>% 
  summarise(sr_tanecznosc=mean(danceability_.),sr_niewiemco=mean(valence_.), sr_dzwiekowosc=mean(acousticness_.), sr_energia=mean(energy_.), sr_instrumentalnosc=mean(instrumentalness_.), sr_wymownosc=mean(speechiness_.), sr_zywotnosc=mean(liveness_.)) -> staty_lato

jesien %>% 
  summarise(sr_tanecznosc=mean(danceability_.),sr_niewiemco=mean(valence_.), sr_dzwiekowosc=mean(acousticness_.), sr_energia=mean(energy_.), sr_instrumentalnosc=mean(instrumentalness_.), sr_wymownosc=mean(speechiness_.), sr_zywotnosc=mean(liveness_.)) -> staty_jesien

razem <- bind_rows(staty_zima,staty_wiosna, staty_lato, staty_jesien)
## Odp.Wniosków to jest kilka, najbardziej tanecznie jest wiosną i latem, najbrdziej dzwiekowo jest jesienia usw. usw.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year=='2022', !is.na(key)) %>% ##nie wiem czemu is.na nie działa
  group_by(key,mode) %>% 
  summarise(wystapienia=n()) %>% 
  arrange(-wystapienia) %>% 
  head(10) %>% 
  mutate(lacz=paste(key,mode, sep=" ")) %>% 
  pull(lacz) -> wek

df %>% 
  select(artist_count, key, mode) %>% 
  mutate(lacz=paste(key,mode, sep=" ")) %>% 
  filter(lacz %in% wek) %>% 
  filter(artist_count==1) %>% 
  group_by(lacz) %>% 
  summarise(licz=n()) %>% 
  arrange(-licz) ->xd




  


## Odp.Najpopularniejsze jest g-dur.

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% 
  select(artist.s._name, streams) %>% 
  separate_longer_delim(artist.s._name, delim = ",") %>% 
  group_by(artist.s._name) %>% 
  summarise(wsumie = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  arrange(-wsumie) %>% 
  head(1)
  


## Odp. Dze Łikend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 



df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>%
  summarise(anfang=min(released_year)) %>% 
  filter(anfang==2022) %>% 
  select(artist.s._name) -> debile

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>%
  filter(artist.s._name %in% debile$artist.s._name) %>% 
  mutate(tonacja=paste(mode,key)) %>% 
  group_by(artist.s._name, tonacja) %>% 
  summarise(liczaj=n()) %>% 
  pivot_wider(names_from = tonacja, values_from = liczaj, values_fill = 0) -> zest

  
## Odp.Masz pan zestawienie
#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists>in_apple_playlists, in_spotify_charts==0, in_apple_charts>0) %>% 
  pull(track_name) -> xd

## Odp.Same szatańskie
#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  mutate(towarzystwo = case_when(artist_count == 1 ~ 'nie',TRUE ~ 'tak')) %>% 
  group_by(artist.s._name,towarzystwo) %>% 
  summarise(liczbapian = n(), przelicz = sum(as.numeric(streams), na.rm = TRUE), .groups = 'drop') %>% 
  mutate(notaksrednio=przelicz/liczbapian) %>% 
  group_by(artist.s._name) %>% 
  summarise(sam=sum(notaksrednio[towarzystwo=='nie']),razem=sum(sotaksrednio[towarzystwo=='tak'])) ##cos tu nie dziala nie wiem czemu
## Odp.



