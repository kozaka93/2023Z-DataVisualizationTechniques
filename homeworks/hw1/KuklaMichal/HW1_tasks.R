library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

View(df)





#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?


df %>% filter(released_year == "2023" & released_month %in% c(1,2,3)) %>% 
  summarise(mean_streams = mean(as.integer(streams), na.rm = TRUE))

## Odp. Średnia to 216150568.







#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% filter(artist_count > 2) %>% 
  summarise(suma1 = sum(in_spotify_playlists)) %>% pull(suma1) -> liczba_1

df %>% filter(artist_count <= 2) %>% 
  summarise(suma2 = sum(in_spotify_playlists)) %>% pull(suma2) -> liczba_2

liczba_1 > liczba_2

## Odp. Tak nie jest, piosenki stworzone przez więcej niż 2 artystów są zawarte na mniejszej liczbie playlist spotify.








#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?


daty <- paste(df$released_year, df$released_month, df$released_day, sep = "-")
df %>% mutate(day_of_the_week = weekdays(as.Date(daty))) %>% 
  group_by(day_of_the_week) %>% 
  summarise(zliczenia = n()) %>% 
  arrange(-zliczenia) %>% 
  head(1) %>% 
  select(day_of_the_week)

## Odp. Najpopularniejszym dniem jest piątek.









#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy opublikowali i w 2021 i w 2022.


df %>% filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>% 
  select(artist.s._name, track_name, released_year) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(ilosc = n()) %>% 
  pivot_wider(names_from = released_year, values_from = ilosc) %>% na.omit %>% 
  rename("rok_2021" = "2021", "rok_2022" = "2022") %>%
  mutate(wzrost_procentowy = (rok_2022/rok_2021)-1) %>% 
  arrange(-wzrost_procentowy) %>% 
  head(1) %>% 
  select(artist.s._name)
  

## Odp. Jest to artysta SZA.










#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


df %>% arrange(-danceability_.) %>% head(0.1*nrow(df)) %>% 
  mutate(ile_ma_lat = 2024 - released_year) %>% 
  mutate(srednio_na_rok = as.integer(streams)/ile_ma_lat) %>% 
  arrange(-srednio_na_rok) %>% 
  head(1) %>% 
  select(artist.s._name)


## Odp. Artysty Bad Bunny. Chodzi o piosenkę "Chencho Corleone".







#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% arrange(-in_spotify_playlists) %>% head(0.2*nrow(df)) %>% 
  select(in_spotify_playlists, bpm, mode) %>% 
  summarise(srednie_tempo = mean(bpm, na.rm = T), najczestsza_skala = names(which.max(table(mode))))


## Odp. Średnie tempo wynosi około 121,5, a najczęstsza skala to Major.








#### 7. Jakie charakterystyki taneczności, energetyki, itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% mutate(
  season = case_when(
    released_month %in% c("1","2") | (released_month == "12" & released_day >= 22) | 
      (released_month == "3" & released_day <= 20) ~ "zima",
    released_month %in% c("4","5") | (released_month == "3" & released_day >= 21) | 
      (released_month == "6" & released_day <= 21) ~ "wiosna",
    released_month %in% c("7","8") | (released_month == "6" & released_day >= 22) |
      (released_month == "9" & released_day <= 22) ~ 'lato',
    TRUE ~ "jesień"
  )) %>%
  group_by(season) %>%
  summarize(
    mean_danceability = mean(danceability_.),  # nie ma NA, zatem nie wykonuję na.rm = T
    mean_valence = mean(valence_.),
    mean_energy = mean(energy_.),
    mean_acousticness = mean(acousticness_.),
    mean_instrumentalness = mean(instrumentalness_.),
    mean_liveness = mean(liveness_.),
    mean_speechiness = mean(speechiness_.),
  ) %>% View()


## Odp.Najbardziej taneczne są piosenki z lata,
# najmniej energetyczne są piosenki z jesieni, 
# najmniej istrumentalne są piosenki zimowe,
# itd., wszystko jest opisane dokładnie w tabelce z funkcji View().









#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% filter(released_year == 2022, key != "", mode != "") %>% group_by(key, mode) %>% 
  summarise(ilosc_par = n(), solisci = sum(artist_count == 1, na.rm = T)) %>% 
  mutate(key_mode = paste(key, mode, sep = '-')) %>%
  arrange(-ilosc_par) %>% head(10) %>%
  filter(solisci != 0) %>% head(1) %>%  # dlatego head(10) najpierw, bo w poleceniu kazano wybrać spośród dziesięciu par dokładnie jedną
  pull(key_mode)


## Odp. Jest to G-Major.










#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?


df %>% filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(odtworzenia = sum(as.numeric(streams), na.rm = T)) %>%
  arrange(-odtworzenia) %>% head(1) %>% select(artist.s._name)


## Odp. Jest to the Weeknd.







#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 


df %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>% filter(artist.s._name != "") %>% 
  filter(key != "", mode != "") %>% 
  summarise(debiut = min(released_year, na.rm = T)) %>%
  filter(debiut == 2022) %>% 
  pull(artist.s._name) -> debiutanci
  

df %>% separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name %in% debiutanci) %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(zliczenia = n()) %>% 
  pivot_wider(names_from = c(key,mode), values_from = zliczenia) -> zadanie10

zadanie10[is.na(zadanie10)] <- 0
  
View(zadanie10)


## Odp. Wszystko jest w data frame zadanie10.







#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% filter(in_spotify_playlists > in_apple_playlists) %>%
  filter(in_spotify_charts == 0, in_apple_charts > 0) %>%
  arrange(desc(in_spotify_playlists)) %>% select(track_name,artist.s._name) %>% 
  View()

## Odp. Było to 337 piosenek widocznych w tabeli.








#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?



df %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  filter(artist.s._name != "") %>% 
  group_by(artist.s._name) %>% 
  summarise(
    mean_solo = mean(as.numeric(streams[artist_count == 1]), na.rm = T),
    mean_more = mean(as.numeric(streams[artist_count >= 2]), na.rm = T),
  ) %>% 
  filter(mean_solo > mean_more) %>% arrange(-mean_solo) %>% 
  mutate(difference = mean_solo - mean_more) %>% 
  head(1) %>% select(artist.s._name)


## Odp. John Legend.
