library(tidyr)
library(dplyr)
library(stringr)
df <- read.csv('spotify-2023.csv')

setwd("C:\\Users\\HP\\R\\TWD_semestr3\\HW1")
df$streams <- as.numeric(df$streams) ### do dalszej pracy z tą kolumną
df$in_spotify_playlists <- as.numeric(df$in_spotify_playlists)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

zad_1 <- df %>%
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>% 
  select(streams) %>% 
  summarise(srednia_liczba_odtworzen = sum(streams) / n())

## Odp.     216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

wiecej_niz_2 <- df %>% 
  filter(artist_count > 2) %>% 
  select(artist_count, in_spotify_playlists) %>% 
  summarise(sum(in_spotify_playlists))

mniej_niz_2 <- df %>% 
  filter(artist_count <= 2) %>% 
  select(artist_count, in_spotify_playlists) %>% 
  summarise(sum(in_spotify_playlists))

zad_2 <- wiecej_niz_2[1,1] > mniej_niz_2[1,1] ### WYCHODZI FALSZ CZYLI PIOSENKI STWORZONE PRZEZ WIECEJ NIZ 2 ARTYSTOW NIE SA ZAWARTE W WIEKSZEJ LICZBIE PLAYLIST

## Odp. NIE

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
.
zad_3 <- df %>% 
  select(released_year, released_month, released_day) %>% 
  mutate(dzien_tygodnia = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>% 
  group_by(dzien_tygodnia) %>% 
  summarise(liczba_piosenek_w_dniach_tygodnia = n()) %>% 
  arrange(desc(liczba_piosenek_w_dniach_tygodnia)) %>% 
  top_n(1)
  
zad_3[1,1]
## Odp    PIATEK

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.


zad_4 <- df %>% 
  filter(artist_count == 1, released_year %in% c(2022, 2021)) %>% 
  select(artist.s._name, released_year) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(liczba_piosenek_na_rok = n()) %>% 
  pivot_wider(
    names_from = released_year,
    values_from = liczba_piosenek_na_rok
  ) %>% 
  filter(!is.na(`2022`) & !is.na(`2021`)) %>% ### rozpatruje tylko tych wykonawcow, ktorzy wydali piosenke i w 2022 i w 2021 roku
  mutate(procent = (`2022` - `2021`) * 100) %>% 
  arrange(desc(procent)) %>% 
  head(1)
  
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


zad_5 <- df %>% 
  select(artist.s._name, track_name, released_year, streams, danceability_.) %>% 
  arrange(desc(danceability_.)) %>% 
  top_n(n() / 10) %>% 
  mutate(srednia = streams / (2024 - released_year)) %>% # liczę od roku, od którego piosenka zostala wydana
  arrange(desc(srednia)) %>% 
  select(artist.s._name, track_name) %>% 
  head(1)
  
## Odp.    Piosenka artystów Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


zad_6 <- df %>% 
  mutate(piosenki = streams / in_spotify_playlists) %>% 
  arrange(desc(piosenki)) %>% 
  top_n(n() / 5) %>% 
  select(mode, bpm) %>% 
  mutate(srednie_tempo = sum(bpm) / n()) %>% 
  group_by(mode, srednie_tempo) %>% 
  summarise(mode_ilosc = n()) %>% 
  arrange(desc(mode_ilosc)) %>% 
  select(mode, srednie_tempo) %>% 
  head(1)
zad_6 #informacja o najczęściej występującej skali i srednim tempie (dla wszystkich utworow, bez rozroznienia na mode)

## Odp.     Minor, 125

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?


zad_7 <- df %>% 
  mutate(season = case_when(
    between(released_month, 3, 5)~"Wiosna",
    between(released_month, 6, 8)~"Lato",
    between(released_month, 9, 11)~"Jesien",
    TRUE ~ "Zima"
  )) %>% 
  select(season, 18:24) %>% 
  group_by(season) %>% 
  summarise(dance = sum(danceability_.) / n(),valence = sum(valence_.) / n(), energy = sum(energy_.) / n(), acoustinecness = sum(acousticness_.) / n(), instrumentalness = sum(instrumentalness_.) / n(), liveness = sum(liveness_.) / n(), speechiness = sum(speechiness_.) / n()  )
zad_7  

## Odp.
# season dance valence energy acoustinecness instrumentalness liveness speechiness
# <chr>  <dbl>   <dbl>  <dbl>          <dbl>            <dbl>    <dbl>       <dbl>
#   1 Jesien  65.3    46.2   62.2           26.7            2.02      18.0       10.2 
# 2 Lato    69.2    51.2   65.8           23.7            2.74      17.6        9.71
# 3 Wiosna  68.0    51.0   64.3           28.3            1.4       18.4       11.0 
# 4 Zima    65.7    56.1   64.7           28.5            0.596     18.7        9.50

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.


najpopularniejsze_key_mode <- df %>% 
  filter(released_year == 2022) %>% 
  select(key, mode) %>%
  group_by(key, mode) %>% 
  summarise(total_2022 = n()) %>% 
  arrange(desc(total_2022)) %>% 
  head(10)

solowi_artysci <- df %>% 
  filter(artist_count == 1 & released_year == 2022) %>% 
  select(key, mode) %>% 
  group_by(key, mode) %>% 
  summarise(total_solo = n())

zad_8 <- inner_join(solowi_artysci, najpopularniejsze_key_mode, by = c("key", "mode"))
zad_8 <- zad_8 %>% 
  arrange(desc(total_solo)) %>% 
  select(key, mode) %>% 
  head(1)

## Odp.   G     Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?


df$artist.s._name <-  str_remove(df$artist.s._name, "<ef>")
zad_9 <- df %>% 
  select(artist.s._name, streams) %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(total_stream = sum(streams)) %>% 
  arrange(desc(total_stream)) %>% 
  head(1)

 
## Odp. The Weeknd


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debiut <- df %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(released_year != 2023) %>% 
  select(artist.s._name, released_year) %>%
  group_by(artist.s._name, released_year) %>% 
  summarise(total_piosenki_rok = n()) %>% 
  pivot_wider(names_from = released_year,values_from = total_piosenki_rok)
debiut[is.na(debiut)] <- 0
debiut$suma <- rowSums(debiut[, -1]) #sprawdzam łąćzną ilość wypuszczonych piosenek i porównam z ilością wypuszczonych w 2022
debiut <- debiut %>% 
  filter(`2022` == suma) 
imiona <- debiut$artist.s._name  

zad_10 <- df %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  filter(artist.s._name %in% imiona) %>%
  mutate(key_mode_combination = str_c(key, mode, sep = " ")) %>% 
  select(artist.s._name, key_mode_combination) %>%
  group_by(artist.s._name, key_mode_combination) %>% 
  summarise(key_mode_total = n()) %>% 
  pivot_wider(names_from = key_mode_combination, values_from = key_mode_total)
  
zad_10[is.na(zad_10)] <- 0

## Odp. --- ramka 'Zad_10'

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?


zad_11 <- df %>% 
  select(track_name, in_spotify_charts, in_spotify_playlists, in_apple_charts, in_apple_playlists) %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name)
## Odp. Ramka 'zad_11'

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df$artist.s._name <-  str_remove(df$artist.s._name, "<ef>")
solo <- df %>% 
  filter(artist_count == 1) %>% 
  select(artist.s._name, streams) %>% 
  group_by(artist.s._name) %>% 
  summarise(average_stream_solo = sum(streams) / n()) %>% 
  arrange(desc(average_stream_solo))
 
grupa <- df %>% 
  filter(artist_count > 1) %>% 
  select(artist.s._name, streams) %>%  
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(average_stream_group = sum(streams) / n()) %>% 
  arrange(desc(average_stream_group))

zad_12 <- inner_join(grupa, solo, by = "artist.s._name")  
zad_12 <- zad_12 %>% 
  filter(average_stream_solo > average_stream_group) %>% 
  select(artist.s._name)
## Odp. Ramka 'zad_12'
