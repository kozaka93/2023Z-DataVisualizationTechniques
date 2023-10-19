library(tidyr)
library(dplyr)
library(stringi)


# setwd('~/Desktop/pw/3 sem/techniki_wizualizacji_danych/WieteckiMichal/')

df <- read.csv('spotify-2023.csv')
df <- df[-575,] # usuwam ten wiersz od rrazu poniewaz jest w nim blad w wierszu 


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == "2023", released_month %in% c("1", "2", "3")) %>%
  transmute(streams = as.integer(streams)) %>% 
  summarize(mean_streams = mean(streams))
  
## Odp. 216150568 


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  transmute(artist_count = case_when(artist_count == "1" ~ "1/2",
                                     artist_count == "2" ~ "1/2",
                                     TRUE ~ "3 or more"),
            playlist_count = as.integer(in_spotify_playlists)) %>% 
  group_by(artist_count) %>% 
  summarise(sum_playlist_count = sum(playlist_count, na.rm = TRUE))
  
  

## Odp. NIE


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(date = as.Date(with(df,paste(released_year, released_month,
                                      released_day, sep="-")),"%Y-%m-%d"),
         day_of_week = weekdays(date)) %>% 
  group_by(day_of_week) %>% 
  summarise(count_day = n()) %>% 
  top_n(1, count_day) %>%
  select(day_of_week)


## Odp. Friday

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  filter(released_year %in% c("2021", "2022")) %>% 
  group_by(artist.s._name) %>% 
  summarise(r_2021 = sum(ifelse(released_year == "2021", 1, 0)),
            r_2022 = sum(ifelse(released_year == "2021", 0, 1))) %>% 
  filter(r_2021 != 0, r_2022 != 0) %>% 
  transmute(artist.s._name, growth = ((r_2022 - r_2021)/r_2021)*100) %>% 
  top_n(1, growth)

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
  arrange(-danceability_.) %>% 
  head(nrow(df)/10) %>% 
  transmute(track_name, artist.s._name,
            views_per_year = as.integer(streams)/(2024-as.integer(released_year))) %>% 
  top_n(1, views_per_year)
  
## Odp.

#   track_name                        artist.s._name views_per_year
# 1 Me Porto Bonito Chencho Corleone, Bad Bunny      720378909

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% #wybieram bez wiersza 575 poniewaz jest w nim blad w kol streams
  filter(in_spotify_playlists != 0) %>% 
  transmute(track_name, bpm = as.integer(bpm), mode,
            ratio = as.integer(streams)/in_spotify_playlists) %>% 
  arrange(-ratio) %>% 
  head(nrow(df)/5) -> A #biore 20% pierwszych wierszy

mean(A$bpm) # srednie tempo

sort(table(A$mode), decreasing = TRUE)[1] # biore najczesciejszy klucz


## Odp: srednie tempo: 125.2, najczesciej wystepujaca skala: minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>% 
  mutate(season = case_when(((released_month == 3 & released_day >= 21) |
                               (released_month == 4) | 
                               (released_month == 5) | 
                               (released_month == 6 & released_day < 22)) ~ "spring",
                            ((released_month == 6 & released_day >= 22) | 
                               (released_month == 7) |
                               (released_month == 8) |
                               (released_month == 9 & released_day < 23)) ~ "summer",
                            ((released_month == 9 & released_day >= 23) |
                               (released_month == 10) |
                               (released_month == 11) |
                               (released_month == 12 & released_day < 22)) ~ "autumn",
                            TRUE ~ "winter")) %>%
  group_by(season) %>% 
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.))
  

## Odp.

# season   mean_danceability mean_valence mean_energy mean_acousticness mean_instrumentalness mean_liveness
# <chr>               <dbl>        <dbl>       <dbl>             <dbl>                 <dbl>         <dbl>
# 1 autumn              63.9         47.0        61.2              29.7                 1.96           17.6
# 2 spring              68.6         49.5        64.2              27.7                 1.67           18.3
# 3 summer              68.6         51.5        65.8              24.7                 2.17           18.0
# 4 winter              66.7         57.5        66.0              25.7                 0.730          18.7

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
  filter(released_year == 2022) %>% 
  select(key, mode, artist_count) %>%
  group_by(key, mode) %>%
  summarise(counter = n(),
            solo_frac = sum(ifelse(artist_count == 1, 1, 0))/n()) %>% 
  arrange(-counter) %>%
  head(10) %>%
  arrange(-solo_frac) %>%
  head(1)



## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df8 <- df %>% 
  select(artist.s._name, streams)
  
df8 <- separate_longer_delim(df8, artist.s._name, ", ")

df8 %>% 
  group_by(artist.s._name) %>% 
  summarise(all_streams = sum(as.numeric(streams))) %>% 
  arrange(-all_streams) %>% 
  head(1)

## Odp. The Weeknd     23929760757

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

#tworze tabele gdzie znajduja sie artysci debuitujacy w 2022

artists <- df %>%
  select(artist.s._name, released_year) %>% 
  separate_longer_delim(artist.s._name, ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(debut = min(released_year)) %>%
  filter(debut == 2022) %>%
  select(artist.s._name)

#tworze tabele z ktorej bede twoerzyl dwie kolejne dla mode i key

df %>% 
  separate_longer_delim(artist.s._name, ", ") %>% 
  filter(artist.s._name %in% artists$artist.s._name) -> df10

# tworze tabele dla mode

df10 %>% 
  select(artist.s._name, mode) %>% 
  group_by(artist.s._name, mode) %>% 
  summarise(count_mode = n()) %>% 
  pivot_wider(names_from = mode, values_from = count_mode, values_fill = 0) -> df_10_1

# tworze tabele dla key (trzeba zamienic pusta wartosc klucza na cos bo 
# funkcja pivot wider nie dziala inaczej)

df10 %>% 
  select(artist.s._name, key) %>% 
  mutate(new_key = ifelse(key == "", "-", key)) %>% 
  group_by(artist.s._name, new_key) %>% 
  summarise(count_key = n()) %>% 
  pivot_wider(names_from = new_key, values_from = count_key, values_fill = 0) -> df_10_2

# lacze dwie tabele razem

last <- merge(df_10_1,df_10_2, by = "artist.s._name")


## Odp. tabela ktora ma 265 wierszow czyli arytow znajduje sie ponizej

last

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  select(track_name, in_spotify_playlists, in_apple_playlists,
         in_spotify_charts, in_apple_charts) %>% 
  filter((in_spotify_playlists > in_apple_playlists) &
           (in_spotify_charts == 0) & (in_apple_charts != 0))

## Odp. Jest takich piosenek aż 337


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?


df %>% #znowu biore tabele bez wiersza 575 aby ominac blad w streams
  select(artist.s._name, artist_count, streams) %>% 
  separate_longer_delim(artist.s._name, ", ") %>% 
  transmute(artist.s._name, 
            ifsolo = ifelse(artist_count == 1, "solo", "not_solo"),
            streams = as.numeric(streams)) %>% 
  group_by(artist.s._name, ifsolo) %>% 
  summarise(mean_streams = mean(streams, na.rm = TRUE)) %>% 
  pivot_wider(names_from = ifsolo, values_from = mean_streams) %>% 
  filter(!is.na(solo)&!is.na(not_solo), solo>not_solo) -> df_12

df_12

## Odp. takich artystow jest 58
# zalozylem, ze bede liczyl tylko artystow ktorzy tworzyli i solo i nie solo, 
# wiec przefiltrowalem i usunal wiersze gdzie ktorys artysta mial NA (nie mial 
# zadnej piosenki solo lub nie solo przez co przy sredniej stworzyly sie NA).
# Gdybym takie przypadki tez liczyl musialbym po prostu dodac argument 
# values_fill = 0 w pivot_wider i przefiltowac bez warunku z NA



