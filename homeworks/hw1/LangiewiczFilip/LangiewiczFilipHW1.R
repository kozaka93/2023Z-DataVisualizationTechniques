library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

df <- read.csv('spotify-2023.csv', fileEncoding = "ISO-8859-1")


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

pierwszy_kwartal <- c(1, 2, 3)

df %>% 
  filter(released_year == 2023 & released_month %in% pierwszy_kwartal) %>% 
  summarise(srednia = mean(as.numeric(streams))) %>% 
  select(srednia)
  
## Odp.216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

wiecej_niz_dwoch <- df %>% 
  filter(artist_count > 2) %>% 
  summarise(playlisty_spotify = sum(in_spotify_playlists))

mniej_niz_trzech <- df %>% 
  filter(artist_count %in% c(1, 2)) %>% 
  summarise(playlisty_spotify = sum(in_spotify_playlists))

wiecej_niz_dwoch > mniej_niz_trzech

## Odp. Nie 


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(date = paste(released_year, released_month, released_day, sep = "-")) %>% 
  mutate(dzien_tygodnia = wday(date, week_start = 1, abbr = FALSE, label = TRUE)) %>% 
  group_by(dzien_tygodnia) %>% 
  summarise(l_piosenek = n()) %>% 
  top_n(1, l_piosenek) %>% 
  select(dzien_tygodnia)

## Odp. piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

rok2021 <- df %>% 
  filter(artist_count == 1 & released_year == 2021) %>% 
  group_by(artist.s._name) %>% 
  summarise(piosenki2021 = n())  

rok2022 <- df %>% 
  filter(artist_count == 1 & released_year == 2022) %>% 
  group_by(artist.s._name) %>% 
  summarise(piosenki2022 = n()) 

inner_join(rok2021, rok2022, by = "artist.s._name") %>% 
  mutate(procentowy_wzrost = ((piosenki2022 / piosenki2021) - 1) * 100) %>% 
  top_n(1, procentowy_wzrost) %>% 
  select(artist.s._name)

## Odp. SZA (o 1600% wiecej piosenek)


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

prog <- quantile(df$danceability_., 0.9)
obecny_rok <- 2023
  
df %>% 
  filter(danceability_. > prog) %>% 
  mutate(lata = obecny_rok - released_year + 1) %>% 
  mutate(srednia = as.numeric(streams) / lata) %>%
  top_n(1, srednia) %>% 
  select(artist.s._name)

## Odp. Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

prog <- as.integer((2 / 10) *length(df$streams))

wyznaczDominante <- function(x) {
  czestosci <- table(x)
  names(czestosci)[which.max(czestosci)]
}

df %>% 
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>%   # bledne dane w tym wierszu w streams!
  mutate(najczesciej_odtwarzane = as.numeric(streams) / in_spotify_playlists) %>% 
  arrange(-najczesciej_odtwarzane) %>% 
  head(prog) %>% 
  summarise(srednie_tempo = mean(bpm), najczestsza_skala = wyznaczDominante(mode))

## Odp. srednie_tempo: 125.2,  najczestsza_skala: Minor


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
zima <- c(12, 1, 2)
wiosna <- c(3, 4, 5)
lato <- c(6, 7, 8)
jesien <- c(9, 10, 11)

df %>% 
  mutate(pora_roku = case_when(released_month %in% zima ~ "zima",
                               released_month %in% wiosna ~ "wiosna",
                               released_month %in% lato ~ "lato",
                               released_month %in% jesien ~ "jesien",
                               TRUE ~ "inna")) %>% 
  group_by(pora_roku) %>% 
  summarise(mean_bpm = mean(bpm),
            mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acousticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.),
            mean_speechiness = mean(speechiness_.))

# ## Odp.     pora_roku     mean_bpm    mean_danceability     mean_valence    mean_energy     mean_acousticness     mean_instrumentalness     mean_liveness     mean_speechiness
#               <chr>        <dbl>             <dbl>              <dbl>         <dbl>               <dbl>                   <dbl>                 <dbl>               <dbl>
#             1 jesien        121.              65.3              46.2          62.2                26.7                    2.02                  18.0                10.2 
#             2 lato          121.              69.2              51.2          65.8                23.7                    2.74                  17.6                9.71
#             3 wiosna        124.              68.0              51.0          64.3                28.3                    1.4                   18.4                11.0 
#             4 zima          124.              65.7              56.1          64.7                28.5                    0.596                 18.7                9.50


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

najpopularniejsze_pary <- df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(l_piosenek = n()) %>% 
  arrange(-l_piosenek) %>% 
  head(10) %>% 
  select(key, mode)

solo <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(key, mode) %>% 
  summarise(l_piosenek = n())

left_join(najpopularniejsze_pary, solo, by = c("key", "mode")) %>% 
  arrange(-l_piosenek)

## Odp. G - Major oraz "" - Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

z_kims <- df %>% 
  filter(artist_count != 1) %>% 
  separate_longer_delim(artist.s._name, delim = ", ")

tworcy_pojedynczo <- df %>% 
  filter(artist_count == 1) %>% 
  add_row(z_kims)

tworcy_pojedynczo %>% 
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>%   # bledne dane w tym wierszu w streams!
  group_by(artist.s._name) %>% 
  summarise(suma_odtworzen = sum(as.numeric(streams))) %>% 
  top_n(1, suma_odtworzen) %>% 
  select(artist.s._name)

## Odp. The Weeknd 


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

z_kims <- df %>% 
  filter(artist_count != 1) %>% 
  separate_longer_delim(artist.s._name, delim = ", ")

tworcy_pojedynczo <- df %>% 
  filter(artist_count == 1) %>% 
  add_row(z_kims) 

debiut_w_2022 <- tworcy_pojedynczo %>% 
  group_by(artist.s._name) %>% 
  summarise(debiut = min(released_year)) %>% 
  filter(debiut == 2022)

dane_debiutantow2022 <- tworcy_pojedynczo %>% 
  inner_join(debiut_w_2022, by = "artist.s._name")

result10 <- dane_debiutantow2022 %>% 
  group_by(artist.s._name, mode, key) %>% 
  summarise(wynik = n()) %>% 
  pivot_wider(names_from = c(key, mode), values_from = wynik, values_fill = 0, names_sep = "-")

## Odp. result10


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

result11 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name)

## Odp. result11 


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo <- df %>% 
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>%   # bledne dane w tym wierszu w streams!
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(odtworzenia_na_piosenke_solo = mean(as.numeric(streams)))

z_kims <- df %>% 
  filter(artist_count != 1) %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(odtworzenia_na_piosenke_z_kims = mean(as.numeric(streams)))

result12 <- inner_join(solo, z_kims, by = "artist.s._name") %>% 
  filter(odtworzenia_na_piosenke_solo > odtworzenia_na_piosenke_z_kims) %>% 
  select(artist.s._name)

# ## Odp. result12
