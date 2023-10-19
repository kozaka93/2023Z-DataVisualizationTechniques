library(tidyr)
library(dplyr)

df <- read.csv("C:/studia/2rok/TWD/homeworkSpotify/spotify-2023.csv")

df


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  select(track_name, artist.s._name, streams, released_year, released_month) %>% 
  filter(released_year == 2023 & released_month < 4) %>% 
  summarise(SredniaOdtworzen = mean(as.numeric(streams)))

## Odp.
# Średnia ta wynosi 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

wiecejNiz2artystow <- df %>% 
  select(track_name, artist.s._name, artist_count, in_spotify_playlists) %>% 
  filter(artist_count > 2) %>% 
  summarise(LiczbaNaPlaylistach = sum(in_spotify_playlists))

jedenLubDwochArtystow <- df %>% 
  select(track_name, artist.s._name, artist_count, in_spotify_playlists) %>% 
  filter(artist_count == 2 | artist_count == 1) %>% 
  summarise(LiczbaNaPlaylistach = sum(in_spotify_playlists))
jedenLubDwochArtystow
wiecejNiz2artystow

## Odp.
#Piosenki stworzone przez 1 lub 2 artystów są na większej liczbie playlist spotify

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(DzienTygodnia = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(DzienTygodnia = format(DzienTygodnia, "%A")) %>% 
  group_by(DzienTygodnia) %>%
  summarise(LiczbaWydanWDanymDniuTygodnia = n(), .groups = "keep") %>% 
  arrange(-LiczbaWydanWDanymDniuTygodnia)

## Odp.
# Najpopularniejszym dniem tygodnia do wypuszczania piosenek jest piątek (526)

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

wydania2021 <- df %>%
  filter(released_year == 2021 & artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarize(liczbaPiosenek2021 = n())

wydania2022 <- df %>%
  filter(released_year == 2022 & artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarize(liczbaPiosenek2022 = n())

porownanie <- wydania2021 %>%
  inner_join(wydania2022,by= "artist.s._name") %>%
  mutate(wzrost = (liczbaPiosenek2022/liczbaPiosenek2021)*100)%>%
  arrange(-wzrost) %>% 
  head(1)

## Odp.
# Największy wzrost: SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>%
  select(track_name, artist.s._name, danceability_., streams,released_year) %>%
  filter(danceability_. > quantile(danceability_., 0.9)) %>%
  mutate(sredniaOdtworzenNaRok = as.numeric(streams) / (2024 - released_year)) %>% 
  arrange(-sredniaOdtworzenNaRok) %>%
  head(1)

## Odp.
# Me Porto Bonito Chencho Corleone, Bad Bunny 

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
  select(bpm, streams, in_spotify_playlists) %>%
  mutate(streamyNaPlayliste = as.numeric(streams) / in_spotify_playlists) %>%
  filter(streamyNaPlayliste > quantile(streamyNaPlayliste, 0.8, na.rm = TRUE)) %>%
  summarise(srednieTempo = mean(bpm))

df %>% 
  select(mode, streams, in_spotify_playlists) %>%
  mutate(streamyNaPlayliste = as.numeric(streams) / in_spotify_playlists) %>%
  filter(streamyNaPlayliste >= quantile(streamyNaPlayliste, 0.8, na.rm = TRUE)) %>%
  group_by(mode)%>%
  summarize(najczestyszyMode = n())%>%
  arrange(-najczestyszyMode) %>% 
  head(1)

## Odp.
# tempo: 125.2775 mode: minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>%
  mutate(season = case_when(
    released_month %in% c(12,1,2) ~ "zima",
    released_month %in% c(3,4,5) ~ "wiosna",
    released_month %in% c(6,7,8) ~ "lato",
    released_month %in% c(9,10,11) ~ "jesien")) %>% 
  group_by(season)%>%
  summarize(mean_danceability = mean(danceability_., na.rm = TRUE),
            mean_energy = mean(energy_., na.rm = TRUE), 
            mean_liveness = mean(liveness_., na.rm = TRUE),
            mean_valance = mean(valence_., na.rm = TRUE),
            mean_accousticnes = mean(acousticness_., na.rm = TRUE),
            mean_instrumentalness = mean(instrumentalness_., na.rm = TRUE),
            mean_speechiness = mean(speechiness_., na.rm = TRUE)) 
  
## Odp.
# season mean_danceability mean_energy mean_liveness mean_valance mean_accousticnes mean_instrumentalness mean_speechiness
#   1 jesien              65.3        62.2          18.0         46.2              26.7                 2.02             10.2 
# 2 lato                69.2        65.8          17.6         51.2              23.7                 2.74              9.71
# 3 wiosna              68.0        64.3          18.4         51.0              28.3                 1.4              11.0 
# 4 zima                65.7        64.7          18.7         56.1              28.5                 0.596             9.50

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>%
  select(released_year, artist_count, key, mode) %>%
  filter(released_year == 2022, artist_count == 1) %>%
  count(key, mode) %>%
  arrange(-n) %>% 
  head(1)
  
## Odp.
#G Majorwystępuje 25 razy
#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  mutate(artysci = str_split(artist.s._name, ", ")) %>%
  unnest(artysci) %>%
  filter(artist.s._name != "") %>% 
  select(artysci, streams) %>%
  group_by(artysci) %>% 
  summarise(sumaOdsluchan = sum(as.numeric(streams),na.rm = TRUE)) %>% 
  arrange(-sumaOdsluchan) %>% 
  head(1)

## Odp.
# Najlepszy na świecie artysta :) The Weeknd  liczba odsłuchań: 23929760757

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debiutanci <- df %>%
  mutate(artysci = str_split(artist.s._name, ", ")) %>%
  unnest(artysci) %>%
  filter(artysci != "") %>% 
  group_by(artysci) %>%
  summarize(debiutArtysty = min(released_year)) %>%
  filter(debiutArtysty == 2022)

df %>% 
  mutate(artysci = str_split(artist.s._name, ", ")) %>%
  unnest(artysci) %>%
  inner_join(debiutanci, by = "artysci") %>% 
  mutate(mode_key = paste(mode, key)) %>%
  group_by(artysci, mode_key) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "mode_key",values_from = "n",values_fill = 0) %>%
  View()
  
## Odp. tabelka wyżej


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists > in_apple_playlists & in_spotify_charts == 0 & in_apple_charts > 0) %>% 
  select(track_name, artist.s._name, in_spotify_playlists, in_apple_playlists, in_spotify_charts, in_apple_charts) %>% 
  View()

## Odp. takich piosenke jest 337, tabelka wyżej


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
  mutate(artysci = str_split(artist.s._name, ", ")) %>%
  unnest(artysci) %>%
  filter(artysci != "") %>% 
  mutate(SoloCzyzKims = if_else(artist_count == 1,
                     "solo",
                     "feat"))%>%
  group_by(artysci, SoloCzyzKims) %>%
  summarize(sredniaOdsluchow = mean(as.numeric(streams), na.rm = TRUE))%>%
  pivot_wider(names_from = SoloCzyzKims, values_from = sredniaOdsluchow)%>%
  filter(solo > feat & !is.na(solo) & !is.na(feat)) %>% 
  View()

## Odp.
# takich artystów jest 58, tabelka wyżej