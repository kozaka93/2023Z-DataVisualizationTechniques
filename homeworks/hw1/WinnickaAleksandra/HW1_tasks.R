library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')
df$streams <- as.integer(df$streams)

View(df)
names(df)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

## Odp. 216150568

mean(df[df$released_year == 2023 & df$released_month %in% c(1,2,3), c('streams')])

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

## Odp. nie

więcej_niz_dwoch <- df %>%
  filter(artist_count > 2) %>%
  summarize(few_artists=sum(in_spotify_playlists))

dwoch_lub_jedego <- df %>%
  filter(artist_count <= 2) %>%
  summarize(more_artists=sum(in_spotify_playlists))
więcej_niz_dwoch > dwoch_lub_jedego

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

## Odp. piątek

df%>%
  mutate(najpopuarniejszy_dzien = weekdays(as.Date(paste(released_year,released_month, released_day, sep = "-"))))%>%
  group_by(najpopuarniejszy_dzien)%>%
  summarise(n = n())%>%
  arrange(-n)%>%
  head(1)

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? 
#### (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.


## Odp. SZA

filtered_data <- df %>%
  filter(artist_count == 1, released_year %in% c(2021, 2022)) %>%
  group_by(artist.s._name, released_year) %>%
  summarise(n = n()) 

pivot_data <- filtered_data %>%
  pivot_wider(names_from = released_year, values_from = n) %>%
  mutate(percentage_increase = ((`2022` - `2021`) / `2021`) * 100)
artist_with_largest_increase <- pivot_data %>%
  arrange(desc(percentage_increase)) %>%
  head(1)
View(artist_with_largest_increase)

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.


## Odp. Chencho Corleone, Bad Bunny

najwiecej_odtworzen <- df %>%
  filter(!is.na(streams) & !grepl("[^0-9.]", streams)) %>%
  mutate(streams = as.numeric(streams),
         plays_per_year = streams / (2023 - released_year + 1)) %>%
  filter(danceability_. >= quantile(danceability_., 0.9)) %>%
  group_by(artist.s._name) %>%
  summarize(avg_plays_per_year = mean(plays_per_year, na.rm = TRUE)) %>%
  arrange(desc(avg_plays_per_year)) %>%
  head(1)
View(najwiecej_odtworzen)


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

## Odp. 124. Minor

df %>% 
  mutate(playspotify = as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(-playspotify) %>% 
  head(nrow(df)/5) %>%
  group_by(mode) %>% 
  summarise(mean_bpm = mean(bpm), number_of_songs = n()) %>% 
  arrange(-number_of_songs) %>% 
  head(1) %>% 
  select(mean_bpm, mode)

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

## Odp.

charakterystyki <- df%>%
  mutate(Season = case_when(
    released_month %in% c(3, 4, 5) ~ "Wiosna",
    released_month %in% c(6, 7, 8) ~ "Lato",
    released_month %in% c(9, 10, 11) ~ "Jesień",
    released_month %in% c(12, 1, 2) ~ "Zima"
  ))

seasonal_stats <- charakterystyki %>%
  group_by(Season) %>%
  summarise(
    mean_danceability = mean(danceability_.),
    mean_energy = mean(energy_.),
    mean_valence = mean(valence_.),
    mean_acousticness = mean(acousticness_.),
    mean_instrumentalness = mean(instrumentalness_.),
    mean_liveness = mean(liveness_.),
    mean_speechiness = mean(speechiness_.)
  )

View(seasonal_stats)


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

## Odp. G Major 

pary <- df %>%
  filter(released_year == 2022) %>%
  filter(artist_count == 1) %>%
  group_by(key, mode) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  arrange(desc(count)) %>%
  head(1)

View(pary)

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

## Odp. (G)I-DLE


df_filtered2 <- df %>%
  filter(!is.na(select(df, artist.s._name)), !is.na(select(df, streams)))

artist_streams <- df_filtered2 %>%
  group_by(select(df, artist.s._name)) %>%
  summarise(total_streams = sum(select(df, streams))) %>%
  arrange(desc(total_streams))

top_artist <- artist_streams[1,]

View(top_artist)


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

## Odp.

debiut2022 <- df %>%
  filter(released_year == 2022) %>% 
  group_by(artist.s._name, key, mode) %>%
  summarize(total_songs = n()) %>%
  ungroup()
View(debiut2022)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
## Odp.

# Wybierz tylko piosenki, które były popularniejsze na Spotify niż na Apple Music

popularniejsze_na_spotify <- df %>%
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == "Yes", in_apple_charts > 0) %>%
  select(artist.s._name)
View(popularniejsze_na_spotify)

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

## Odp.
  
  
  