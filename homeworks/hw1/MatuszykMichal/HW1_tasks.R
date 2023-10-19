library(tidyr)
library(dplyr)

df <- read.csv('c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/hw1/spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?


df %>%                                                                          # Wczytaj dane
  filter(released_year == 2023, released_month %in% 1:3) %>%                    # Wybierz tylko te z Q1 2023
  mutate(streams = as.numeric(streams)) %>%                                     # Zmien typ kolumny streams (bez tego nie działa summarise)
  summarise(average_streams = mean(streams))


## Odp.216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  group_by(artist_count) %>%
  summarise(n = sum(in_spotify_playlists)) %>%
  mutate(artist_group = ifelse(artist_count> 2, "Reszta", "1-2")) %>%
  group_by(artist_group) %>% # Grupuje po grupach
  summarise(n = sum(n)) %>% # zlicza wszystkich
  arrange(desc(n)) %>% #sortuje
  slice_head(n = 1) # Zostawia tylko 1 wiersz - wiekszy

  

## Odp. Piosenki stworzone przez 1-2 artystów są zawarte w większej liczbie playlist spotify


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

head(df)
df %>%
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"), format = "%Y-%m-%d"),
         day_of_week = weekdays(date)) %>%
  group_by(day_of_week) %>%
  summarise(count = n()) %>%
  arrange(desc(count) )%>% #sortuje
  slice_head(n = 1) # Zostawia tylko 1 wiersz - wiekszy


## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>% 
  select(released_year, artist.s._name, artist_count) %>%
  filter(released_year %in% 2021:2022 & artist_count == 1) %>%
  group_by(artist.s._name, released_year) %>% summarise(songs_released = n()) %>%
  pivot_wider(names_from = released_year, values_from = songs_released) %>%
  mutate(percentage_change = ((`2022` - `2021`) / `2021`) * 100) %>%
  na.omit() %>%
  arrange(desc(percentage_change)) %>% head(n = 1)


## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

head(df)
df %>% filter(danceability_. > 90 & artist_count == 1) %>% select(artist.s._name, released_year, streams) %>% 
  mutate(streams = as.numeric(streams)) %>%
  mutate(streams_per_year = streams/(2024-released_year)) %>% 
  arrange(desc(streams_per_year)) %>% 
  slice_head(n = 10)


## Odp. Piosenka ROSAL

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

temp <- df %>% na.omit()%>% mutate(streams_per_playlist = as.numeric(streams)/as.numeric(in_spotify_playlists)) %>%
  arrange(desc(streams_per_playlist)) %>%
  top_n(n = nrow(.) * 0.20) %>% select(bpm, mode)

average_tempo <- mean(temp$bpm)
average_tempo
most_freq_mode <- temp %>% 
  group_by(mode) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  head(n = 1)
most_freq_mode

## Odp. Tempo=125.2; most_freq_mode = minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>%
  mutate(
    season = case_when(
      between(released_month, 3, 5) ~ "Spring",
      between(released_month, 6, 8) ~ "Summer",
      between(released_month, 9, 11) ~ "Autumn",
      TRUE ~ "Winter"
    )
  ) %>%
  group_by(season) %>% 
  summarise(avg_dannceability = mean(danceability_.),
            avg_valence = mean(valence_.),
            avg_energy = mean(energy_.),
            avg_acousticness = mean(acousticness_.),
            avg_insttrumentalness = mean(instrumentalness_.),
            avg_liveness = mean(liveness_.),
            avg_speechiness = mean(speechiness_.),
            )

## Odp.   season avg_dannceability avg_valence avg_energy avg_acousticness avg_insttrumentalness avg_liveness avg_speechiness
#         <chr>              <dbl>       <dbl>      <dbl>            <dbl>                 <dbl>        <dbl>           <dbl>
#         1 Autumn              65.3        46.2       62.2             26.7                 2.02          18.0           10.2
#         2 Spring              68.0        51.0       64.3             28.3                 1.4           18.4           11.0
#         3 Summer              69.2        51.2       65.8             23.7                 2.74          17.6            9.71
#         4 Winter              65.7        56.1       64.7             28.5                 0.596         18.7            9.50

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% filter(artist_count == 1) %>% group_by(key, mode) %>% summarise(count = n()) %>% filter(key != "") %>% arrange(desc(count)) %>% head(1)

## Odp. key   mode  count
      # <chr> <chr> <int>
      # 1 "G"   Major    47

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>% filter(artist_count == 1) %>% group_by(artist.s._name) %>% summarise(total_streams = sum(as.numeric(streams))) %>%
  select(artist.s._name, total_streams) %>% arrange(desc(total_streams)) %>% head(n = 1) 


## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df$release_date <- as.Date(paste(df$released_year, df$released_month, df$released_day, sep = "-"))

# Group by artist and find the earliest release date
artist_debut_df <- df %>%
  group_by(artist.s._name) %>%
  summarize(earliest_release = min(released_year)) %>%
  ungroup()

artists_debuted_in_2022 <- artist_debut_df %>%
  filter(earliest_release == 2022)

wynik10 <- df %>% filter(artist.s._name %in% artists_debuted_in_2022$artist.s._name) %>% 
  group_by(artist.s._name, mode, key) %>% 
  summarise(song_number = n()) %>% 
  pivot_wider(names_from = c(mode, key), values_from = song_number, values_fill = 0)



## Odp. wynik10
# Za duża aby wkleic: # A tibble: 243 × 25



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% filter(in_spotify_playlists>in_apple_playlists) %>% filter(in_spotify_charts == 0 & in_apple_charts>0) %>% select(track_name)

## Odp. Za dużo aby wszystkie wypisać (337 jest)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo_artysci <- df%>% filter(artist_count == 1) %>% select(artist.s._name) %>% distinct()

solo_averages <- df %>% 
  filter(artist.s._name %in% solo_artysci$artist.s._name)%>%
  filter(!is.na(streams)) %>%
  group_by(artist.s._name) %>%
  summarise(average_streams_solo = sum(as.numeric(streams), na.rm = T) / n())

# solo_averages

# averages %>% left_join(solo_averages, by = "artist.s._name") %>% filter(average_streams<average_streams_solo)

multiple_artists <- df %>% filter(artist_count>1) %>% select(artist.s._name, artist_count, streams)%>%
  mutate(artist.s._name = iconv(artist.s._name, to = "UTF-8", sub = "")) %>% separate_rows(artist.s._name, sep=", ", convert = T) %>%
  group_by(artist.s._name) %>%  summarise(average_streams_as_co_artist = sum(as.numeric(streams), na.rm = T)/n())

solo_averages %>% left_join(multiple_artists, by = "artist.s._name") %>% na.omit() %>% filter(average_streams_solo > average_streams_as_co_artist) %>% arrange(desc(average_streams_solo))


## Odp. Najwięcej John Legedend, ale jest 56 takich artystów.



