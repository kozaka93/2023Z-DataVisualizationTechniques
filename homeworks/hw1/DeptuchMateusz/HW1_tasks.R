library(tidyr)
library(dplyr)

df <- read.csv('/Users/mateuszdeptuch/RStudio/TWD/pd1/spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df%>%
  filter(released_year == 2023 & (released_month %in% c(1,2,3))) %>%
  summarise(mean = mean(strtoi(streams)))

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

more_than_2 <- df%>%
  filter(artist_count > 2)%>%
  summarise(sum(in_spotify_playlists))
less_equal_2 <- df%>%
  filter(artist_count <=2)%>%
  summarise(sum(in_spotify_playlists))
more_than_2[1,1]>less_equal_2[1,1]

## Odp. FALSE


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df%>%
  mutate(weekday = weekdays(as.Date(paste(released_year,released_month, released_day, sep = "-"))))%>%
  group_by(weekday)%>%
  summarise(n = n())%>%
  arrange(-n)%>%
  head(1)


## Odp. Friday

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df%>%
  group_by(artist.s._name)%>%
  filter(released_year == 2021 | released_year == 2022)%>%
  filter(2021 %in% released_year & 2022 %in% released_year)%>%
  group_by(artist.s._name,released_year)%>%
  summarise(number_of_released_songs = n())%>%
  pivot_wider(names_from = released_year, values_from = number_of_released_songs)%>%
  mutate(percent_increase = (`2022`-`2021`)/`2021`*100)%>%
  arrange(-percent_increase)%>%
  head(1)
  
  

## Odp. SZA (1700% w porownaniu do 2021)

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df%>%
  top_n(as.integer(nrow(df)/10), danceability_.) %>% #tu jest odrobine wiecej niz 10%, ale dlatego, że sie powtarza ostatni wynik duzo razy
  mutate(how_many_years = 2024 - released_year)%>%
  mutate(plays_per_year = strtoi(streams)/how_many_years)%>%
  arrange(-plays_per_year)%>%
  head(1)%>%
  select(artist.s._name)

## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

  data <- df%>%
    mutate(stream_per_playlist = strtoi(streams) / in_spotify_playlists)%>%
    arrange(-stream_per_playlist)%>%
    head(round(nrow(df) * 0.2))
  
  average_bpm = summarise(data, mean(bpm))
  
  most_common_mode = data%>%
    group_by(mode)%>%
    summarise(n = n())%>%
    arrange(-n)%>%
    head(1)
    

## Odp. ~125bpm, Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

  
  
  df %>%
    mutate(season = case_when(
      (released_month * 100 + released_day) %in% 321:621 ~ "Spring",
      (released_month * 100 + released_day) %in% 622:922 ~ "Summer",
      (released_month * 100 + released_day) %in% 923:1221 ~ "Autumn",
      (released_month * 100 + released_day) %in% c(1222:1231, 101:320) ~ "Winter"
    ))%>%
  group_by(season)%>%
  summarise(danceability = mean(danceability_.),
            valence = mean(valence_.),
            energy = mean(energy_.),
            acousticness = mean(acousticness_.),
            instrumentalness = mean(instrumentalness_.),
            liveness = mean(liveness_.),
            speechiness = mean(speechiness_.)
            
            )
  
  
## Odp.

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df%>%
  filter(released_year == 2022)%>%
  group_by(key,mode)%>%
  summarise(n = n(), number_of_solo_artists = sum(artist_count == 1))%>%
  arrange(-n)%>%
  head(10)%>%
  arrange(-number_of_solo_artists)%>%
  head(1)


## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df%>%
  mutate(artist_name = iconv(artist.s._name, to = "UTF8"))%>%
  separate_rows(artist_name, sep = ", ")%>%
  group_by(artist_name)%>%
  summarise(total_streams = sum(strtoi(streams), na.rm = TRUE))%>%
  arrange(-total_streams)%>%
  head(1)


## Odp. Bad bunny

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df_with_artist <- df%>%
  mutate(artist_name = iconv(artist.s._name, to = "UTF8"))%>%
  separate_rows(artist_name, sep = ", ")

debut_2022 <- df_with_artist%>%
  group_by(artist_name)%>%
  summarise(debut_date = min(released_year))%>%
  filter(debut_date == 2022 & artist_name != "")%>%
  select(artist_name)

artists_keys <- df_with_artist%>%
  filter(artist_name %in% debut_2022$artist_name)%>%
  group_by(artist_name,key)%>%
  summarise(key_count = n())%>%
  filter(key != "")%>%
  pivot_wider(names_from = key, values_from = key_count, values_fill = 0)

artists_modes <- df_with_artist%>%
  filter(artist_name %in% debut_2022$artist_name)%>%
  group_by(artist_name,mode)%>%
  summarise(mode_count = n())%>%
  pivot_wider(names_from = mode, values_from = mode_count, values_fill = 0)
res <- artists_keys%>%
  right_join(artists_modes, by = "artist_name") #niektorzy z artystow nie maja danych o "key"
  


## Odp. res



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

x <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists)%>%
  filter(in_spotify_charts == 0 & in_apple_charts != 0)


## Odp. 337 piosenek


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
res <- df%>%
  mutate(artist_name = iconv(artist.s._name, to = "UTF8"))%>%
  separate_rows(artist_name, sep = ", ")%>%
  mutate(artist_count = ifelse(artist_count == 1, "solo", "more"))%>%
  group_by(artist_name, artist_count)%>%
  summarise(average_streams_per_song = mean(strtoi(streams),na.rm = TRUE))%>%
  pivot_wider(names_from = artist_count, values_from = average_streams_per_song, values_fill = 0)%>%
  filter(solo>more & solo != 0 & more != 0 & !is.na(artist_name))

## Odp. 57 artystow, np Nicki Minaj 



