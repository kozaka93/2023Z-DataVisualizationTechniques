library(tidyr)
library(dplyr)

df <- read.csv('C:/Users/milos/Desktop/spotify-2023.csv')

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>%
  summarize(avg_streams = mean(as.integer(streams),na.rm = TRUE)) %>%
  unlist(use.names = FALSE)

## Odp.216150568 

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
  mutate(howMany = ifelse(artist_count %in% c(1,2),"oneOrTwo","moreThanTwo")) %>%
  group_by(howMany) %>% 
  summarize(n_playlist = sum(in_spotify_playlists))

## Odp.NIE

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?


df %>%
  mutate(weekDay = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>%
  group_by(weekDay) %>% 
  summarize(n = n()) %>% 
  top_n(1,n)

## Odp.Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>%
  filter(artist_count == 1 & released_year %in% c(2021,2022)) %>%
  group_by(artist.s._name,released_year) %>% 
  summarize(n_songs = n(), .groups = "drop") %>%
  pivot_wider(names_from = released_year, values_from = n_songs) %>% 
  filter(!is.na(`2021`) & !is.na(`2022`)) %>% 
  mutate(perc_incr = (`2022` - `2021`) / `2021` * 100) %>%
  top_n(1,perc_incr) %>%
  select(artist.s._name)

## Odp.SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>%
  filter(danceability_. > quantile(danceability_., .9)) %>%
  mutate(streamsPerYear = as.numeric(streams) / (2024 - released_year)) %>%
  filter(!is.na(streamsPerYear)) %>%
  top_n(1,streamsPerYear) %>% 
  select(artist.s._name)

## Odp.Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>%
   mutate(streamsPerPlaylist = as.numeric(streams) / in_spotify_playlists) %>%
   filter(streamsPerPlaylist > quantile(streamsPerPlaylist, .8, na.rm = T)) %>%
   mutate(avg_bpm = mean(bpm,na.rm=TRUE)) %>%
   group_by(mode,avg_bpm) %>%
   summarize(n_mode = n()) %>%
   ungroup() %>%
   top_n(1,n_mode) %>%
   select(mode,avg_bpm)

## Odp.Minor i Major 126

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

df %>%
   mutate(season = case_when(released_month < 3 | (released_month == 3 & released_day < 20) ~ "Zima",
                             released_month < 6 | (released_month == 6 & released_day < 21) ~ "Wiosna",
                             released_month < 9 | (released_month == 9 & released_day < 22) ~ "Lato",
                             released_month < 12 | (released_month == 12 & released_day < 22) ~ "Jesien",
                             TRUE ~ "Zima")) %>%
  group_by(season) %>%
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.))
                  
## Odp. Wiosna i lato bardziej taneczne i energiczne
## Zima tanecznosc i energicznosc
## Jesien taneczne i energiczne ale najnizsze ze wszystkich por roku

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
      filter(released_year == 2022) %>% 
      group_by(key,mode) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      top_n(10,n) %>%
      filter(artist_count == 1) %>%
      group_by(key,mode) %>%
      summarize(n_solo = n()) %>%
      top_n(1,n_solo) %>%
      select(key,mode)

## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>%
   separate_rows(artist.s._name, sep = ", ") %>%
   group_by(artist.s._name) %>%
   mutate(streams_int = as.numeric(streams)) %>%
   filter(!is.na(streams_int)) %>%
   summarize(totalStreams = sum(streams_int)) %>%
   top_n(1,totalStreams) %>%
   select(artist.s._name)

## Odp.The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df_separated <- df %>%
                separate_rows(artist.s._name, sep = ", ")

debute2022 <- df_separated %>%
                 group_by(artist.s._name) %>%
                 summarize(min_year = min(released_year)) %>%
                 filter(min_year == 2022)

debute2022 %>% 
           left_join(df_separated,join_by(artist.s._name)) %>%
           filter(artist.s._name != "") %>%
           group_by(artist.s._name,mode,key) %>%
           summarize(n = n()) %>%
           pivot_wider(names_from = c(mode,key),values_from = n, values_fill=0)


## Odp.



#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>%
  filter((in_spotify_playlists > in_apple_playlists) & in_spotify_charts == 0 & in_apple_charts > 0) %>%
  select(track_name)

## Odp.

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo <- df %>%
           mutate(streams_int = as.numeric(streams)) %>%
           filter(!is.na(streams_int) & artist_count == 1) %>%
           group_by(artist.s._name) %>%
           summarize(streamsPerSongSolo = sum(streams_int) / n())


group = df %>%
           separate_rows(artist.s._name, sep = ",\\s*") %>%
           filter(artist_count > 1) %>%
           mutate(streams_int = as.numeric(streams)) %>%
           filter(!is.na(streams_int)) %>%
           group_by(artist.s._name) %>%
           summarize(streamsPerSongGroup = sum(streams_int) / n())
           
group %>% 
      inner_join(solo,join_by(artist.s._name)) %>%
      filter(streamsPerSongSolo > streamsPerSongGroup) %>%
      select(artist.s._name) %>%
      pull(artist.s._name)


## Odp. 

