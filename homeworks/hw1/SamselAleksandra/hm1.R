library(tidyr)
library(dplyr)


df <- read.csv('spotify-2023.csv')
str(df)


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
        filter(released_year == 2023, released_month < 4) %>% 
        transform(streams = as.numeric(streams)) %>% 
        summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
        mutate(how_many_artists = ifelse(artist_count > 2, "> 2", "1 - 2")) %>% 
        group_by(how_many_artists) %>% 
        summarise(how_many_spotify_playlists = sum(in_spotify_playlists))

## Odp. Piosenki stworzone przez więcej niż 2 artystów są zawarte na mniejszej liczbie playlist spotify niż
## piosenki stworzone przez 1 lub 2 artystów


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
        mutate(date = as.Date(paste(released_year, 
                            released_month, 
                            released_day, sep="-"))) %>% 
        mutate(day_of_the_week = weekdays(date)) %>% 
        group_by(day_of_the_week) %>% 
        summarise(number_of_songs = n()) %>% 
        arrange(desc(number_of_songs)) %>% 
        head(1)
        
## Odp. Piątek


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? 
#### (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2022 i w 2021.

df %>% 
        filter(artist_count == 1, released_year %in% c(2021, 2022)) %>% 
        group_by(artist.s._name, released_year) %>% 
        summarise(number_of_songs = n()) %>% 
        spread(released_year, number_of_songs, sep = "_") %>% 
        drop_na() %>% 
        mutate(growth_2022_vs_2021 = (released_year_2022 - released_year_2021) / released_year_2021) %>% 
        arrange(desc(growth_2022_vs_2021)) %>% 
        head(1)

## Odp. SZA


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
#### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

df %>% 
        arrange(desc(danceability_.)) %>% 
        head(nrow(df)/10) %>%
        mutate(year = as.numeric(released_year), streams = as.numeric(streams)) %>% 
        mutate(mean_streams_per_year = streams / (2024 - year)) %>% 
        arrange(desc(mean_streams_per_year)) %>% 
        select(artist.s._name, track_name, mean_streams_per_year, streams, year) %>% 
        head(1)

## Odp. Chencho Corleone, Bad Bunny


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
        mutate(streams_per_spotify_playlist = as.numeric(streams)/in_spotify_playlists) %>% 
        arrange(desc(streams_per_spotify_playlist)) %>% 
        head(nrow(df)/5) %>%
        group_by(mode) %>% 
        summarise(mean_bpm = mean(bpm), number_of_songs = n()) %>% 
        arrange(desc(number_of_songs)) %>% 
        head(1) %>% 
        select(mean_bpm, mode)

## Odp. Piosenki które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist na spotify
## charakteryzują się średnim tempem równym 124 i najczęściej wystęoującą skalą jest Minor


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
        
df %>% 
        mutate(date = as.Date(paste(released_year, 
                                    released_month, 
                                    released_day, sep="-"))) %>% 
        mutate(season = case_when(
                (date >= as.Date(paste(released_year, 1, 1, sep="-")) & date < as.Date(paste(released_year, 3, 21, sep="-"))) ~ "Winter",
                (date >= as.Date(paste(released_year, 3, 21, sep="-")) & date < as.Date(paste(released_year, 6, 22, sep="-"))) ~ "Spring",
                (date >= as.Date(paste(released_year, 6, 22, sep="-")) & date < as.Date(paste(released_year, 9, 23, sep="-"))) ~ "Summer",
                (date >= as.Date(paste(released_year, 9, 23, sep="-")) & date < as.Date(paste(released_year, 12, 22, sep="-"))) ~ "Autumn",
                (date >= as.Date(paste(released_year, 12, 22, sep="-")) & date < as.Date(paste(released_year, 12, 31, sep="-"))) ~ "Winter"
               )) %>% 
        select(season, last_col(8):last_col(2)) %>% 
        group_by(season) %>% 
        summarise(across(everything(), list(mean)))
        
## Odp. 
## season   danceability_._1 valence_._1 energy_._1 acousticness_._1 instrumentalness_._1 liveness_._1 speechiness_._1
## 1 Autumn             63.9        47.0       61.2             29.7                1.96          17.6            9.84
## 2 Spring             68.6        49.5       64.2             27.7                1.67          18.3           11.0 
## 3 Summer             68.6        51.5       65.8             24.7                2.17          18.0           10   
## 4 Winter             66.7        57.5       66.0             25.7                0.730         18.7            9.56


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

df %>% 
        filter(released_year == 2022 & !is.na(key) & key != "") %>% 
        mutate(key_mode = paste(key, mode, sep = " - "), how_many_artists = ifelse(artist_count == 1, "Solo", "Other")) %>% 
        group_by(key_mode, how_many_artists) %>% 
        summarise(number_of_songs = n()) %>% 
        spread(how_many_artists, number_of_songs) %>% 
        mutate(solo_and_other = sum(Other, Solo)) %>% 
        arrange(desc(solo_and_other)) %>% 
        head(10) %>% 
        arrange(desc(Solo)) %>% 
        head(1)
        
## Odp. G - Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

# Jeśli mają się też wliczać piosenki z kolaboracji

df %>% 
        separate_wider_delim(artist.s._name, delim = ",", names = c("a", "b", "c", "d", "e", "f", "g", "h"), too_few = "align_start") %>%
        pivot_longer(cols = a:h, names_to = "variable", values_to = "solo_artist_name", values_drop_na = TRUE) %>% 
        group_by(solo_artist_name) %>% 
        summarise(sum_of_streams = sum(as.numeric(streams))) %>% 
        arrange(desc(sum_of_streams)) %>% 
        head(1)

## Odp. The Weeknd

# Jeśli mają byc brane pod uwagę tylko pioseneki jednego wykonawcy

df %>%
        filter(artist_count == 1) %>% 
        group_by(artist.s._name) %>%
        summarize(sum_of_streams = sum(as.numeric(streams))) %>%
        arrange(desc(sum_of_streams)) %>%
        head(1)

## Odp. The Weeknd


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
#### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
#### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debiut_2022 <- df %>%
        filter(artist_count == 1) %>% 
        group_by(artist.s._name) %>% 
        summarise(min_year = min(released_year)) %>% 
        filter(min_year >= 2022) %>% 
        pull(artist.s._name)

df %>% 
        filter(artist.s._name %in% debiut_2022) %>% 
        group_by(artist.s._name, key, mode) %>% 
        summarise(number_of_songs = n()) %>% 
        pivot_wider(names_from = c("key", "mode"), values_from = number_of_songs, names_sep="-") %>% 
        replace(is.na(.), 0)
        
## Odp. Tabelka powyżej


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
#### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
        filter(in_spotify_playlists > in_apple_playlists) %>% 
        filter(in_spotify_charts == 0, in_apple_charts > 0) %>% 
        select(track_name)
        
## Odp. Powyższa ramka danych


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

df %>% 
        separate_wider_delim(artist.s._name, delim = ",", names = c("a", "b", "c", "d", "e", "f", "g", "h"), too_few = "align_start") %>%
        pivot_longer(cols = a:h, names_to = "variable", values_to = "solo_artist_name", values_drop_na = TRUE) %>% 
        mutate(solo_or_more = if_else(artist_count == 1, "solo", "more")) %>% 
        group_by(solo_artist_name, solo_or_more) %>% 
        summarise(sum_of_streams = sum(as.numeric(streams))/n()) %>% 
        spread(solo_or_more, sum_of_streams) %>% 
        group_by(solo_artist_name) %>% 
        filter(!is.na(solo), !is.na(more), solo > more) %>% 
        select(solo_artist_name)

## Odp. Powyższa tabelka


