library(tidyr)
library(dplyr)
library(stringr)

df <- read.csv('spotify-2023.csv')
# Podmiana błędnej wartości streams na 0
df[575, "streams"] <- 0
# Usunięcie błędów w nazwach artystów
df <- df %>%
  mutate(artist.s._name = gsub("<ef>|<bf>", "", artist.s._name)) 




#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

ODP_1 <- df %>%
          filter(released_month %in% c(1, 2, 3) & released_year == 2023) %>%
          summarise(mean_streams = mean(as.numeric(streams), na.rm = TRUE)) %>%
          select(mean_streams)


## Odp. 216150568





#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

ODP_2 <- df %>% 
            mutate(more_then_2_artists = artist_count > 2) %>% 
            group_by(more_then_2_artists) %>%
            summarise(mean_spotify_playlists = sum(in_spotify_playlists))


## Odp. Nie, piosenki z więcej niż 2 artystami są zawarte na 428126 playlistach na spotify,
##      a stworzone przez mniejszą ilość artystów 4527593. 





#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

ODP_3 <- df %>% 
            mutate(week_day = weekdays(as.Date(paste(released_year, released_month, released_day, sep="-")))) %>% 
            group_by(week_day) %>%
            summarise(track_count = n()) %>%
            top_n(1, track_count)

## Odp. Piątek (526 piosenek) 





#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2022 i w 2021.

ODP_4 <- df %>% 
            filter(artist_count == 1) %>%
            group_by(artist.s._name) %>% 
            summarise(
              tracks_in_2021 = sum(released_year == 2021),
              tracks_in_2022 = sum(released_year == 2022)) %>%
            filter(tracks_in_2021 != 0 & tracks_in_2022 != 0) %>% 
            mutate(tracks_increased_percent = (tracks_in_2022/tracks_in_2021 - 1 )* 100) %>% 
            top_n(1, tracks_increased_percent)

## Odp. SZA (wzrost o 1600% - 17 piosenek w 2022, 1 w 2021)





#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

percentile_90_danceability = quantile(df$danceability_., 0.9)

ODP_5 <- df %>% 
            filter(danceability_. >= percentile_90_danceability) %>% 
            mutate(years_count = 2023 - released_year + 1, streams_per_year = as.numeric(streams)/years_count) %>% 
            top_n(1, streams_per_year) %>% 
            select(track_name,  artist.s._name, streams_per_year)
 
## Odp. Jest to piosenka "Me Porto Bonito", autorstwa Chencho Corleone, Bad Bunny. Średnia roczna: 720378909





#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

percentile_80_spotify_playlist = quantile(df$in_spotify_charts, 0.8)

ODP_6_temp <- df %>% 
                filter(in_spotify_playlists >= percentile_80_spotify_playlist) %>% 
                summarise(mean_bpm = mean(bpm)) %>%
                select(mean_bpm)


ODP_6_mode <- df %>% 
                filter(in_spotify_playlists >= percentile_80_spotify_playlist) %>%
                group_by(mode) %>% 
                summarise(mode_count = n()) %>%
                top_n(1, mode_count)
  
## Odp. Średnie tempo - 122.5404; Cześciej występująca skala - Major






#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

## Dodatkowe założenie: wyznaczam porę roku wyłącznie na podstawie miesiąca 

ODP_7 <- df %>% 
            mutate(season = case_when(
                (released_month == 12 & released_day >= 22) |
                (released_month %in% c(1, 2)) | 
                (released_month == 3 & released_day < 21) 
                  ~ "winter",
                (released_month == 3 & released_day >= 21) |
                (released_month %in% c(4, 5)) | 
                (released_month == 6 & released_day < 22) 
                  ~ "spring",
                (released_month == 6 & released_day >= 22) |
                (released_month %in% c(7, 8)) | 
                (released_month == 9 & released_day < 23) 
                  ~ "summer",
                (released_month == 9 & released_day >= 23) |
                (released_month %in% c(10, 11)) | 
                (released_month == 12 & released_day < 22) 
                  ~ "autumn",
                TRUE ~ NA)) %>%
            group_by(season) %>% 
            summarise(
                      danceability_mean = mean(danceability_.), 
                      valence_mean = mean(valence_.), 
                      energy_mean = mean(energy_.),
                      acousticness_mean = mean(acousticness_.),
                      instrumentalness_mean = mean(instrumentalness_.),
                      liveness_mean = mean(liveness_.),
                      speechiness_mean = mean(speechiness_.))

## Odp.  Odpowiedź w zmiennej ODP_7





#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

ODP_8 <- df %>% 
            filter(released_year == 2022) %>% 
            group_by(key, mode) %>% 
            summarise(artist_solo_count = sum(artist_count == 1), artist_no_solo_count = sum(artist_count != 1), count = n()) %>% 
            arrange(-count) %>% 
            filter(artist_solo_count > artist_no_solo_count) %>% 
            head(1) %>% 
            select(key, mode)

## Odp. "G"   Major




#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

ODP_9 <- df %>% 
            separate_rows(artist.s._name, sep=', ') %>% 
            group_by(artist.s._name) %>%
            mutate(streams_sum = sum(as.numeric(streams))) %>% 
            arrange(-streams_sum) %>% 
            head(1) %>% 
            select(artist.s._name, streams_sum)
  
# Odp.The Weeknd - 23929760757





#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

ODP_10 <- df %>%
            separate_rows(artist.s._name, sep=", ") %>%
            group_by(artist.s._name) %>% 
            summarise(debut_year = min(released_year)) %>% 
            filter(debut_year == 2022) %>% 
            select(artist.s._name) %>% 
            left_join(
              df %>%
                separate_rows(artist.s._name, sep=", ") %>%
                group_by(artist.s._name, mode) %>%
                summarise(tracks_count = n()) %>% 
                pivot_wider(names_from = mode, values_from = tracks_count, values_fill = 0)
              , by = 'artist.s._name') %>% 
            left_join(
              df %>%
                separate_rows(artist.s._name, sep=", ") %>%
                mutate(key = ifelse(key == "", "empty", key)) %>% 
                group_by(artist.s._name, key) %>%
                summarise(tracks_count = n()) %>% 
                pivot_wider(names_from = key, values_from = tracks_count, values_fill = 0)
              , by = 'artist.s._name') 

## Odp. Odpowiedź znajduje sie w zmiennej ODP_10




#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

ODP_11 <- df %>% 
            filter(in_spotify_playlists > in_apple_playlists & in_apple_charts > 0 & in_spotify_charts == 0) %>% 
            select(track_name) 

# Odp. Odpowiedź w zmiennej ODP_11




#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

ODP_12 <- df %>% 
              separate_rows(artist.s._name, sep=", ") %>%
              group_by(artist.s._name) %>% 
              summarise(
                streams_solo_mean = mean(as.numeric(streams[artist_count ==1])),
                streams_no_solo_mean = mean(as.numeric(streams[artist_count > 1]))
                ) %>% 
              filter(!is.na(streams_solo_mean) & !is.na(streams_no_solo_mean) & streams_solo_mean > streams_no_solo_mean) %>% 
              select(artist.s._name)

# Odp:  Odpowiedź w zmiennej ODP_12

