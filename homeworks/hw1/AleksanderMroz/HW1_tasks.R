library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

mean_streams <- df %>% 
  select(track_name, released_year, released_month, streams) %>% 
  filter(released_year == 2023 & released_month <= 3)  %>% 
  mutate(streams = as.numeric(streams)) %>% 
  summarise(mean_streams = mean(streams))
mean_streams

## Odp. 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
ans <- df %>%
      select(track_name, artist_count, in_spotify_playlists)

case1 <- ans %>%
  filter(artist_count <= 2) %>% 
  summarise("2_or_less" = sum(in_spotify_playlists))

case2 <- ans %>%
  filter(artist_count > 2) %>% 
  summarise(more_than_two = sum(in_spotify_playlists))

ans <- cbind(case1, case2)
column_names <- colnames(ans)
max_col_index <- which.max(1)
ans <- column_names[max_col_index]
ans

## Odp. Na więszkej liczbie playlist są piosenki tworzone przez 1 albo 2 artystów



#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?
ans <- df %>%
  mutate(Date = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         day_of_the_week = weekdays(Date)) %>% 
  select(day_of_the_week) %>% 
  count(day_of_the_week) %>% 
  arrange(desc(n)) %>% 
  head(1)

ans
## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

ans <- df %>% 
  filter(artist_count == 1 & (released_year == 2022 | released_year == 2021)) %>% 
  select(artist.s._name, released_year) %>%
  group_by(released_year, artist.s._name) %>%
  summarize(count = n()) %>% 
  pivot_wider(names_from = released_year, values_from = count) %>% 
  na.omit() %>% 
  mutate(percent = .[["2022"]]/.[["2021"]]-1) %>% 
  arrange(desc(percent)) %>% 
  head(1)

  
ans
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

ans <- df %>% 
  select(artist.s._name, danceability_., streams, released_year) %>% 
  arrange(desc(danceability_.)) %>% 
  head(0.1 * length(df$artist.s._name)) %>% 
  mutate(average_streams_per_year = as.numeric(streams)/(2024 - released_year)) %>% 
  arrange(desc(average_streams_per_year)) %>% 
  head(1)

ans
## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
ans <- df %>% 
  filter(!grepl("[a-zA-Z]", streams)) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  mutate(ratio = streams/ in_spotify_playlists) %>% 
  select(artist.s._name, bpm, mode, ratio) %>% 
  arrange(desc(ratio)) %>% 
  head(0.2 * length(df$track_name)) %>% 
  summarise(most_common_mode = names(which.max(table(mode))), average_temp = mean(bpm))

ans
## Odp. Minor, 125.2 bpm

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

season <- function(day, month) {
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  } else {
    return("Winter")
  }
}


ans <- df %>% 
  rowwise() %>%
  mutate(season = season(released_day, released_month)) %>%
  ungroup() %>% 
  select(track_name, season, danceability_., energy_., valence_.) %>%
  group_by(season) %>%
  summarize(
    avg_danceability = mean(danceability_.), avg_energy = mean(energy_.), avg_valence = mean(valence_.)
  )
  

ans
# Odp.
View(ans)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
pairs <- df %>% 
  filter(released_year == 2022) %>% 
  group_by(key, mode) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

ans <- df %>% 
  filter(released_year == 2022, key %in% pairs$key, mode %in% pairs$mode, artist_count == 1) %>% 
  group_by(key, mode) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(1)
  
  

ans
## Odp. G, Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

ans <- df %>% 
  separate_longer_delim(artist.s._name, ", ") %>% 
  filter(!grepl("[a-zA-Z]", streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(summarised_streams = sum(as.numeric(streams))) %>% 
  arrange(desc(summarised_streams)) %>% 
  head(1)
ans

## Odp. The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

ans <- df %>%
  group_by(artist.s._name) %>% 
  filter(min(released_year) == 2022) %>% 
  mutate(songs_count = n()) %>% 
  filter(key != "", mode != "") %>% 
  pivot_wider(names_from= c(key,mode), values_from = songs_count, names_prefix = "K-M ", values_fill = 0) %>% 
  select(artist.s._name, starts_with("K-M "))

## Odp.
View(ans)

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
ans <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts != 0) %>% 
  select(track_name)

## Odp.
View(ans)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
ans <- df %>% 
  filter(!grepl("[a-zA-Z]", streams)) %>% 
  separate_longer_delim(artist.s._name, ", ") %>% 
  group_by(artist.s._name, group = ifelse(artist_count == 1, "one_artist", "2_or_more")) %>% 
  summarise(mean_streams = mean(as.numeric(streams))) %>% 
  group_by(artist.s._name, group) %>% 
  pivot_wider(names_from = group, values_from = mean_streams) %>%
  na.omit() %>% 
  filter(one_artist > "2_or_more")
## Odp.
View(ans)


