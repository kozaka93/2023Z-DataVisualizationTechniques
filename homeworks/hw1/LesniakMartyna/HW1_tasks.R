library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df$streams <- as.numeric(df$streams)
answer <- df %>% 
  filter(released_year == "2023" & released_month %in% c(1,2,3)) %>% 
  select(streams)  %>% 
  summarise(streams_mean = mean(streams))

#odp. 216150568
#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

average_playlists_number_more_than_2_artists <- df %>% 
  filter(artist_count > 2) %>% 
  select(in_spotify_playlists) %>% 
  summarise(average_playlists_number = mean(in_spotify_playlists))

average_playlists_number_less_than_2_artists <- df %>% 
  filter(artist_count <= 2) %>% 
  select(in_spotify_playlists) %>% 
  summarise(average_playlists_number = mean(in_spotify_playlists))

if (average_playlists_number_more_than_2_artists > average_playlists_number_less_than_2_artists){
  cat("Piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist")
} else {
  cat("Piosenki stworzone przez więcej niż 2 artystów nie są zawarte na większej liczbie playlist")
}

#Odpowiedź: average_playlists_number_more_than_2_artists = 3822.554
#           average_playlists_number_less_than_2_artists = 5383.583
#           Piosenki stworzone przez więcej niż 2 artystów nie są zawarte na większej liczbie playlist

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

answer3 <- df %>% 
  mutate(weekday = weekdays(as.Date(paste(released_year, released_month, released_day, sep="-"), format="%Y-%m-%d"))) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

## Odp.piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

answer4 <- df %>% 
  filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>% 
  group_by(artist.s._name, released_year) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = released_year, values_from = n) %>% 
  rename(released_year_2021 = '2021', released_year_2022 = '2022') %>% 
  mutate(percentage = ((released_year_2022- released_year_2021)/released_year_2021)*100) %>% 
  arrange(desc(percentage)) %>% 
  head(1)

  

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

answer5 <- df %>% 
  arrange(desc(danceability_.)) %>% 
  head(round(nrow(df) *0.1)) %>% 
  mutate(average_streams_per_year = streams/(2023 - released_year + 1)) %>% 
  arrange(desc(average_streams_per_year)) %>% 
  head(1) %>% 
  select(artist.s._name, average_streams_per_year)
  
## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_20 <- df %>% 
  mutate(streams_per_playlist = streams/in_spotify_playlists)  %>% 
  arrange(desc(streams_per_playlist)) %>% 
  head(round(nrow(df) *0.2))
  
  
average_tempo = mean(top_20$bpm)
answer6 <- top_20 %>% 
  group_by(mode) %>% 
  summarise(n = n()) %>% 
  mutate(average_tempo) %>% 
  arrange(desc(n)) %>% 
  head(1)
  
## Odp. średnie tempo: 125.2775, mode: Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
answer7 <- df %>% 
  mutate(season = case_when(
    (released_month == 12 & released_day >21) | (released_month <= 2) | (released_month == 3 & released_day<= 20) ~"Winter",
    (released_month == 3 & released_day >20) | (released_month <= 5) | (released_month == 6 & released_day<= 20) ~"Spring",
    (released_month == 6 & released_day > 20) | (released_month <= 8) | (released_month == 9 & released_day <= 22) ~ "Summer", 
    (released_month == 9 & released_day > 22) | (released_month <= 11) | (released_month == 12 & released_day <= 21) ~"Autumn",
    TRUE ~ "Other"
  )) %>% 
  group_by(season) %>% 
  summarise(average_danceability = mean(danceability_.), average_valence = mean(valence_.), average_energy = mean(energy_.),
            average_acousticness = mean(acousticness_.), average_instrumentalness = mean(instrumentalness_.), average_liveness = mean(liveness_.), average_speechiness = mean(speechiness_.))
  colnames(answer7)

## Odp.
# "season"  "average_danceability"  "average_valence"   "average_energy"  "average_acousticness"  "average_instrumentalness" "average_liveness" "average_speechiness"
# Spring  68.59574  49.31206  64.22340  27.86879  1.6631206 18.31206  10.960993
# Summer  68.53763  51.68817  65.81720  24.49462  2.1774194 18.04301  10.026882
# Winter  66.74131  57.45560  65.95367  25.71429  0.7297297 18.74517  9.555985
# Autumn  63.91150  46.96018  61.16372  29.69469  1.9646018 17.61947  9.840708


#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

answer8 <- df %>% 
  filter(nchar(key) >0 &released_year == 2022) %>% 
  mutate(key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(key_mode) %>% 
  summarise(n = n(), solist_number = sum(artist_count==1)) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  mutate(percentage = solist_number/n) %>% 
  arrange(desc(percentage)) %>% 
  head(1)


## Odp. G Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
answer9 <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(all_streams = sum(streams)) %>% 
  arrange(desc(all_streams)) %>% 
  head(1)
## Odp.The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

debut_in_2022 <- df %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(debut = min(released_year)) %>% 
  filter(debut == 2022)

answer10 <- df %>% 
  inner_join(debut_in_2022, by = "artist.s._name") %>% 
  group_by(artist.s._name, key, mode) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = c("key", "mode"), values_from = n, values_fill = 0 )

## Odp.answer10

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

answer11 <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists) %>% 
  filter(in_spotify_charts == 0 & in_apple_charts >0) %>% 
  select(track_name)


## Odp.answer11: 337 piosenek


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

 solo <- df %>% 
   filter(artist_count==1) %>% 
   group_by(artist.s._name) %>% 
   summarise(mean_streams_solo = mean(streams))
  
 with_others <- df %>% 
   separate_rows(artist.s._name, sep = ",") %>% 
   filter(artist_count > 1) %>% 
   group_by(artist.s._name) %>% 
   summarise(mean_streams_with_others = mean(streams))

 answer12 <- solo %>% 
   inner_join(with_others, by = "artist.s._name") %>% 
   filter(mean_streams_solo > mean_streams_with_others)
 ## Odp.answer12