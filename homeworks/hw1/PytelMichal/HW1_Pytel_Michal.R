library(tidyr)
library(dplyr)


df <- read.csv('spotify-2023.csv',fileEncoding = "ISO-8859-1")


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>% 
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>%   
  summarize(mean_stream_count = mean(as.integer(streams)))

## Odp. mean_stream_count = 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>% 
  filter(artist_count %in% c(1,2)) %>% 
  summarize(sum = sum(in_spotify_playlists)) 
  
df %>% 
  filter(artist_count > 2) %>% 
  summarize(sum_more = sum(in_spotify_playlists)) 


## Odp. Nie

  

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = '-'))) %>% 
  mutate(weekday = weekdays.Date(date)) %>% 
  group_by(weekday) %>% 
  summarise(total_count = n()) %>% 
  arrange(-total_count)
  
  

## Odp. piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.
count_2021 <- df %>% 
  filter(artist_count == 1, released_year == 2021) %>%
  group_by(artist.s._name) %>% 
  summarise(in2021 = n()) %>% 
  arrange(artist.s._name)

count_2022 <- df %>% 
  filter(artist_count == 1, released_year == 2022) %>%
  group_by(artist.s._name) %>% 
  summarise(in2022 = n()) %>% 
  arrange(artist.s._name)

result_inner <- inner_join(count_2021, count_2022, by = "artist.s._name")
result_inner %>% 
  mutate(procentage_incr = in2022/in2021 * 100) %>% 
  arrange(-procentage_incr) %>% 
  head(1)
  
  

## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.
df %>% 
  arrange(-danceability_.) %>% 
  head(nrow(df)*0.1) %>% 
  select(track_name,artist.s._name, released_year, streams) %>% 
  mutate(meanStreamsYearly = as.integer(streams) / (2024-(as.integer(released_year)))) %>% 
  arrange(-meanStreamsYearly) %>% 
  head(1)

## Odp. Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

pop_music <- df %>% 
  mutate(streams = as.integer(streams)) %>%
  filter(!is.na(streams)) %>% 
  top_n(nrow(df)*0.2, wt = streams/in_spotify_playlists) 

pop_music %>% 
  summarise(mean(bpm))

pop_music %>% 
  group_by(mode) %>% 
  summarize(mode_count = n()) %>% 
  arrange(-mode_count) %>% 
  head(1)
  
## Odp. mean_bpm = 125.2 , most_popular_mode = Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
seasons_data<-df %>%
  mutate(seasons = case_when(
    (released_month == 12 & released_day >=22) | 
      (released_month %in% c(1,2)) |
      (released_month == 3 & released_day <=20) ~ "Winter",
    (released_month == 3 & released_day>=21) |
      (released_month %in% c(4,5)) |
      (released_month == 6 & released_day <= 21)~ "Spring",
    (released_month == 6 & released_day >= 22) |
      (released_month %in% c(7,8)) |
      (released_month == 9 & released_day <= 22)~ "Summer",
    .default = "Autumn"
  ))
seasons_data %>% group_by(seasons) %>% 
  summarise(mean_danceability= mean(danceability_.), 
            mean_valence= mean(valence_.),
            mean_energy= mean(energy_.),
            mean_acousticness= mean(acousticness_.),
            mean_instrumentalness= mean(instrumentalness_.),
            mean_liveness= mean(liveness_.),
            mean_speechiness= mean(speechiness_.))

## Odp.
View(seasons_data)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(key != "", released_year == 2022, artist_count == 1) %>% 
  group_by(key, mode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)
  

## Odp. G-Major

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>%  
  group_by(artist.s._name) %>% 
  mutate(streams = as.integer64(streams)) %>% 
  summarize(total = sum(streams, na.rm = T)) %>% 
  arrange(-total) %>% 
  head(1)
  

## Odp. The Weekend

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

past_years <- df %>% 
  filter(released_year <= 2021) %>% 
  separate_longer_delim(artist.s._name, delim = ", ")

oldies <- unique(past_years$artist.s._name)

releases_in_2022 <- df %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  filter(released_year == 2022) 
  
possible_debutants <- unique(releases_in_2022$artist.s._name)

debutants <- setdiff(possible_debutants, oldies)

debutant_data <- df %>% 
  filter(artist.s._name %in% debutants) %>% 
  mutate(key = ifelse(key=="", "Unknown", key),
         mode = ifelse(mode=="","Unknown", mode)) %>% 
  group_by(artist.s._name, mode, key) %>% 
  summarise(amount = n())

tabelka_10 <- debutant_data %>% 
  pivot_wider(names_from = c(key,mode), values_from = amount, 
              names_prefix = "Number_of ", 
              values_fill=0) %>%  
  select(starts_with("Number_of "))
## Odp.
View(tabelka_10)


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

tabelka_11 <- df %>% 
  filter(in_spotify_charts == 0, in_apple_charts > 0, in_spotify_playlists > in_apple_playlists) %>% 
  select(track_name)

## Odp.
View(tabelka_11)


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?


colab <- df %>% 
  filter(artist_count >= 2) %>% 
  mutate(streams = as.integer64(streams)) %>% 
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(colab_stream_mean = mean(streams, na.rm = T))

solo <- df %>% 
  filter(artist_count == 1) %>% 
  mutate(streams = as.integer64(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarize(single_stream_mean = mean(streams, na.rm = T))

solo_colab <- inner_join(colab, solo, by = "artist.s._name")

tabelka_12 <- solo_colab %>% 
  filter(single_stream_mean > colab_stream_mean)

## Odp. 
View(tabelka_12)
