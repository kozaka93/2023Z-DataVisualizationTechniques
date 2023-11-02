library(tidyr)
library(dplyr)

setwd("C:/Users/User/Desktop/IAD/TWD/HW1")
df <- read.csv('spotify-2023.csv')

library(readr)

df <- read_csv('spotify-2023.csv', locale = locale(encoding = "UTF-8"))



#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>% 
  filter(released_year==2023) %>% 
  summarise(mean_streams = mean(as.numeric(streams)))


## Odp. 147477052 odtworzeń

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

one_or_two <- df %>% 
  filter(artist_count == 1 | artist_count == 2) %>% 
  summarise(PlaylistsSum  = sum(as.numeric(in_spotify_playlists)))

more_than_two <- df %>% 
  filter(artist_count > 2) %>% 
  summarise(PlaylistsSum  = sum(as.numeric(in_spotify_playlists)))

more_than_two > one_or_two # FALSE

## Odp. Piosenki stworzone przez 1 lub 2 artystów są zawarte na większej liczbie playlist
## te stworzone przez więcej niż 2 artystów (4527593 > 428126)


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>% 
  mutate(data = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         weekday = weekdays(data)) %>% 
  group_by(weekday) %>% 
  count()

## Odp. Najczęstszym dniem tygodnia jest piątek (526 piosenek)

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

#liczę osobno ile piosenek dla danego autora było w 2021 a ile w 2022
nsongs21 <- df %>% 
  filter(released_year == 2021) %>% 
  select(track_name, artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  count() %>% 
  rename(n21 = n)

nsongs22 <- df %>% 
  filter(released_year == 2022) %>% 
  select(track_name, artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  count() %>% 
  rename(n22 = n)

# tworzę nową ramkę danych po scaleniu tych dwóch
nsongs_combined <- inner_join(nsongs21, nsongs22, by = "artist.s._name")

# obliczam procentowy wzrost
nsongs_combined %>% 
  mutate(percent_gain = ((n22-n21)/n21)*100) %>% 
  arrange(desc(percent_gain))


## Odp. Największy procentowy wzrost zanotowała SZA (1600%)

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

# obliczam kwantyl
dance_quantile <- quantile(df$danceability_., 0.9)

df_zad5 <- df %>% 
  filter(danceability_. >= dance_quantile) %>% 
  mutate(age = 2023-released_year+1) %>% 
  mutate(stream_per_year = as.numeric(streams)/age) %>% 
  arrange(desc(stream_per_year)) %>% 
  head(10)

## Odp.Spośród tanecznych piosenek najwięcej odtworzeń na rok ma piosenka autorstwa
# Chencho Corleone i Bad Bunny (720378909 odtworzeń rocznie)

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df_zad6 <- df %>% 
  mutate(stream_per_playlist = as.numeric(streams)/in_spotify_playlists) %>%
  na.omit()

spp_quantile <- quantile(df_zad6$stream_per_playlist, 0.8)

df_zad6 %>% 
  filter(stream_per_playlist >= spp_quantile) %>% 
  summarise(mean(bpm))

df_zad6 %>% 
  filter(stream_per_playlist >= spp_quantile) %>% 
  group_by(mode) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp. Średnie tempo 125.2775 a najczęściej  występująca skala to Minor (96) pośród
# tych 20% piosenek

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

install.packages("lubridate")
library(lubridate)

df_zad7 <- df %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         year = format(date, "%Y"),
         month_day = format(date, "%m-%d"),
         season = case_when(
           (month_day >= "03-21" & month_day < "06-22") ~ "Spring",
           (month_day >= "06-22" & month_day < "09-23") ~ "Summer",
           (month_day >= "09-23" & month_day < "12-22") ~ "Autumn",
           (month_day >= "12-22" | month_day < "03-21") ~ "Winter"
         )) %>% 
  select(-year, -month_day)

# Wiosna
spring <- df_zad7 %>% 
  filter(season=="Spring") %>% 
  select(danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  summarise_all(mean)

# Lato
summer <- df_zad7 %>% 
  filter(season=="Summer") %>% 
  select(danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  summarise_all(mean)

# Jesień
autumn <- df_zad7 %>% 
  filter(season=="Autumn") %>% 
  select(danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  summarise_all(mean)

# Zima
winter <- df_zad7 %>% 
  filter(season=="Winter") %>% 
  select(danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.) %>% 
  summarise_all(mean)

seasons <- rbind(spring, summer, autumn, winter) 
rownames(seasons) <- c("Spring", "Summer", "Autumn", "Winter")

## Odp.: Średnie charakterystyki dla pór roku znajdują się w ramce danych seasons
print(seasons)

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

# Tworzymy nową kolumnę: para key mode
# a następnie znajdujemy 10 najpopularniejszych

top_10_pairs <- df %>% 
  filter(released_year==2022) %>% 
  mutate(pair_key_mode = paste(key, mode, sep = " ")) %>% 
  group_by(pair_key_mode) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  select(pair_key_mode) 

df_zad8 <- df %>% 
  mutate(pair_key_mode = paste(key, mode, sep = " "))

df_zad8 <- inner_join(df_zad8, top_10_pairs, by = "pair_key_mode")

df_zad8 %>% 
  filter(artist_count==1) %>% 
  group_by(pair_key_mode) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  top_n(1, pair_key_mode)

## Odp. Najczęściej używana para dla wykonawcy solo z wybranego grona 
## to " Major" i "G Major"

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

install.packages("stringr")
library(stringr)

separate_artists <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", ")) %>% 
  unnest(artist)

df_zad9 <- separate_artists %>% 
  group_by(artist) %>% 
  select(streams, artist) %>% 
  summarise(streams_sum = sum(as.numeric(streams), na.rm=T)) %>% 
  arrange(desc(streams_sum)) %>% 
  top_n(1, streams_sum)


## Odp.Najwięcej odtworzeń piosenek w sumie ma The Weeknd (23929760757 odtworzeń)

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

# pożyczam kod z zadania 9 

separate_artists <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", ")) %>% 
  unnest(artist)


# znajdźmy artystów którzy zadebiutowali w 2022
debuting_artists <- separate_artists %>%
  group_by(artist) %>%
  select(artist, released_year) %>% 
  summarise(debut = min(released_year)) %>% 
  filter(debut==2022) %>% 
  select(artist)

df_zad10 <- inner_join(separate_artists, debuting_artists, by = "artist")

# dla key
key_summary <- df_zad10 %>%
  group_by(artist, key) %>%
  summarise(total_songs = n()) %>%
  pivot_wider(names_from = key, values_from = total_songs, names_prefix = "songs_")

key_summary[is.na(key_summary)] <- 0

# dla mode
mode_summary <- df_zad10 %>%
  group_by(artist, mode) %>%
  summarise(total_songs = n()) %>%
  pivot_wider(names_from = mode, values_from = total_songs, names_prefix = "songs_")

mode_summary[is.na(mode_summary)] <- 0

# wspólna ramka danych
mode_key_summary <- inner_join(mode_summary, key_summary, by = "artist")

df_zad10a <- mode_key_summary %>% 
  ungroup() %>% 
  select(-artist)

odp10 <- colSums(df_zad10a)

## Odp. Liczba piosenek z daną skalą i notacją znajduje się w wektorze odp10


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

# częściej spotify niż apple
spotify_popular <- df %>% 
  filter(in_spotify_playlists > in_apple_playlists)

# odnotowanie w zestawieniu apple
apple_charts <- spotify_popular %>% 
  filter(in_spotify_charts==0 & in_apple_charts>0)

## Odp.337 piosenek w ramce danych apple_charts

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

# pożyczam kod z zadania 9 i 10
separate_artists <- df %>% 
  mutate(artist=str_split(artist.s._name, pattern = ", ")) %>% 
  unnest(artist)

df_zad12 <- separate_artists %>% 
  select(artist, streams, artist_count)

# artyści którzy tworzą nie tylko solo
not_only_solo <- df_zad12 %>% 
  group_by(artist) %>% 
  summarise(max_collab = max(artist_count)) %>% 
  filter(max_collab>1)

# artyści którzy tworzą nie tylko w grupie
not_only_collab <- df_zad12 %>% 
  group_by(artist) %>% 
  summarise(min_collab = min(artist_count)) %>% 
  filter(min_collab==1)

# artyści którzy tworzą zarówno w grupie jak i solo
both_collab_and_solo <- inner_join(not_only_collab, not_only_solo, by = "artist") %>% 
  select(artist)

df_zad12a <- inner_join(df_zad12, both_collab_and_solo, by = "artist")

# średnie dla piosenek solo
means_for_solos <- df_zad12a %>% 
  filter(artist_count == 1) %>% 
  group_by(artist) %>% 
  summarise(mean_solo = mean(as.numeric(streams)))

# średnie dla piosenek w grupie
means_for_collabs <- df_zad12a %>% 
  filter(artist_count > 1) %>% 
  group_by(artist) %>% 
  summarise(mean_collab = mean(as.numeric(streams)))

# więcej solo niż w grupie
df_zad12b <- inner_join(means_for_solos, means_for_collabs, by = "artist") %>% 
  filter(mean_solo > mean_collab) %>% 
  select(artist)

## Odp.Ramka danych df_zad12b zawiera kolumnę 58 artystów, którzy średnio mają więcej
## odtworzeń solo niż w grupie




