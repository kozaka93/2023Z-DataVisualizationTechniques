library(tidyr)
library(dplyr)

# Otwieram plik csv w kodowaniu ISO-8859-1, gdyż w domyślnym niepoprawnie otwierane są nazwiska niektórych artystów
df <- read.csv('spotify-2023.csv', fileEncoding = 'ISO-8859-1')

# Zastępuję wartości, które nie są liczbami w kolumnie 'streams' na NA, gdyż są to ewidentnie błędne dane
df$streams <- as.numeric(df$streams)

#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

answer_1 <- df %>% 
  # wybieram piosenki opublikowane w pierwszym kwartale 2023
  filter(released_year == 2023, released_month <= 3) %>%
  # liczę średnią liczbę odtworzeń wszystkich tych piosenek
  summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp.Średnia liczba odtworzeń: 216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

more_than_2_artists <- df %>% 
  # wybieram piosenki stworzone przez więcej niż dwóch artystów
  filter(artist_count > 2) %>% 
  # zliczam łączną liczbę playlist spotify, na których zawarte są te piosenki
  summarise(number_of_playlists_1 = sum(in_spotify_playlists))

less_than_2_artists <- df %>% 
  # wybieram piosenki stworzone przez jednego lub dwóch artystów
  filter(artist_count == 1 | artist_count == 2) %>% 
  # zliczam łączną liczbę playlist spotify, na których zawarte są te piosenki
  summarise(number_of_playlists_2 = sum(in_spotify_playlists))

answer_2 <- ifelse(more_than_2_artists > less_than_2_artists, TRUE, FALSE)

## Odp. Nie

#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

answer_3 <- df %>% 
  # tworzę kolumnę 'released_date', która połączy mi informacje o dniu, roku i miesiącu wydania w jedną datę (character)
  mutate(released_date = paste(released_year, released_month, released_day, sep = "-")) %>% 
  # zamieniam uzyskaną wcześniej datę na obiekt klasy 'Date' i korzystając z funkcji 'weekdays' ustalam dzień tygodnia
  mutate(day_of_week = weekdays(as.Date(released_date))) %>%
  # grupuję po dniach tygodnia
  group_by(day_of_week) %>% 
  # sumuję liczbę obserwacji w każdej grupie
  summarise(cnt = n()) %>% 
  # sortuje malejąco po liczbie obserwacji
  arrange(-cnt) %>% 
  head(1) %>% 
  select(day_of_week)

## Odp. Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? 
### (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy opublikowali i w 2021 i w 2022.

artists <-  df %>% 
  # grupuję po artystach
  group_by(artist.s._name) %>% 
  # wybieram artystów, którzy opublikowali i w 2021 i w 2022
  filter(all(c(2021, 2022) %in% released_year)) %>% 
  # wybieram wszystkie unikalne nazwiska artystów
  distinct(artist.s._name)

# wybieram z ramki df tych artystów, którzy spełniają warunek zdefiniowany powyżej
answer_4 <- inner_join(artists, df, by = 'artist.s._name') %>% 
  # wybieram utwory solistów i piosenki z lat 2021 i 2022
  filter(artist_count == 1 & (released_year == 2021 | released_year == 2022)) %>% 
  # grupuję po nazwiskach artystów i latach opublikowania piosenk
  group_by(artist.s._name, released_year) %>% 
  # sumuję liczbę obserwacji w każdej grupie
  summarise(number_of_songs =n()) %>% 
  # tworzę pivot_wider'a, w którym kolumnami są lata 2021 i 2022, wierszami artyści, a wartościami liczba piosenek wydanych przez danego artystę w danym roku
  pivot_wider(names_from = released_year, values_from = number_of_songs, names_prefix = "number_of_songs_in_") %>% 
  # liczę procentowy wzrost liczby piosenek wypuszczonych w 2022 względem 2021
  mutate(percentage_increase = ((number_of_songs_in_2022 - number_of_songs_in_2021) / number_of_songs_in_2021) * 100) %>% 
  # sortuję malejąco po procentowym wzroście
  arrange(-percentage_increase) %>% 
  head(1) %>% 
  select(artist.s._name)
  
## Odp. SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

answer_5 <- df %>% 
  # sortuję malejąco po taneczności
  arrange(-danceability_.) %>% 
  # wybieram 10% piosenek o największej taneczności
  head(floor(nrow(df) * 0.1)) %>% 
  # liczę, ile lat na rynku jest dana piosenka
  mutate(years_on_market = 2023 + 1 - released_year) %>%
  # wyliczam, ile średnio odtworzeń na rok generowała piosenka
  mutate(mean = streams / years_on_market) %>%
  # sortuję malejąco po średniej
  arrange(-mean) %>%
  head(1) %>%
  select(artist.s._name)

## Odp. Chencho Corleone, Bad Bunny

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

most_streams_per_spotify_playlist <- df %>% 
  # wyliczam w nowej kolumnie liczbę odtworzeń w przeliczeniu na jedną playlistę spotify
  mutate(streams_per_playlist = streams / in_spotify_playlists) %>% 
  # sortuję malejąco po tej kolumnie
  arrange(-streams_per_playlist) %>%
  # wybieram 20% najczęściej odtwarzanych piosenek w przeliczeniu na playlistę spotify
  head(floor(nrow(df) * 0.2)) 
  
mean_tempo <- most_streams_per_spotify_playlist %>% 
  # wyliczam średnie tempo piosenek spełniających warunek z polecenia
  summarise(mean_tempo = mean(bpm))

most_frequent_mode <- most_streams_per_spotify_playlist %>% 
  # grupuję po skali
  group_by(mode) %>%
  # zliczam dla każdej skali, ile piosenek się nią charakteryzuje
  summarise(frequency_of_mode = n()) %>% 
  # sortuję malejąco po częstości występowania danej skali
  arrange(-frequency_of_mode) %>% 
  head(1) %>% 
  select(mode)

answer_6 <- c(unlist(mean_tempo), unlist(most_frequent_mode))

## Odp. Średnie tempo = 125.2 ,  Najczęstsza moda = Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

answer_7 <- df %>% 
  # tworzę kolumnę, w której dla podanej daty wyliczona zostanie odpowiadająca jej pora roku
  mutate(season = case_when(
    (released_month == 3 & released_day >= 21) | (released_month > 3 & released_month < 6) | (released_month == 6 & released_day < 22) ~ "Spring",
    (released_month == 6 & released_day >= 22) | (released_month > 6 & released_month < 9) | (released_month == 9 & released_day < 23) ~ "Summer",
    (released_month == 9 & released_day >= 23) | (released_month > 9 & released_month < 12) | (released_month == 12 & released_day < 22) ~ "Autumn",
    TRUE ~ "Winter"
  )) %>% 
  # grupuję piosenki po porach roku
  group_by(season) %>% 
  # dla każdej pory roku wyliczam średnią wartość w kolumnach kończących się na '_.'
  summarise(across(ends_with("_."), mean, .names = "mean_{.col}"))
  

 ## Odp. Tabela zawarta w zmiennej answer_7
answer_7

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześciej tworzą artyści solowi.

most_popular_key_mode_combination <- df %>% 
  # wybieram piosenki wydane w 2022
  filter(released_year == 2022) %>%
  # grupuję po kluczu i skali
  group_by(key, mode) %>% 
  # dla każdej kombinacji zliczam liczbę obserwacji
  summarise(songs_per_combination_in_2022 = n()) %>%
  # sortuję malejąco po liczbie piosenek dla danej kombinacji
  arrange(-songs_per_combination_in_2022) %>% 
  head(10)

answer_8 <- df %>% 
  # z domyślnej ramki danych wybieram tylko te piosenki, których kombinacja kluczu i skali zalicza się do 10 najpopularniejszych w 2022
  inner_join(most_popular_key_mode_combination, by = c("key", "mode")) %>% 
  # wybieram piosenki stworzone przez solistów
  filter(artist_count == 1) %>% 
  # grupuję po kluczu i skali
  group_by(key, mode) %>% 
  # dla każdej grupy zliczam liczbę obserwacji
  summarise(songs_per_combination_by_solists = n()) %>% 
  # sortuję malejąco po liczbie piosenek dla każdej kombinacji
  arrange(-songs_per_combination_by_solists) %>% 
  head(2) %>% 
  select(key, mode)
  
## Odp. Są to dwie pary. Pierwsza : klucz: G, moda: Major. Druga: klucz: "", moda: Major.

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

answer_9 <- df %>% 
  # rozdzielam piosenki stworzone przez kilku artystów na kilka obserwacji, po jednej dla każdego artysty
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  # grupuję po artystach
  group_by(artist.s._name) %>%
  # dla każdej grupy sumuję liczbę odtworzeń
  summarize(number_of_streams = sum(streams)) %>%
  # sortuję malejąco po liczbie odtworzeń
  arrange(-number_of_streams) %>% 
  head(1) %>% 
  select(artist.s._name)

## Odp. The Weeknd

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

separated_artists <- df %>% 
  # rozdzielam piosenki stworzone przez kilku artystów na kilka obserwacji, po jednej dla każdego artysty
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  # wyrzucam obserwacje, w których artystą jest "", gdyż są to błędne dane
  filter(artist.s._name != "")

debuted_in_2022 <- separated_artists %>% 
  # grupuję po artyście i roku utworzenia
  group_by(artist.s._name, released_year) %>% 
  # dla każdej grupy zliczam liczbę piosenek
  summarise(number_of_songs = n()) %>% 
  # robię pivot_wider'a, w którym w kolumnach mam różne lata, w których piosenki powstawały, w wierszach nazwiska artystów, zaś wartości to liczby piosenek
  # stworzonych przez danego artystę w danym roku
  pivot_wider(names_from = released_year, values_from = number_of_songs, names_prefix = "number_of_songs_in_") %>% 
  # usuwam kolumnę dotyczącą piosenek wyprodukowanych w 2023
  select(-number_of_songs_in_2023) %>% 
  # wybieram te wiersze, które we wszystkich kolumnach dotyczących lat wcześniejszych niż 2022 mają wartość NA (co można rozumieć, że w tym roku nie powstały żadne piosenki danego artysty)
  filter(if_all(2:last_col(), ~is.na(.))) %>% 
  # wybieram te wiersze, które nie mają wartości NA w kolumnie dotyczącej roku 2022
  filter(!is.na(number_of_songs_in_2022)) %>% 
  # wybieram nazwiska artystów, którzy zadebiutowlai na spotify w 2022
  select(artist.s._name)

answer_10 <- separated_artists %>% 
  # z ramki zawierającej piosenki wybieram tylko te stworzone przez piosenkarzy debiutujących w 2022
  inner_join(debuted_in_2022, by = 'artist.s._name') %>% 
  # grupuję po artyście, kluczu i skali
  group_by(artist.s._name, key, mode) %>% 
  # dla każdej grupy zliczam liczbę piosenek
  summarise(number_of_songs = n()) %>% 
  # tworzę pivot_wider'a, w którym kolumny reprezentują poszczególne kombinacje skali i klucza, wiersze to nazwiska artystów, zaś wartości to liczby piosenek danego artysty,
  # które charakteryzują się dana kombinacją
  pivot_wider(names_from = c(key, mode), values_from = number_of_songs) %>% 
  # zastępuję wartości NA zerami
  mutate_all(~replace(., is.na(.), 0))
  
## Odp. Zestawienie zapisane w zmiennej answer_10
answer_10

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

answer_11 <- df %>% 
  # wybieram te piosenki, który pojawiły się na większej liczbie playlist spotify niż apple, nie zostały odnotowane w żadnym zestawieniu spotify, ale w apple już tak
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts > 0) %>% 
  select(track_name)
  
## Odp. Jest 337 takich piosenek. Ich nazwy są zapisane w zmiennej answer_11 .
answer_11

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

solo <- df %>% 
  # wybieram piosenki stworzone przez solistów
  filter(artist_count == 1) %>% 
  # grupuję po artystach
  group_by(artist.s._name) %>% 
  # dla każdej grupy wyliczam średnią liczbę odtworzeń
  summarise(mean_streams_when_solo = mean(streams, na.rm = TRUE))

not_solo <- df %>% 
  # wybieram piosenki stworzone przez więcej niż jednego artystę
  filter(artist_count != 1) %>% 
  # rozdzielam piosenki stworzone przez kilku artystów na kilka obserwacji, po jednej dla każdego artysty
  separate_longer_delim(artist.s._name, delim = ", ") %>% 
  # grupuję po artystach
  group_by(artist.s._name) %>%
  # dla każdej grupy wyliczam średnią liczbę odtworzeń
  summarise(mean_streams_when_not_solo = mean(streams, na.rm = TRUE))

# tworzę ramkę z artystami, którzy tworzyli piosenki zarówno solo, jak i nie
answer_12 <- inner_join(solo, not_solo, by = 'artist.s._name') %>% 
  # wybieram tych artystów, którzy mieli średnio więcej odtworzeń na piosenkę, gdy tworzyli solo niż w przeciwnym wypadku
  filter(mean_streams_when_solo > mean_streams_when_not_solo) %>% 
  select(artist.s._name)
  
## Odp. Jest 58 takich artystów. Ich nazwiska są zapisane w zmiennej answer_12
answer_12