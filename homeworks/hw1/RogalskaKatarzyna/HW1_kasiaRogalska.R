
install.packages("tidyr")
library(tidyr)
library(dplyr)

setwd("C:\\Users\\rogal\\OneDrive\\Pulpit\\RogalskaKatarzyna")
df <- read.csv('spotify-2023.csv')
colnames(df)
str(df)


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale

df %>% 
  filter(released_year==2023,released_month %in% c(1,2,3)) %>%  # wybieramy piosenki stworzone w 1 kwartale 2023
  summarize(mean=mean(as.integer(streams))) # liczymy srednia liczbe odtworzen

## Odp.
# srednia liczba odtworzeń wszystkich piosenek z 1 kwartalu 2023 to 216150568







#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>% 
  filter(artist_count>2) %>% 
  summarize(sum(in_spotify_playlists))  # tabela z iloscia playlist,w ktorej sa piosenki tworzone przez >2 artystow

df %>% 
  filter(artist_count==1 | artist_count==2) %>%  # tabela z iloscia playlist, w ktorej sa piosenki tworzone przez 1 lub 2 artystow
  summarize(sum(in_spotify_playlists))

## Odp.
# nie. Piosenek stworzonych przez >2 artystow jest na playlistach 428126, a przez 1 lub 2 :4527593






#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%  # dodajemy nowa kolumne release_weekday, najpierw laczymy rok,miesiac,dzien w jeden napis z separatorem -, potem przeksztalcamy na typ Date i uzywamy funkcji podajacej dzien tygodnia
  mutate(release_weekday =weekdays(as.Date(paste(df$released_year, df$released_month, df$released_day, sep="-")))) ->df_with_weekdays

names(which.max(table(df_with_weekdays$release_weekday))) # names po to, zeby nie wyswietlala sie pozycja piatku w tabeli

## Odp.
#piatek






#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2022.

df %>% 
  filter(released_year==2021) %>% # wybor (bez powtorzen) artystow, ktorzy wypuscili piosenke w 21
  distinct(artist.s._name)->artist_2021

df %>% 
  filter(released_year==2022) %>% 
  distinct(artist.s._name)->artist_2022 #wybor artystow, ktorzy wypuscili piosenke w 22

 intersect(artist_2021$artist.s._name, artist_2022$artist.s._name) ->artist_21_and_22 # wybor czesci wspolnej tych 2 wektorow
 
df %>% 
  filter(artist.s._name %in% artist_21_and_22) %>% # wybor z ramki danych artystow ktorzy wypuscili piosenki zarowno w 21 jak i 22
  group_by(artist.s._name) %>% 
  summarise(liczba_wydanych_21 = sum(released_year==2021), # grupowanie po artystach i sumowanie piosenek z 21 i z 22, potem obliczenie przyrostu
            liczba_wydanych_22 = sum(released_year==2022),
            growth = (liczba_wydanych_22 - liczba_wydanych_21)/liczba_wydanych_21 *100) %>% 
  arrange(-growth) %>% # ustawienie malejaco i wybor 1 artysty z najwiekszym wzrostem procentowym
  head(1) %>% 
  select(artist.s._name)
## Odp.
#SZA





#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

# przy zrozumieniu, ze 10% najbardziej tanecznych to 10% wierszy z ramki posegregowanej po tanecznosci
df %>% 
  arrange(-danceability_.) %>%
  head(as.integer(nrow(df)*0.1)) %>%  # ustawiamy malejaco po tanecznosci i wybieramy 0,1 pierwszych rzedow
  mutate(meanStreams = as.integer(streams)/ (2024 - as.integer(released_year))) %>%  # dodajemy kolumne ze srednia iloscia odsluchan w zaleznosci od tego od ilu lat istnieje dana piosenka
  arrange(-meanStreams) %>% 
  head(1) %>% # ustawiamy malejaco po srednich odsluchaniach i wybieramy 1 wiersz, czyli ten z najwieksza srednia
  select(artist.s._name) # wybieramy artyste


# przy zrozumieniu, ze wiele piosenek moze miec taka sama wartosc danceability i 10% najbardziej tanecznych to te,
# ktorych warosc danceability znajduje sie w wektorze 10% najwiekszych unikalnych wartosci danceability,
#mozna zauwazyc, ze w obu przypadkach wynik ten sam

sort(unique(df$danceability_.))-> tanecznosci
tail(tanecznosci,length(tanecznosci)*0.1)-> najwieksze_tanecznosci
df %>% 
  filter(danceability_. %in% najwieksze_tanecznosci) %>% 
  mutate(meanStreams = as.integer(streams)/ (2024 - as.integer(released_year))) %>%  # dodajemy kolumne ze srednia iloscia odsluchan w zaleznosci od tego od ilu lat istnieje dana piosenka
  arrange(-meanStreams) %>% 
  head(1) %>% # ustawiamy malejaco po srednich odsluchaniach i wybieramy 1 wiersz, czyli ten z najwieksza srednia
  select(artist.s._name)
  
## Odp.
# Artysci : Chencho Corleone, Bad Bunny







#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df %>% 
  mutate(streams = as.integer(streams)) %>%  # naprawienie ramki zeby streams byl int
  mutate(odtwrz = streams/in_spotify_playlists) %>% #obliczenie odtwarzania piosenek w przeliczeniu na liczbe playlist
  arrange(-odtwrz) %>% 
  head(nrow(df)*0.2) %>% #sortowanie i wybranie 20% najczesciej odtwarzanych
  summarise(max_mode = names(which.max(table(mode))), #table() zlicza nam wystapienia i wsadza do tabeli, wybieramy to ktore jest wieksze,ale obchodzi nas nazwa mode, wiec "names"
            mean_temp = mean(bpm)) #tabela z wartosciami, ktore nas interesuja
  
## Odp.
# srednie tempo 125,2 a najczesciej wystepujaca skala to Minor







#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df %>% 
  mutate(season = case_when(released_month==3 & released_day<21 ~ "Winter", # tworzymy nowa kolumne z porami roku w zaleznosci od daty
                            released_month==3 & released_day>=21 ~'Spring',
                            released_month %in% c(4,5) ~"Spring",
                            released_month ==6 & released_day<22 ~"Spring",
                            released_month ==6 & released_day>=22 ~"Summer",
                            released_month %in% c(7,8) ~"Summer",
                            released_month==9 & released_day<23 ~"Summer",
                            released_month==9 & released_day>=23~"Fall",
                            released_month%in% c(10,11) ~"Fall",
                            released_month==12 & released_day<22 ~"Fall",
                            released_month==12 & released_day>=22 ~"Winter",
                            released_month %in% c(1,2) ~"Winter")) ->df_with_seasons
df_with_seasons %>% 
  group_by(season) %>% # grupujemy po porach roku i podajemy ramke ze srednimi wartosciami parametrow w kazdej porze roku
  summarise(mean_danceability = mean(danceability_.),
            mean_valence = mean(valence_.),
            mean_energy = mean(energy_.),
            mean_acounticness = mean(acousticness_.),
            mean_instrumentalness = mean(instrumentalness_.),
            mean_liveness = mean(liveness_.),
            mean_speechiness = mean(speechiness_.))

## Odp.
# z tabeli mozna odczytac, ze najbardziej taneczne i energetyczne piosenki sa wydane w wiosne i lato, za to np najmniej slow maja piosenki z zimy i jesieni







#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>% 
  filter(released_year==2022) %>% 
  group_by(key,mode) %>% 
  summarize(count = n()) %>% 
  arrange(-count) %>% 
  head(10)  -> top_10_pairs # najpierw znajdujemy w ogolnosci najpopularniejsze pary 2022 roku, dostajemy ramke z kolumna key, mode i ich iloscia

df %>% 
  filter(released_year==2022, key %in% top_10_pairs$key, mode %in% top_10_pairs$mode, artist_count==1) %>%  # teraz sprawdzamy, ktore z tych par sa najczesciej wybierane przez solistow
  group_by(key,mode) %>% 
  summarise(solocount=n()) %>% 
  arrange(-solocount) -> top_solo_pairs
t
most_common_pair <- top_solo_pairs[1,c(1,2)] # wybieramy 1 wiersz, bo posegregowalismy malejaco, 1 i 2 kolumna odpowiadaja wartosciom key i mode
most_common_pair

## Odp.
# G Major






#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>% 
  separate_longer_delim(artist.s._name, delim =", ") %>%  # rozdziela napis na kilka wierszy, czyli tworzymy osobne wiersze dla artystow w grupach
  group_by(artist.s._name) %>%  #grupujemy po artystach z osobna
  summarise(sum_streams = sum(as.integer(streams))) %>% # sumujemy wszystkie odtworzenia do nowej kolumny
  arrange(-sum_streams) %>% #segregujemy malejaco
  head(1) %>% 
  select(artist.s._name)# pierwszy wiersz i nazwa artysty

## Odp.
#Bad Bunny 






#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 

df %>% 
  group_by(artist.s._name) %>% 
  filter(min(released_year)==2022) %>% 
  mutate(liczba_utworow = n())->debiutanci  #grupujemy po artystach, aby wybrac tych, ktorych pierwsza piosenka byla w 2022

debiutanci %>% 
  mutate(key = ifelse(key=="", "Unknown", key),
         mode = ifelse(mode=="","Unknown", mode))-> fixed_df #przygotowujemy ramke do uzycia pivota, czyli usuwamy puste komorki


fixed_df %>% 
  pivot_wider(names_from= c(key,mode), values_from = liczba_utworow, #tworzymy kolumny z kombinacjami kluczy i tonacji, dla kazdego artysty pokazuje sie liczba jego utworow w danej kombinacji key-mode i wybieramy tylko te kolumny, ktore nas interesuja
              names_prefix = "Songs_number ", #zaczynamy nowe kolumny od Songs_number zeby potem je latwiej wybrac
              values_fill=0) %>%  #wypelniamy NA zerami
  select(starts_with("Songs_number "))-> tabelka #wybieramy ramke z tymi kolumnami, ktore nas interesuja
  
tabelka
## Odp.
# tabela w zmiennej tabelka







#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

df %>% 
  filter(in_spotify_playlists>in_apple_playlists) %>% # wybieramy piosenki, ktore maja wiecej playlist na spotify niz apple
  filter(in_spotify_charts==0 & in_apple_charts>0) %>% #ponownie z tych wybieramy teraz piosenki, ktore nie zostaly odnotowane na spotify charts, ale na apple charts tak
  select(track_name) #wybieramy nazwy tych utworow
  
## Odp.
# ramka z nazwami piosenek, w ktorych wiecej bylo playlist na spotify, ale ich wartosc w spotify charts to zero( czyli nie zostaly odnotowane), a w apple charts dodatnia







#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df %>% 
  filter(artist_count==1) %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_streams = sum(as.integer(streams)),
            songs_count = n()) %>% 
  mutate(streams_for_song = sum_streams/songs_count)-> table_with_singles


df %>% 
  filter(artist_count>1) %>% 
  separate_longer_delim(artist.s._name, delim=", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_streams =sum(as.integer(streams)),
            songs_count= n()) %>% 
  mutate(streams_for_song = sum_streams/songs_count)->table_with_bands

table_with_singles %>% 
  inner_join(table_with_bands, by= "artist.s._name") %>% 
  filter(streams_for_song.x>streams_for_song.y) %>% 
  select(artist.s._name, streams_for_song.x, streams_for_song.y)-> table_with_better_solo_artists

table_with_better_solo_artists
## Odp.
# nazwy artystow oraz ich srednie odtworzenia samemu jak i w zespolach znajduja sie w ramce pod zmienna table_with_better_solo_artists


