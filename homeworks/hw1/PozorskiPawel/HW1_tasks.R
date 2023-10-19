library(tidyr)
library(dplyr)

options(warn = -1)

current_dir <- getwd()
relative_path <- "../spotify-2023.csv"
file_path <- file.path(current_dir, relative_path)
df <- read.csv(file_path)
df <- df %>% mutate(streams = as.numeric(streams))


#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?

df %>%
    # interesuje nas rok 2023, piorwszy kwartal
    filter(released_year == 2023 & released_month %in% c(1, 2, 3)) %>%
    # obliczamy srednia na podstawie wszystkich dostepnych w tym okresie danych
    summarise(mean_streams = mean(streams, na.rm = TRUE))

## Odp. 216150568


#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify
#### niż piosenki stworzone przez 1 lub 2 artystów?

df %>%
    # kolumna logiczna - true jesli utwor ma wiecej niz dwoch autorow
    mutate(more_than_2_artists = artist_count > 2) %>%
    # grupujemy utwory - wiecej niz 2 autorow grupa 1, pozostale grupa 2
    group_by(more_than_2_artists) %>%
    # obliczamy laczna liczbe playlist na ktorych te utwory grupowo wystepuja
    summarise(num_of_playlists = sum(in_spotify_playlists, na.rm = TRUE))

## Odp. nie


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%
    # tworzymy date wydania
    mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
    # wyciagamy z niej dzien tygodnia
    mutate(week_day = weekdays(released_date)) %>%
    # grupujemy po dniu tygodnia
    group_by(week_day) %>%
    # liczymy dla kazdej z grup liczbe wypuszczonych utworow
    summarise(num_of_songs = n()) %>%
    # sortujemy po liczbie wypuszczonych utworow malejaco
    arrange(desc(num_of_songs)) %>%
    # bierzemy pierwszy z nich
    head(1)

## Odp. Friday


#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>%
    # interesuja nas utwory z jedenym wykonawca z lat 2021, 2022
    filter(artist_count == 1 & released_year %in% c(2021, 2022)) %>%
    # grupujemy po artyscie, roku wydania
    group_by(artist.s._name, released_year) %>%
    # liczymy laczna liczbe odtworzen w kazdej z tych grup
    summarise(total_streams = sum(streams, rm.remove = TRUE)) %>%
    # zmianiamy rok -> nazwa kolumny, laczna liczba odtworzen -> wartosci kolumn
    pivot_wider(names_from = released_year, values_from = total_streams) %>%
    # pomijamy artystow z brakiem wartosci w jednej z kolumn
    filter(!is.na(`2022`) & !is.na(`2021`)) %>%
    # wyliczamy wzrost procentowy
    mutate(growth = `2022` / `2021` * 100) %>%
    # sortujemy po wzroscie malejaco
    arrange(desc(growth)) %>%
    # wybieramy tylko imie artysty
    select(artist.s._name) %>%
    # wybieramy pierwsze wejsce
    head(1)

## Odp. SZA


#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok?
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

# czesc pierwsza - zdobycie wszystkich danych
df1 <- df %>%
    # interesuje nas 10% najbardziej tanecznych
    arrange(desc(danceability_.)) %>%
    head(dim(df)[1] * 0.1) %>%
    # grupujemy po artyscie
    group_by(artist.s._name) %>%
    # obliczamy srednie liczby odtworzen dla kazdej piosenki
    mutate(avg_streams = streams / (2023 - released_year + 1)) %>%
    # sortujemy po sredniej liczbie odtworzen malejaco
    arrange(desc(avg_streams))

# czesc druga - odpowiedzi
# wybieramy tylko imie artysty - ZAL: artysta to rowniez kolaboracja paru osob
df1 %>%
    select(artist.s._name) %>%
    head(1)
# wybieramy tylko imie artysty - ZAL: jest to jedna osoba
df1 %>%
    filter(artist_count == 1) %>%
    select(artist.s._name) %>%
    head(1)

## Odp. Chencho Corleone, Bad Bunny (ZAL 1) lub Manuel Turizo (ZAL 2)


#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki,
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

# zbieramy interesujące nas piosenki
df1 <- df %>%
    # liczba odtworzen w przeliczeniu na liczbe playlist na ktorych wystepuja na spotify
    mutate(popularity_score = streams / in_spotify_playlists) %>%
    # interesuje nas 20% najpopularniejszych
    arrange(desc(popularity_score)) %>%
    head(dim(df)[1] * 0.2)

# bierzemy najbardziej popularny mode
df1 %>%
    # zliczamy ilosc wystopien kazdego mode
    count(mode, name = "mode_freq") %>%
    # sortujemy po tej ilosci malejaco
    arrange(-mode_freq) %>%
    # wybieramy tylko kolumne z jego nazwa
    select(mode) %>%
    # wybieramy pierwsze wystapienie
    head(1)

# bierzemy sredni bpm
df1 %>%
    select(bpm) %>%
    summarize(mean_bpm=mean(bpm, na.rm=TRUE))

## Odp. Minor bmp 125.2


#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?

# funkcja pomocnicza do wziecia sezonu z daty
get_season <- function(dates) {
    day_of_year <- as.numeric(format(dates, "%j"))
    seasons <- character(length(dates))

    for (i in seq_along(dates)) {
        if (day_of_year[i] >= 80 && day_of_year[i] <= 171) {
            seasons[i] <- "Spring"
        } else if (day_of_year[i] >= 172 && day_of_year[i] <= 263) {
            seasons[i] <- "Summer"
        } else if (day_of_year[i] >= 264 && day_of_year[i] <= 354) {
            seasons[i] <- "Autumn"
        } else {
            seasons[i] <- "Winter"
        }
    }
    return(seasons)
}

res <- df %>%
    # tworzymy date
    mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
    # ustalamy na jej podstawie pore roku
    mutate(seazon = get_season(released_date)) %>%
    # wybieramy interesujace nas dane
    select(seazon, ends_with("_.")) %>%
    # grupujemy po porze roku
    group_by(seazon) %>%
    # wyliczamy srednie charakterystyki interesujacych nas danych
    summarise(across(ends_with("_."), mean, na.rm = TRUE))

## Odp.
res


#### 8. Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.

# tworzymy kazdemu pare key-mode
df1 <- df %>%
    # interesuje nas rok 2022
    filter(released_year == 2022) %>%
    # bierzemy tylko te co mają i klucz i mode
    filter(key != "" & mode != "") %>%
    # sklejamy je w interesujaca nas pare
    mutate(pair = paste(key, mode, sep = "-"))

# znajdujemy najpopularniejsze 10 par
df2 <- df1 %>%
    # zliczamy ilosc wystąpien kazdej pary
    count(pair, name = "freq") %>%
    # sortujemy po tej ilosci malejaco
    arrange(-freq) %>%
    # bierzemy 10 najpopularniejszych
    head(10)

# największa ilosc osob w jednej piosence
num_art <- df %>%
    select(artist_count) %>%
    max()
# generujemy nazwy kolumn
artists <- paste("artist", 1:num_art, sep = "_")
# szukamy artystów nie solowych
group_artists <- df %>%
    # piosenki nie solowe
    filter(artist_count > 1) %>%
    # dzielimy wykonawcow do oddzielnych kolumn
    separate(artist.s._name, into = artists, sep = ", ", extra = "merge", fill = "right") %>%
    # wybieramy tylko podzielonych wykonawcow
    select(all_of(artists)) %>%
    # rzutujemy do jednej kolumny
    pivot_longer(all_of(artists), names_to = NULL, values_to = "names") %>%
    # usuwamy NA
    filter(!is.na(names)) %>%
    # bierzemy unikalne wartosci
    unique()

# szukamy najpopularniejsze wsrod artystow solowych o wybranych parach key-mode
df1 %>%
    # interesują nas artysci nie bedacy artystami grupowymi  i pary bedace w najpopularniejszych 10
    filter(!(artist.s._name %in% group_artists$names) & `pair` %in% df2$pair) %>%
    # zliczamy ilosc wystapien kazdej pary
    count(pair, name = "freq") %>%
    # sortujemy po wystapieniach malejaco
    arrange(-freq) %>%
    # wybieramy tylko pare
    select(pair) %>%
    # bierzemy najpopularniejsza
    head(1)

# Odp C#-Major


#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?

df %>%
    # licza sie tylko solowe piosenki artysty
    filter(artist_count == 1) %>%
    # grupujemy po artyscie
    group_by(artist.s._name) %>%
    # zliczamy laczna liczbe odtworzen jego piosenek
    summarise(all_streams = sum(streams)) %>%
    # sortujemy po tych odtworzeniach malejaco
    arrange(-all_streams) %>%
    # wybieramy jego imie
    select(artist.s._name) %>%
    # bierzemy najpopularniejsze - pierwsze wejscie
    head(1)

## Odp. The Weeknd


#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku,
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key').
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach.

# przygotowujemy liste artystow ktorzy zadebiutowali w 2022 i ich utworow
df1 <- df %>%
    # dzielimy artystow do roznych kolumn
    separate(artist.s._name, into = artists, sep = ", ", extra = "merge", fill = "right") %>%
    # tam gdzie piosenka ma paru artystow, doublujemy ja i przypisujemy kazdej kopii innego wspolartyste
    pivot_longer(all_of(artists), names_to = NULL, values_to = "name") %>%
    # usuwamy wejscia ktore sa NA
    filter(!is.na(name) & name != "") %>%
    # grupujemy po artyscie
    group_by(name) %>%
    # wybieramy tylko tych, co zadebiutowali w 2022
    mutate(debut_year = min(released_year)) %>%
    filter(debut_year == 2022)

# zliczamy dla wybranych wczeniej artystow liczbe utworow w zaleznosci od mode
df2 <- df1 %>%
    # grupujemy po imieniu artysty
    group_by(name) %>%
    # ignorujemy wpisy bez wartosci mode
    filter(mode != "") %>%
    # zliczamy wystapienia mode
    count(mode, name = "mode_count") %>%
    # zmieniamy aby mode bylo jako kolumny
    pivot_wider(names_from = mode, values_from = mode_count) %>%
    # NA zamieniamy na 0
    mutate_all(~ replace(., is.na(.), 0)) %>%
    # sort colnames alphabetically except for name column
    select(name, sort(setdiff(colnames(.), "name")))
# zliczamy dla wybranych wczeniej artystow liczbe utworow w zaleznosci od key
df3 <- df1 %>%
    # grupujemy po imieniu artysty
    group_by(name) %>%
    # ignorujemy wpisy bez wartosci key
    filter(key != "") %>%
    # zliczamy wystapienia mode
    count(key, name = "key_count") %>%
    # zmieniamy aby key bylo jako kolumny
    pivot_wider(names_from = key, values_from = key_count) %>%
    # NA zamieniamy na 0
    mutate_all(~ replace(., is.na(.), 0)) %>%
    # sort colnames alphabetically except for name column
    select(name, sort(setdiff(colnames(.), "name")))

# laczymy wyniki
res <- df2 %>% inner_join(df3, by = join_by(name == name))

# Odp
res


#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?

res <- df %>%
    # wybieramy piosenki, ktre byly w zestawieniu apple ale spotify juz nie
    # oraz byly czesciej w playlistach na spoify niz na apple
    filter(in_apple_charts != 0 & in_spotify_charts == 0 & in_spotify_playlists > in_apple_playlists) %>%
    # bierzemy ich nazwe
    select(track_name) %>%
    # ustawiamy alfabetycznie
    arrange(track_name)

## Odp.
res


#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?

res <- df %>%
    # dzielimy artystow do roznych kolumn
    separate(artist.s._name, into = artists, sep = ", ", extra = "merge", fill = "right") %>%
    # tam gdzie piosenka ma paru artystow, doublujemy ja i przypisujemy kazdej kopii innego wspolartyste
    pivot_longer(all_of(artists), names_to = NULL, values_to = "name") %>%
    # usuwamy wejscia ktore sa NA
    filter(!is.na(name) & name != "") %>%
    # czy to piosenka solowa
    mutate(type = ifelse(artist_count > 1, "solo", "group")) %>%
    # grupujemy po artyscie i type
    group_by(name, type) %>%
    # zliczamy odtworzenia
    mutate(total_streams = mean(streams)) %>%
    # wybieramy dalej interesujace nas dane
    select(name, type, total_streams) %>%
    # usuwamy dublikaty powstale z mutate
    distinct() %>%
    # przekladamy typ w kolumny
    pivot_wider(names_from = type, values_from = total_streams) %>%
    # NA zamieniamy na 0
    mutate_all(~ replace(., is.na(.), 0)) %>%
    # bierzemy tylko tych co wydawali i solo i w grupie, ktorzy generuja solo wiecej
    filter(solo != 0 & group != 0 & solo > group) %>%
    # wybieramy tylko imie
    select(name) %>%
    # sortujemy po imieniu
    arrange(name)

## Odp.
res
