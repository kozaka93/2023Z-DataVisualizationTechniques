library(tidyr)
library(dplyr)

df <- read.csv('spotify-2023.csv')

df <- df %>%
  mutate(streams = as.numeric(streams)) %>%
  filter(!is.na(streams))



#### 1.Jaka jest średnia liczba odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale?
df %>%
  filter(released_year == 2023, released_month <= 3) %>%
  summarise(mean_streams = mean(streams))



## Odp.

#   216150568

#### 2. Czy piosenki stworzone przez więcej niż 2 artystów są zawarte na większej liczbie playlist spotify 
#### niż piosenki stworzone przez 1 lub 2 artystów?
df %>%
  group_by(artist_count > 2) %>%
  summarise(avg_playlist_spotify = mean(in_spotify_playlists))



## Odp.

##Nie, nie zawarte


#### 3. Jaki jest najpopularniejszy dzień tygodnia w wypuszczaniu piosenek?

df %>%
  mutate(day_of_week = weekdays(as.Date(paste(released_year, released_month, released_day, sep="-")))) %>%
  group_by(day_of_week) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



## Odp.

##Piątek

#### 4. Który artysta zanotował największy procentowy wzrost liczby wypuszczonych piosenek w 2022 względem roku 2021? (ogranicz się tylko do utworów w których był jeden wykonawca)
#### Ogranicz się tylko do artystów, którzy oublikowali i w 2021 i w 2021.

df %>%
  filter(released_year %in% c(2021, 2022), artist_count == 1) %>%
  group_by(`artist.s._name`, released_year) %>%
  summarise(count = n()) %>%
  arrange(`artist.s._name`, released_year) %>%
  mutate(pct_change = (count - lag(count)) / lag(count) * 100) %>%
  filter(released_year == 2022, !is.na(pct_change))%>%arrange(desc(pct_change))



## Odp.
##SZA

#### 5. Spośród piosenek znajdujących się w 10% najbardziej tanecznych, piosenka którego artysty ma średnio najwięcej odtworzeń na rok? 
### Załóżmy, że piosenki wypuszczone w 2023 były dostępne przez cały jeden rok.

top_10_dance <- quantile(df$danceability_., 0.9)
df %>%
  filter(danceability_. >= top_10_dance) %>%
         group_by(`artist.s._name`) %>%
           summarise(mean_streams = mean(streams))
         


## Odp.

## 50 Cent

#### 6. Jakim średnim tempem i najczęściej występującą skalą ('mode') charakteryzują się piosenki, 
#### które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top_20_percent_threshold <- quantile(df$streams / df$in_spotify_playlists, 0.8)


df_top_20_percent <- df %>% 
  filter((streams / in_spotify_playlists) >= top_20_percent_threshold)


df_top_20_percent %>% 
  summarise(
    avg_bpm = mean(bpm, na.rm = TRUE), 
    most_common_mode = names(sort(table(mode), decreasing = TRUE)[1])
  )


## Odp.

##BPM=125.2775 Mode=Minor

#### 7. Jakie charakterystyki taneczności, energetyki itd. mają piosenki publikowane w poszczególnych porach roku?
df_s <- df %>%
  mutate(season = case_when(
    released_month %in% c(12, 1, 2) ~ "Winter",
    released_month %in% c(3, 4, 5) ~ "Spring",
    released_month %in% c(6, 7, 8) ~ "Summer",
    released_month %in% c(9, 10, 11) ~ "Autumn",
    TRUE ~ "Unknown"
  ))

df_s %>%
  group_by(season) %>%
  summarise(mean_dance = mean(danceability_.), mean_energy = mean(energy_.))


## Odp.
###season#####danceability##energy
# Autumn       65.3        62.2
# Spring       68.0        64.3
# Summer       69.2        65.8
# Winter       65.7        64.7

#### 8.Dla 10 najpopularniejszych par key-mode w roku 2022 wybierz tę którą najcześcej tworzą artyści solowi.
df %>%
  filter(released_year == 2022, artist_count == 1) %>%
  group_by(key, mode) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


## Odp.
#1 "G"   Major    
#2 "G#"  Major  
#3 ""    Major    
#4 "C#"  Major    
#5 "E"   Minor    
#6 "D"   Major    
#7 "F"   Major    
#8 "A#"  Minor    
#9 "F"   Minor    
#10 "A#"  Major    

#### 9. Który artysta (osobno) ma najwięcej odtworzeń wszystkich jego piosenek w sumie?
df %>%
  group_by(`artist.s._name`) %>%
  summarise(total_streams = sum(streams)) %>%
  arrange(desc(total_streams))



## Odp.

#1 The Weeknd        14185552870
#2 Taylor Swift      14053658300
#3 Ed Sheeran        13908947204
#4 Harry Styles      11608645649
#5 Bad Bunny          9997799607
#6 Olivia Rodrigo     7442148916
#7 Eminem             6183805596
#8 Bruno Mars         5846920599
#9 Arctic Monkeys     5569806731
#10 Imagine Dragons    5272484650

#### 10. Dla artystów, którzy zadebiutowali na spotify w 2022 roku, 
### zrób zestawienie ogólnej liczby utworów wykonanych w poszczególnych skalach ('mode') i tonacji ('key'). 
### W wyniku jeden wiersz powinien odpowiadać jednemu artyście, informację o tych liczbach umieść w kolejnych kolumnach. 
df %>%
  filter(released_year == 2022) %>%
  group_by(`artist.s._name`, mode, key) %>%
  summarise(count = n())


## Odp.

#1 (G)I-DLE                                                                              Minor "E"       
#2 21 Savage, Gunna                                                                      Major "C#"      
#3 Abhijay Sharma, Riar Saab                                                             Minor "F"       
#4 Ak4:20, Cris Mj, Pailita                                                              Major ""        
#5 Anggi Marito                                                                          Major "F"       
#6 Anitta, Tini, Becky G                                                                 Major "C#"      
#7 Arcangel, Bad Bunny                                                                   Major "G#"      
#8 Arijit Singh, Sachin-Jigar                                                            Major "A"       
#9 Arijit Singh, Vishal Dadlani, Sukriti Kakar, Vishal-Shekhar, Shekhar Ravjiani, Kumaar Major "G"       
#10 Armani White                                                                          Major "C#"  

#### 11. Jakie piosenki mimo tego, że były bardziej popularne (pojawiały się częściej na playlistach) na spotify niż na apple,
### nie zostały odnotowane na zestawieniu spotify, ale w apple już tak?
df %>%
  filter(in_spotify_playlists > in_apple_playlists, in_spotify_charts == 0, in_apple_charts == 1) %>%
  select(track_name)


## Odp.
#1                                     SORRY NOT SORRY
#2                        I'm Not Here To Make Friends
#3                                        Hey, Mickey!
# 4                                 Escapism. - Sped Up
# 5                                        Evoque Prata
# 6              Running Up That Hill (A Deal With God)
# 7                                               Limbo
# 8              Let It Snow! Let It Snow! Let It Snow!
# 9                            Happy Xmas (War Is Over)
# 10                          Every Angel is Terrifying
# 11                              Phantom Regret by Jim
# 12       The Joker And The Queen (feat. Taylor Swift)
# 13                                            Brividi
# 14                                      Mount Everest
# 15                                               Swim
# 16                              Do We Have A Problem?
# 17 I'm Tired - From "Euphoria" An Original HBO Series
# 18                                       Freaky Deaky
# 19                                            La Zona
# 20                             Get Lucky - Radio Edit
# 21                                    We Cry Together
# 22                         Honest (feat. Don Toliver)
# 23                         10 Things I Hate About You
# 24                                          True Love
# 25                                         Grapejuice
# 26                                             Cinema
# 27             STAYING ALIVE (feat. Drake & Lil Baby)
# 28                                     BILLIE EILISH.
# 29                                               Talk
# 30                             XQ Te Pones As\xef\xbf
# 31                                         BABY OTAKU
# 32                                       Con La Brisa

#### 12. Który artysta średnio generuje więcej odtworzeń na piosenkę gdy tworzy solo niż gdy tworzy z innymi artystami?
df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8")

solo_stats <- df %>%
  filter(artist_count == 1) %>%
  group_by(`artist.s._name`) %>%
  summarise(mean_streams = mean(streams))


collab_stats <- df %>%
  filter(artist_count > 1) %>%
  separate_rows(`artist.s._name`, sep = ", ") %>%
  group_by(`artist.s._name`) %>%
  summarise(mean_streams = mean(streams))


final_table <- solo_stats %>%
  inner_join(collab_stats, by = "artist.s._name", suffix = c(".solo", ".collab")) %>%
  filter(mean_streams.solo > mean_streams.collab) 
 
final_table

## Odp.
####Są 46 takich artysci nprz.
# 1 Agust D              
# 2 Anitta                
# 3 Ariana Grande      
# 4 BoyWithUke            
# 5 Bruno Mars            
# 6 Carin Leon             
# 7 Central Cee           
# 8 Chino Pacas          
# 9 Chris Brown           
# 10 Ckay                  



