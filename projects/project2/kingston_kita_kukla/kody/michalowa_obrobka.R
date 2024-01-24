#michalowa_obrobka <- function()
#{
  

file_path_mil <- "./CSV-ki/search_history/milosz_search_history.csv"
file_path_mic <- "./CSV-ki/search_history/michal_search_history.csv"
file_path_ant <- "./CSV-ki/search_history/antek_search_history.csv"

df_mil <- read_csv(file_path_mil)
df_mic <- read_csv(file_path_mic)
df_ant <- read_csv(file_path_ant)


df_mic <- df_mic %>% filter(Akcja == "Szukaj")

df_mic <- df_mic %>% mutate(wyszukaj = Tytuł) %>% select(wyszukaj)

df_ant <- df_ant %>% mutate(wyszukaj = Titles) %>% select(wyszukaj)


df_mil <- df_mil %>%
  separate(wyszukaj, into = c("wyszukaj", "dzien"), sep = " (?=\\S+$)", extra = "merge") %>%
  mutate(dzien = str_extract(dzien, "\\d+$"))


df_mil <- df_mil %>%
  mutate(miesiac = str_sub(wyszukaj, start = -3),
         wyszukaj = str_sub(wyszukaj, end = -4)) 


df_mil <- df_mil %>%
  mutate(miesiac = recode(miesiac, 
                          "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, 
                          "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, 
                          "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12))


df_mil <- df_mil %>% 
  select(wyszukaj, dzien, miesiac, rok, godzina)


df_words_mil <- df_mil %>%
  unnest_tokens(word, wyszukaj) %>%
  count(word, sort = TRUE)

df_words_mic <- df_mic %>% 
  unnest_tokens(word, wyszukaj) %>%
  count(word, sort = TRUE)

df_words_ant <- df_ant %>% 
  unnest_tokens(word, wyszukaj) %>%
  count(word, sort = TRUE)



df_words1_mil <- head(df_words_mil, 50)
df_words1_mic <- head(df_words_mic, 50)
df_words1_ant <- head(df_words_ant, 50)


assign("df_words1_mil", df_words1_mil, envir = .GlobalEnv)
assign("df_words1_mic", df_words1_mic, envir = .GlobalEnv)
assign("df_words1_ant", df_words1_ant, envir = .GlobalEnv)
#}