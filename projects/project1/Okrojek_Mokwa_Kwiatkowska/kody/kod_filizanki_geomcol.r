library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud2)
library(htmlwidgets)
library(extrafont)

setwd("C:\\Users\\HP\\R\\TWD_semestr3\\projekt1")
kawa <- read.csv("reviews_feb_2023.csv")

kawa <- kawa %>% 
  select(title, rating, coffee_origin, blind_assessment)
kawa$rating <- as.numeric(kawa$rating)
kawa$coffee_origin <- gsub("\\.", "", kawa$coffee_origin)

kawa <- kawa %>% 
  filter(coffee_origin != "Not disclosed")

wybierz_ostatni_wyraz <- function(tekst) {
  podzielone <- unlist(strsplit(tekst, ", "))
  ostatni_wyraz <- tail(podzielone, n = 1)
  return(ostatni_wyraz)
}

kawa$coffee_origin <- sapply(kawa$coffee_origin, wybierz_ostatni_wyraz)

wybierz_ostatni_wyraz1 <- function(tekst) {
  wyrazy <- unlist(strsplit(tekst, " "))
  ostatni_wyraz <- tail(wyrazy, n = 1)
  return(ostatni_wyraz)
}

kawa$coffee_origin <- sapply(kawa$coffee_origin, wybierz_ostatni_wyraz1)

kawa_kraj <- kawa
kawa_przym <- kawa


########## przymiotniki opisujące kawy

kawa_przym$blind_assessment <- gsub("\\.", ",", kawa_przym$blind_assessment)
kawa_przym$blind_assessment <- gsub(":", "", kawa_przym$blind_assessment)
kawa_przym$blind_assessment <- gsub(";", "", kawa_przym$blind_assessment)
kawa_przym <- kawa_przym %>% 
  mutate(blind_assessment = tolower(blind_assessment))
kawa_przym$blind_assessment <- strsplit(kawa_przym$blind_assessment, ",")
max_length <- max(sapply(kawa_przym$blind_assessment, length))

for (i in 1:max_length) {
  kawa_przym <- kawa_przym %>%
    mutate(!!paste0("kolumna_", i) := sapply(kawa_przym$blind_assessment, function(x) ifelse(length(x) >= i, x[i], NA))
    )
}

kawa_przym <- kawa_przym %>% 
  pivot_longer(
    cols = starts_with("kolumna"),
    names_to = "kolumna",
    values_to = "przymiotnik"
  ) %>% 
  select(title, rating, coffee_origin, przymiotnik) %>% 
  na.omit(kawa_przym)

kawa_przym$przymiotnik <- trimws(kawa_przym$przymiotnik)

dziwne_slowa_do_usuniecia <- c(" ", "\n", "cedar", "round", "bright", "sandalwood", "date")

kawa_przym <-kawa_przym %>% 
  select(title, coffee_origin, przymiotnik) %>% 
  group_by(coffee_origin, przymiotnik) %>% 
  summarise( n = n()) %>% 
  arrange(coffee_origin) %>%
  filter(!przymiotnik %in% dziwne_slowa_do_usuniecia)


#########################   tworzenie wordcloudów

paleta <- c("#6B1D0C", "#000000", "#843E12", "#5D2603", "#3F0008", "#BB7A48", "#0B0500")


etiopia_wordcloud <- kawa_przym %>% 
  filter(coffee_origin == "Ethiopia") %>% 
  arrange(-n)
etiopia_wordcloud <- etiopia_wordcloud %>% 
  arrange(-n) %>% 
  head(round(0.1 * nrow(etiopia_wordcloud)))
nowa_ramka_e <- etiopia_wordcloud[c("przymiotnik", "n")]
etiopia <- wordcloud2(data = nowa_ramka_e, figPath = "kawka1.jpg", size = 2, color=rep_len( paleta, nrow(nowa_ramka_e)), rotateRatio = 0.3)

colombia_wordcloud <- kawa_przym %>% 
  filter(coffee_origin == "Colombia") %>% 
  arrange(-n)
colombia_wordcloud <- colombia_wordcloud %>% 
  head(round(0.1 * nrow(colombia_wordcloud)))
nowa_ramka_c <- colombia_wordcloud[c("przymiotnik", "n")]
colombia <- wordcloud2(data = nowa_ramka_c, figPath = "kawka1.jpg", size = 2, color=rep_len( paleta, nrow(nowa_ramka_c)), rotateRatio = 0.3)

kenya_wordcloud <- kawa_przym %>% 
  filter(coffee_origin == "Kenya") %>% 
  arrange(-n)
kenya_wordcloud <- kenya_wordcloud %>% 
  head(round(0.15 * nrow(kenya_wordcloud)))
nowa_ramka_k <- kenya_wordcloud[c("przymiotnik", "n")]
kenya <- wordcloud2(data = nowa_ramka_k, figPath = "kawka1.jpg", size = 2, color=rep_len( paleta, nrow(nowa_ramka_k)), rotateRatio = 0.3)

panama_wordcloud <- kawa_przym %>% 
  filter(coffee_origin == "Panama") %>% 
  arrange(-n)
panama_wordcloud <- panama_wordcloud %>% 
  head(round(0.15 * nrow(panama_wordcloud)))
nowa_ramka_p <- panama_wordcloud[c("przymiotnik", "n")]
panama <- wordcloud2(data = nowa_ramka_p, figPath = "kawka1.jpg", size = 2, color=rep_len( paleta, nrow(nowa_ramka_p)), rotateRatio = 0.3)

guatemala_wordcloud <- kawa_przym %>% 
  filter(coffee_origin == "Guatemala") %>% 
  arrange(-n)
guatemala_wordcloud <- guatemala_wordcloud %>% 
  head(round(0.15 * nrow(guatemala_wordcloud)))
nowa_ramka_g <- guatemala_wordcloud[c("przymiotnik", "n")]
guatemala <-wordcloud2(data = nowa_ramka_g, figPath = "kawka1.jpg", size = 2, color=rep_len( paleta, nrow(nowa_ramka_g)), rotateRatio = 0.3)

saveWidget(etiopia,"etiopia_wordcloud.html",selfcontained = F)
saveWidget(colombia,"colombia_wordcloud.html",selfcontained = F)
saveWidget(kenya,"kenya_wordcloud.html",selfcontained = F)
saveWidget(panama,"panama_wordcloud.html",selfcontained = F)
saveWidget(guatemala,"guatemala_wordcloud.html",selfcontained = F)


####################################    najlepsza kawa

kawa_kraj$coffee_origin <- gsub("Hawai’i", 'Hawaii', kawa_kraj$coffee_origin)
kawa_kraj$coffee_origin <- gsub("Rica", 'Costa Rica', kawa_kraj$coffee_origin)


kawa <- kawa_kraj %>% 
  group_by(title, coffee_origin) %>% 
  summarise(mean_rating = round(mean(rating))) %>% 
  filter(mean_rating > 90) %>% 
  group_by(coffee_origin) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(8)

ggplot(kawa, aes(x = reorder(coffee_origin, -n), y = n)) +
  geom_col(fill = "#682E19", position = position_dodge(width = 1)) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "black", linetype = 2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "transparent"),
    text = element_text(size = 16, family = "Bell MT", color = "black"),
    plot.title = element_text(family = "Bell MT", size = 22, hjust = 0.5, colour = "black"),
    axis.title = element_text(family = "Bell MT", size = 16, color = "black"),
    axis.ticks.x = element_blank(), 
    axis.text = element_text(family = "Bell MT", size = 11, color = "black")
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.15), c(10, 10))) +
  labs(fill = "coffee origin", title = "Where does the best coffee come from ?", x = "Coffee Origin", y = "Quantity") 
#ggsave("best11.png", device = "png", bg = "transparent", width = 9, height = 7)

