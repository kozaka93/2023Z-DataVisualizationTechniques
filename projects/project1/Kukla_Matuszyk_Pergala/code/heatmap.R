library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(reshape2)
library(forcats)
library(svglite)

# Folder "code" powinien być obecnym katalogiem roboczym.
# Jeśli getwd() nie zwraca ".../Kukla_Matuszyk_Pergala/code",
# to należy użyć: setwd(".../Kukla_Matuszyk_Pergala/code").
# Wtedy skrypt będzie poprawnie czerpał dane względem obecnego katalogu roboczego.
# Ścieżka jest wg konwencji Windowsa. Na Liunksie wystarczy zmienić forward slashe "/"
# na backshlashe "\" w ścieżkach do ramek danych. Dla ułatwienia życia te miejsca 
# będą oznaczane poprzez komentarz: "# TUTAJ JEST ŚCIEŻKA DO RAMKI DANYCH".


# Przygotowywanie danych -----------------------------------------
# ktora_ramka <- "../data/personality_taste.xlsx"
# ktora_ramka <- "../data/personality_taste_only_sweet.xlsx"
ktora_ramka <- "../data/personality_taste_only_sweet_no_tastes.xlsx" # tutaj jest ścieżka do ramki danych
# ktora_ramka <- "../data/personality_taste_only_sweet_no_tastes_no_sweet.xlsx"
df_personality_taste <- read_xlsx(ktora_ramka)
df_personality_taste <- df_personality_taste %>%
  mutate(Extraversion = as.numeric(Extraversion),
         Openness = as.numeric(Openness),
         Neuroticism = as.numeric(Neuroticism),
         Agreeableness = as.numeric(Agreeableness),
         Conscientiousness = as.numeric(Conscientiousness),
         'Food groups preference' = fct_inorder(df_personality_taste$`Food groups preference`))
df_personality_taste <- df_personality_taste[, c("Agreeableness",
                                                 "Conscientiousness",
                                                 "Neuroticism",
                                                 "Openness",
                                                 "Extraversion")]
data <- as.matrix(df_personality_taste)
df_personality_taste <- read_xlsx(ktora_ramka)
rownames(data) <- df_personality_taste$`Food groups preference`
data <- t(data)
data <- melt(data)
colnames(data) <- c("Personality", "Taste preference", "Value")


# Tworzenie heatpamy ----------------------------------------------------
# aby podpisy nie nachodziły na siebie
data$`Taste preference` <- str_wrap(data$`Taste preference`,
                                    width = 10)

# heatmap
ggplot(data, aes(y = Personality,
                 x = `Taste preference`,
                 fill = Value)) +
  geom_tile() +
  labs(#title = "Correlation between personality traits and taste preferences", # bez tytułu w wykresie, aby dodać tytuły w Canva o jednolitym stylu czcionki dla wszystkich wykresów
       # caption = "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License\nhttps://www.ncbi.nlm.nih.gov/pmc/articles/PMC8428309/", # informacje będą podane na plakacie w źródłach
       fill = "Correlation") +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="transparent", colour=NA),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color="white",
                                 size = 12),
        legend.text = element_text(color="white"),
        # plot.caption = element_text(color="white", size = 5), # rezygnacja z caption
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        legend.background = element_rect(fill="transparent",
                                             size=0.5, linetype="solid", 
                                             colour ="transparent"),
        title = element_text(color="white", 
                             face="bold")) +
  scale_fill_gradient2(low = "#553185", high = "#ff009d", mid = "#9000ff")+
  coord_fixed(1) # wtedy heatmap ma estyczne kwadratowe pola


# Zapisywanie wykresu -----------------------------------------------------------------------------
# ggsave("C:/Users/Sebastian/Desktop/TWZ_PRO_1/heatmap_osobowosc_i_preferecnje/wykres_wektorowy_heatmap.svg",
#        scale = 1.25,
#        bg = "transparent")


# dane o autorze danych i licencji ---------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8428309/
# Copyright : © 2021 International Journal of Preventive Medicine
# Department of Nutrition, Diabetes Research Center, Health Research Institute, Ahvaz Jundishapur University of Medical Sciences, Ahvaz, Iran
# 1 Cancer Research Center, Shahid Beheshti University of Medical Sciences, Tehran
# 2 The Early Life Research Unit, Division of Child Health, Obstetrics and Gynaecology, University of Nottingham, Nottingham, UK
# 3 Department of Clinical Nutrition, School of Nutrition and Food Science, Food Security Research Center, Isfahan University of Medical Sciences, Isfahan, Iran
# 4 Department of Psychiatry, School of Medicine, Ahvaz Jundishapur University of Medical Sciences, Ahvaz, Iran
# 5 Department of Statistic and Epidemiology, School of Health, Ahvaz Jundishapur University of Medical Sciences, Ahvaz, Iran
# Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License