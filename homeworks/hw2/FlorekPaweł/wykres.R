library(ggplot2)
library(dplyr)
library(tidyverse)

kraje <- c("Rumunia", "Bułgaria", "Grecja", "Hiszpania", "Łotwa", "Estionia",
               "Litwa", "Włochy", "Średnia UE", "Francja", "Niemcy", " Irlandia",
               "Portugalia", "Malta", "Chorwacja", "Luksemburg", "Belgia", "Szwecja",
               "Węgry", "Austria", "Dania", "Cypr", "Holandia", "Słowacja", "Finlandia",
               "Polska", "Słowenia", "Czechy")

w <- c(34, 32, 26, 26, 26, 25, 25, 24, 22, 21, 21, 21, 20, 20, 20, 19, 19,
              19, 18, 18, 17, 17, 17, 17, 16, 16, 13, 12)

df <- data.frame(kraje, w)
colnames(df) <- c("kraj", "wskaznik")
View(df)

df %>% 
  mutate(kraj = forcats::fct_reorder(kraj, -wskaznik)) %>% 
  ggplot(aes(x= kraj, y= wskaznik, 
             fill=factor(ifelse(kraj=="Polska","Highlighted","Normal")))) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_manual(values=c("cornflowerblue", "lightblue")) +
  theme(axis.text.x = element_text(size = 8)) + 
  geom_text(aes(label = paste(wskaznik, "%", sep = "")), size = 2.5, vjust = 1.5, hjust = 0.5, color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45, title = "")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggtitle("Wskaźnik zagrożenia ubóstwem i wykluczeniem społecznym",
          subtitle = "dane 2022")

