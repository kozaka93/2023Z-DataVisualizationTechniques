library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fmsb)



df <- read.csv("C:/Users/micha/OneDrive/Desktop/Michal studia-LAPTOP-6IKBDHP7/Techniki wizualizacji danych/Projekt/RAW_recipes.csv")



df_nutrition <- bind_cols(df$id, as.data.frame(str_replace_all(df$nutrition, "\\[|\\]|'", '')))

View(df_nutrition)

colnames(df_nutrition) <- c("id", "nutrition")


#df_skladniki <- df_skladniki %>% 
#  separate_rows(ingredients, sep = ', ')

View(df_nutrition)

View(df)


df_nutrition$cyfra1 <- as.numeric(substr(df_nutrition$nutrition, 1, 1))
df_nutrition$cyfra2 <- as.numeric(substr(df_nutrition$nutrition, 2, 2))
df_nutrition$cyfra3 <- as.numeric(substr(df_nutrition$nutrition, 3, 3))
df_nutrition$cyfra4 <- as.numeric(substr(df_nutrition$nutrition, 4, 4))
df_nutrition$cyfra5 <- as.numeric(substr(df_nutrition$nutrition, 5, 5))
df_nutrition$cyfra6 <- as.numeric(substr(df_nutrition$nutrition, 6, 6))
df_nutrition$cyfra7 <- as.numeric(substr(df_nutrition$nutrition, 7, 7))

View(df_nutrition)





# Inicjalizacja nowej ramki danych do przechowywania podzielonych wartości
dane_nowe <- data.frame(matrix(NA, nrow = nrow(df_nutrition), ncol = 7))

# Podziel każdy wiersz na 7 nowych kolumn
for (i in 1:nrow(df_nutrition)) {
  dane_split <- strsplit(df_nutrition$nutrition[i], ", ")[[1]]
  dane_nowe[i, ] <- as.numeric(dane_split)
}

# Zmień nazwy nowych kolumn, jeśli jest to konieczne
colnames(dane_nowe) <- c("Calories", "Total fat", "Sugar", "Sodium", "Protein", "Saturated fat", "Carbohydrates")


dane_nowe %>% 
  mutate(`Total fat` = `Total fat`*1800/Calories,
         Sugar = Sugar*1800/Calories,
         Sodium = Sodium*1800/Calories,
         Protein = Protein*1800/Calories,
         `Saturated fat` = `Saturated fat`*1800/Calories,
         Carbohydrates = Carbohydrates*1800/Calories,
         Calories = 1800) -> ramka



ramka %>% 
  na.omit() -> ramka

ramka %>% 
  filter(Sugar <= 100) -> zdrowe

ramka %>% 
  filter(Sugar > 100) -> niezdrowe


zdrowe %>% 
  summarise(`Mean total fat` = mean(`Total fat`),
            `Mean sodium` = mean(Sodium),
            `Mean protein` = mean(Protein),
            `Mean saturated fat` = mean(`Saturated fat`),
            `Mean carbohydrates` = mean(Carbohydrates)) -> ramka1

niezdrowe %>% 
  summarise(`Mean total fat` = mean(`Total fat`),
            `Mean sodium` = mean(Sodium),
            `Mean protein` = mean(Protein),
            `Mean saturated fat` = mean(`Saturated fat`),
            `Mean carbohydrates` = mean(Carbohydrates)) -> ramka2


ramka1 <- rbind(rep(400,6), rep(0,5), ramka1)
ramka2 <- rbind(rep(400,6), rep(0,5), ramka2)



#plot(c(0, 0.5), c(0, 0.5), type = "n", xlab = "", ylab = "", main = "Radar Chart", axes = FALSE)
#rect(0, 0, 1, 1, col = "black", border = NA)


par(new = TRUE, bg = "black", col = "white")



radarchart(ramka1, axistype = 1,
           #custom polygon
           pcol=rgb(89/300, 52/300, 235/300, 1) , pfcol=rgb(89/300, 52/300, 235/300, 0.7) , plwd=4 , 
           
           #custom the grid
           cglcol="#9c34eb", cglty=5, axislabcol="white", caxislabels=seq(0,400,100), cglwd=0.8,
           #custom labels
           vlcex=0.8
           
           
) -> wykres1


colors_border=c( rgb(0.1,0.7,0.9,0.9), rgb(0.4,0.6,0.5,0.2) , rgb(0.9,0.9,0.1,0.9) )
colors_in=c( rgb(1, 52/300, 136/300, 0.7), rgb(0.1,0.1,0.1,0.1) , rgb(0.9,0.9,0.1,0.1) )


par(new = TRUE, col = "white")


radarchart(ramka2, axistype = 1,
           #custom polygon
           pcol="#eb3489" , pfcol=colors_in ,  plwd=4 , plty=1,           
           #custom the grid
           cglcol="#9c34eb", cglty=5, axislabcol="grey", caxislabels=seq(0,400,100), cglwd=0.8,
           bg = "black",
           #custom labels
           vlcex=0.8 ,
           
) -> wykres2

legenda <- legend(x = 1.1, y = 0.2, legend = c("Nutrients for meals with sugar PDV <= 100", "Nutrients for meals with sugar PDV > 100"),
       fill = c(rgb(89/300, 52/300, 235/300, 0.7), colors_in), cex = 1, text.width = 0.8, bty = "n",
       bg = "white", border = "white", text.col = "white")




