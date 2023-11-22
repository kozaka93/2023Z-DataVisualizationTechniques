library(dplyr)
library(tidyr)
library(ggplot2)

burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")
dane <- read.csv("data/Number_of_fastfood_restaurants_worldwide.csv", sep = ";")

library(reshape2)
melted_data <- melt(dane, id.vars = "Year")
print(melted_data)
str(dane)
selected_columns <- c("Domino.s.Pizza","KFC","Burger.King","PizzaHut","Subway","McDonald.s")
dane[selected_columns] <- apply(dane[selected_columns], 2, function(x) as.numeric(gsub(" ", "", x)))
dane %>% 
  na.omit() -> data_good
library(reshape2)
melted_data_good <- melt(data_good, id.vars = "Year")


melted_data_good %>% 
  mutate(logo = 0) -> melted_data_good_with_logo
melted_data_good_with_logo$logo <- as.character(melted_data_good_with_logo$logo)

melted_data_good_with_logo[c(19:27),4] <- paste("logos", "logo3.png", sep = "/")
melted_data_good_with_logo[c(10:18),4] <- paste("logos", "logo2.png", sep = "/")
melted_data_good_with_logo[c(1:9),4] <- paste("logos", "logo1.png", sep = "/")
melted_data_good_with_logo[c(28:36),4] <- paste("logos", "logo4.png", sep = "/")
melted_data_good_with_logo[c(37:45),4] <- paste("logos", "logo5.png", sep = "/")
melted_data_good_with_logo[c(46:54),4] <- paste("logos", "logo6.png", sep = "/")


library("ggimage")

melted_data_good_with_logo %>% ggplot(aes(x=Year, y=value)) +
  geom_image(aes(image=logo),size = .05) -> plot_with_logo
par(bg="#18191C")
melted_data_good_with_logo %>% ggplot(aes(x=Year, y=value, image=logo)) +
  geom_line(color="white") + geom_point() + 
  geom_image(size = .05) +
  scale_x_continuous(breaks = seq(min(melted_data_good_with_logo$Year), max(melted_data_good_with_logo$Year), by = 1)) -> plot_with_logo_v2

mcZestaw <- read.csv("data/McZestawBigMac.csv")
zdrowyObiad <- read.csv("data/zdrowyObiad.csv")

zdrowy_vs_fastfood <- mcZestaw %>% 
  filter(Danie == "Zestaw") %>% 
  select(-c(porcja,Blonnik..g.,Cukry..g.,Kwasy.tluszczowe.nasycone..g.)) %>% 
  rows_insert(y = zdrowyObiad %>% 
                select(-porcja))

rownames(zdrowy_vs_fastfood) <- c("Zestaw McDonald's", "Lekki grillowany kurczak z warzywami")

zdrowy_vs_fastfood %>% 
  select(-"Danie") -> zdrowy_vs_fastfood
colnames(zdrowy_vs_fastfood) <- c("Energy","Fat","Carbohydrates", "Protein", "Salt")

#z weglami 

zdrowy_vs_fastfood <- rbind(rep(0,5), zdrowy_vs_fastfood)
zdrowy_vs_fastfood <- rbind(c(2000,70,260,100,6), zdrowy_vs_fastfood)
colors_border=c(rgb(1,1,0.2,0.9 ),rgb(0.3984375,0,0 ,1))
colors_in=c(rgb(1,1,0, 0.4),rgb(0.3984375, 0, 0,0.7))
labelColors <- rep("white", ncol(zdrowy_vs_fastfood))
# custom_labels <- list(c(0, 375, 750, 1125, 1500), seq(0,60,15), 
#                       seq(0,200,50), seq(0,60,15), seq(0,6,1.5))
par(bg = rgb(0.09375,0.09765625, 0.109375))
par(col="white")
library(fmsb)
radarchart( zdrowy_vs_fastfood  , axistype=0 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="white", cglty=2, axislabcol="white", cglwd=2,
            #custom labels
            vlcex=1
)

#legend(x=1, y=1, legend = rownames(zdrowy_vs_fastfood[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "white", cex=1.5, pt.cex=5)
  
