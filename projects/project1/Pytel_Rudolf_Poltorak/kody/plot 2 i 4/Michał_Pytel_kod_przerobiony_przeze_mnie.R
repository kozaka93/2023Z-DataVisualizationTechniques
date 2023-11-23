library("dplyr")
# install.packages("ggridges")
library("ggridges")
# install.packages('reticulate')
library("reticulate")
library(ggplot2)
library(stringr)
library(tidyr)
setwd("C:\\Users\\admin\\Documents\\TWD_projekt")

source_python("read_pickle.py")
pickle_data <- read_pickle_file("Data\\ingr_map.pkl")
Interactions_Raw <- read.csv('D:\\R\\RAW_interactions.csv')
Recipes_Raw <- read.csv('D:\\R\\RAW_recipes.csv')
Recipes_PP <- read.csv('D:\\R\\PP_recipes.csv')
Users_PP <- read.csv('D:\\R\\PP_users.csv')
glimpse(Recipes_Raw)




## 1. Wykres

recipes <- Recipes_Raw %>%
  mutate(
    nutrition = gsub("\\[|\\]", "", nutrition),  # Remove brackets
    nutrition = gsub(" ", "", nutrition),  # Remove spaces
    nutrients = strsplit(nutrition, ","),  # Split into individual nutrient values
    calories = as.numeric(sapply(nutrients, function(x) as.numeric(x[1]))),
    total_fat = as.numeric(sapply(nutrients, function(x) as.numeric(x[2]))),
    sugar = as.numeric(sapply(nutrients, function(x) as.numeric(x[3]))),
    sodium = as.numeric(sapply(nutrients, function(x) as.numeric(x[4]))),
    protein = as.numeric(sapply(nutrients, function(x) as.numeric(x[5]))),
    saturated_fat = as.numeric(sapply(nutrients, function(x) as.numeric(x[6]))),
    carbohydrates = as.numeric(sapply(nutrients, function(x) as.numeric(x[7])))
  )



# analiza1 <- Recipes_Raw %>% 
#   mutate(
#     tags = gsub("\\[|\\]", "", tags)
#     ) %>% 
#   separate_longer_delim(tags, delim = ", ") %>%
#   group_by(tags) %>% 
#   summarize(n = n()) %>% 
#   arrange(-n) %>% 
#   head(100)

# ggplot(analiza1, aes(x = tags, y = n))+
#   geom_bar(stat = "identity", fill = "blue")
# recipes %>% 
#   filter(calories < 1500) %>% 
#   ggplot(aes(x = calories, y = protein)) +
#   geom_point()
# 
# recipes %>% 
#   filter(calories < 1500) %>%
#   filter(minutes < 120) %>% 
#   ggplot(aes(x = minutes, y = protein)) +
#   geom_point()
# 
# recipes %>% 
#   filter(calories < 2000) %>%
#   filter(minutes < 120) %>% 
#   ggplot(aes(x = minutes)) +
#   geom_density()
# 
# recipes %>% 
#   filter(calories < 2000) %>%
#   filter(minutes < 120) %>% 
#   ggplot(aes(x = calories)) +
#   geom_density()




# recipes_seperated_tags <- common_part %>% 
#   mutate(
#     tags = gsub("\\[|\\]", "", tags)
#   ) %>% 
#   separate_longer_delim(tags, delim = ", ")




# 
# recipes_time <- recipes_seperated_tags %>% 
#   filter(tags %in% c("'15-minutes-or-less'", "'30-minutes-or-less'",
#                      "'60-minutes-or-less'","'4-hours-or-less'"))

recipes_seperated_tags <- recipes %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")

all_recipes_tag <- recipes_seperated_tags %>% 
  filter(tags %in% c("'french'", "'italian'","'mexican'","'canadian'","'greek'","'indian'")) %>% 
  mutate(calories = calories/20) %>% 
  summarize(mean_calories = median(calories), 
            mean_protein = median(protein),
            mean_sodium = median(sodium),
            mean_total_fat = median(total_fat),
            mean_saturated_fat = median(saturated_fat),
            mean_sugar = median(sugar),
            mean_carbs = median(carbohydrates)) %>% 
  mutate(tags = "'all'")

recipes_tag <- recipes_seperated_tags %>% 
  filter(tags %in% c("'french'", "'italian'")) %>% 
  mutate(calories = calories/20) %>% 
  group_by(tags) %>% 
  summarize(mean_calories = median(calories), 
            mean_protein = median(protein),
            mean_sodium = median(sodium),
            mean_total_fat = median(total_fat),
            mean_saturated_fat = median(saturated_fat),
            mean_sugar = median(sugar),
            mean_carbs = median(carbohydrates))

radar_data <- rbind(recipes_tag, all_recipes_tag) 
radar_data


# melted_data <- radar_data %>%
#   select(tags, mean_calories, mean_protein, mean_sodium, mean_total_fat, mean_saturated_fat, mean_sugar, mean_carbs) %>%
#   gather(key = "category", value = "value", -tags)
# 
# ggplot(melted_data, aes(x = category, y = value, fill = tags)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Mean Nutritional Values by Tag",
#        x = "Nutritional Categories",
#        y = "Mean Value",
#        fill = "Tags") +
#   theme_minimal()


radar_data <- radar_data %>% 
  select(mean_calories, 
         mean_protein,
         mean_sodium,
         mean_total_fat,
         mean_saturated_fat,
         mean_sugar,
         mean_carbs)

library(fmsb)
# ggplot(radar_data, aes(x = ))
# Create data: note in High school for several students
# set.seed(99)
# 
# colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
# rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
radar_data <- rbind(rep(40,7) , rep(0,7) , radar_data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
colors_in<-c(alpha("#5874E4",0.5),alpha("#80C4F4",0.5),alpha("white",0)) 
colors_border<-c("#5874E4","#80C4F4","darkred")
plty_vals<-c(1,1,3)

# plot with default options:
radarchart( radar_data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=plty_vals,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,40,10), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# 
# recipes_time %>% 
#   filter(calories < 1500) %>% 
#   ggplot(aes(x = calories, y = tags))+
#   geom_density_ridges()+
#   labs(title = "Distribution of recepies with given calories",
#        x = "Calories",
#        y = "Time Tag") +
#   theme_minimal()
# 
# recipes_time %>% 
#   filter(calories < 1500) %>% 
#   ggplot(aes(x = calories, y = tags))+
#   geom_boxplot()+
#   labs(title = "Distribution of recepies with given calories",
#        x = "Calories",
#        y = "Time Tag") +
#   theme_minimal()
# 
# recipes_time %>% 
#   filter(protein < 150) %>% 
#   ggplot(aes(x = protein, y = tags))+
#   geom_boxplot()+
#   labs(title = "Distribution of recepies with given protein",
#        x = "Protein",
#        y = "Time Tag") +
#   theme_minimal()
# 
# recipes_time %>% 
#   filter(protein < 150) %>% 
#   ggplot(aes(x = protein, y = tags))+
#   geom_density_ridges2()+
#   labs(title = "Distribution of recepies with given protein",
#        x = "Protein",
#        y = "Time Tag") +
#   theme_minimal()




### Drugi wykres

separated_tags <- Recipes_Raw %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")



mainDish <- separated_tags %>% 
  filter(tags == "'high-protein'") %>% 
  select(id)

common_part <- Recipes_Raw %>%
  inner_join(mainDish, by = "id")

mainDish_separated_tags <- common_part %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")


end <- mainDish_separated_tags %>% 
  filter(tags %in% c("'french'", "'italian'","'mexican'","'canadian'","'greek'","'indian'")) %>% 
  filter(minutes < 120, minutes > 0) %>% 
  ggplot(aes(x = minutes, y = tags, fill = tags))+
  geom_violin()+
  theme(legend.position = "none")
end
