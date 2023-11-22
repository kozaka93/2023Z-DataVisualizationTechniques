library("dplyr")
# install.packages("ggridges")
library("ggridges")
# install.packages('reticulate')
library("reticulate")
library(ggplot2)
library(stringr)
library(tidyr)


# source_python("read_pickle.py")
pickle_data <- read_pickle_file("Data\\ingr_map.pkl")
Interactions_Raw <- read.csv('Data\\Raw_interactions.csv')
Recipes_Raw <- read.csv("Data\\RAW_recipes.csv")
Recipes_PP <- read.csv("Data\\PP_recipes.csv")
Users_PP <- read.csv("Data\\PP_users.csv")
glimpse(Recipes_Raw)




## 1. Wykres spyderchart (nie jest to wersja końcowa)

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



analiza1 <- Recipes_Raw %>%
  mutate(
    tags = gsub("\\[|\\]", "", tags)
    ) %>%
  separate_longer_delim(tags, delim = ", ") %>%
  group_by(tags) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  head(100)




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





radar_data <- radar_data %>% 
  select(mean_calories, 
         mean_protein,
         mean_sodium,
         mean_total_fat,
         mean_saturated_fat,
         mean_sugar,
         mean_carbs)

library(fmsb)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
radar_data <- rbind(rep(40,7) , rep(0,7) , radar_data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( radar_data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)






### Wykres boxplot

separated_tags <- Recipes_Raw %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")



mainDish <- separated_tags %>% 
  filter(tags == "'for-1-or-2'") %>% 
  select(id)

common_part <- Recipes_Raw %>%
  inner_join(mainDish, by = "id")

mainDish_separated_tags <- common_part %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")
library(showtext)
font_add(family="Caveat Brush", regular="C:\\users\\admin\\appdata\\local\\microsoft\\windows\\fonts\\caveatbrush-regular.ttf")
showtext_auto() 

end <- mainDish_separated_tags %>% 
  filter(tags %in% c("'french'", "'italian'", "'mexican'", "'canadian'", "'greek'", "'indian'")) %>% 
  filter(minutes < 90, minutes > 0) %>% 
  ggplot(aes(x = reorder(tags, minutes, FUN = median), y = minutes, fill = tags)) +
  geom_boxplot(alpha=1, notch = TRUE, notchwidth = 0.9,
                outlier.colour = "grey50", outlier.fill = "grey50", outlier.size = 2) +
  stat_summary(fun=mean, geom="point", shape=20, size=10, color="darkred", fill="darkred")+
  theme(legend.position = "none", text = element_text(family="Caveat Brush", color = "#4c410c", size=20),
    axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey50"),
        panel.grid = element_line(color = "#b4aea9"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#f0e6b4", color = "#f0e6b4"),
        plot.background = element_rect(fill = "#fffad4", color = "#fffad4"),
    plot.title = element_text(
      size = 35,
      face = "bold"
    ),
        ) +
  labs(title = "Distribution of time to make recipes by cuisine",
       subtitle = "Recipes for 1-or-2 people",
       x = "Cuisine",
       y = "Minutes")+
  coord_flip()
end

my_colors <- c("#f87c6c", "#5864ac", "#b8d47c", "#f8ac34", "#80c4ac", "#f8946c" )
end + scale_fill_manual(values = my_colors)

