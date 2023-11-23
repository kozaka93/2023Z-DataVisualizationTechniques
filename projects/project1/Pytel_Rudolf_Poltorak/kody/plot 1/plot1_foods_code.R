pp_recipes <- read.csv('C:\\Users\\igorr\\Documents\\archive\\PP_recipes.csv')
pp_users <- read.csv('C:\\Users\\igorr\\Documents\\archive\\PP_users.csv')
raw_interactions<- read.csv('C:\\Users\\igorr\\Documents\\archive\\RAW_interactions.csv')
raw_recipes<- read.csv('C:\\Users\\igorr\\Documents\\archive\\RAW_recipes.csv')
ingrediants_list<- read.csv('C:\\Users\\igorr\\Documents\\archive\\ingrediants_list.csv')

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reticulate)
library(lubridate)
library(gridExtra)

#let's find out what are the most popular not american speaking cuisines,
#excluding Canada

unique_tags<-c()

for(i in 1:length(raw_recipes$tags)){
  tag<- raw_recipes$tags[i]
  tag_unlisted<- unlist(strsplit(gsub("\\[|\\]", "", tag), ", "))
  
  unique_tags<- union(unique_tags,tag_unlisted )
  
}

unique_tags

unique_tab<- data.frame(
  unique= unique_tags,
  vals= numeric(length(unique_tags))
)

for(i in 1:length(raw_recipes$tags)){
  tag<- raw_recipes$tags[i]
  tag_unlisted<- unlist(strsplit(gsub("\\[|\\]", "", tag), ", "))
  
  for(j in 1:length(tag_unlisted)){
    occur<- tag_unlisted[j]
    
    row_num<-which(unique_tab$unique==occur)
    unique_tab$vals[row_num]<- unique_tab$vals[row_num]+1
  }
  
}

unique_tab<- unique_tab %>% arrange(desc(vals))

unique_tab

#check which cousines are the most popular

#'mexican', 'canadian', 'italian', 'indian', 'turkey'

#--------------------------------------------------------------------

ingrediants_list<- ingrediants_list %>% select(raw_ingr, id)

#now we need somehow to transform this data

ingrediants_list <- ingrediants_list[!duplicated(ingrediants_list$id), ]

rownames(ingrediants_list)<-NULL


italian_ingr<- data.frame(
  ingrediants= ingrediants_list$raw_ingr,
  vals = rep(0, length(ingrediants_list$raw_ingr))
)

canadian_ingr<- italian_ingr
mexican_ingr<- italian_ingr
indian_ingr<- italian_ingr
greek_ingr<- italian_ingr
french_ingr<- italian_ingr


list_of_words<- c("'canadian'","'mexican'","'italian'","'indian'","'greek'", "'french'" )
list_of_frames<- list(canadian_ingr,mexican_ingr,italian_ingr,indian_ingr,greek_ingr, french_ingr  )

for (i in 1:length(pp_recipes$id)){
  which_row_in_tags <- which(raw_recipes$id == pp_recipes$id[i])
  tag <- raw_recipes$tags[which_row_in_tags]
  tag_unlisted <- unlist(strsplit(gsub("\\[|\\]", "", tag), ", "))
  
  matching_indices <- which(list_of_words %in% tag_unlisted)
  
  if (length(matching_indices) == 1) {
    if (matching_indices[1] >= 1 && matching_indices[1] <= length(list_of_frames)) {
      ingrediants <- pp_recipes$ingredient_ids[i]
      ingrediants <- str_extract_all(ingrediants, "\\d+")
      numeric_ingrediants_unlisted <- as.numeric(ingrediants[[1]])
      indices <- which(ingrediants_list$id %in% numeric_ingrediants_unlisted)
      
      if (!length(indices) == 0) {
        list_of_frames[[matching_indices[1]]][indices, "vals"] <- list_of_frames[[matching_indices[1]]][indices, "vals"] + 1
      }
    } else {
      print("Invalid matching_indices[1] value.")
    }
  } else {
    print("No matching index found in list_of_words.")
  }
}




list_of_words<- c("'canadian'","'mexican'","'italian'","'indian'","'greek'", "'french'" )



list_of_frames[[1]] %>% arrange(desc(vals))
list_of_frames[[2]] %>% arrange(desc(vals))
list_of_frames[[3]] %>% arrange(desc(vals))
list_of_frames[[4]] %>% arrange(desc(vals))
list_of_frames[[5]] %>% arrange(desc(vals))
list_of_frames[[6]] %>% arrange(desc(vals))



#now let's create a treemap


library(plotly)

# Create the data frame
data_tree <- data.frame(
  names = c("italian", "mexican", "canadian", "indian", "greek", "french"),
  values = c(7410/2268, 6694/2268, 4572/2268, 2708/2268, 2391/2268, 2268/2268)
)

# Create the treemap without text labels
treemap_plot <- plot_ly(
  labels = data_tree$names,
  parents = "",
  values = data_tree$values,
  type = "treemap",
  marker = list(
    colors = c("#78C0A8", "#F7966B", "#F37B6B", "#F0A830", "#FCEBB6", "#5464AF"),
    line = list(width = 2)
  ),
  textinfo = "none"  # Set textinfo to "none" to remove text labels
) %>% 
  layout(
    plot_bgcolor = "#F0E6B4",
    paper_bgcolor = "#F0E6B4"
  )

# Display the treemap
treemap_plot

#now from data below let's take not trivial most popular ingrediants

#canadian

list_of_frames[[1]] %>% arrange(desc(vals))

#mexican
list_of_frames[[2]] %>% arrange(desc(vals))

#italian
list_of_frames[[3]] %>% arrange(desc(vals))

#indian
list_of_frames[[4]] %>% arrange(desc(vals))

#turkey
list_of_frames[[5]] %>% arrange(desc(vals))



list_of_frames[[1]] %>%
  anti_join(list_of_frames[[2]], by = c("ingrediants")) %>%
  anti_join(list_of_frames[[3]], by = c("ingrediants")) %>%
  anti_join(list_of_frames[[4]], by = c("ingrediants")) %>%
  anti_join(list_of_frames[[5]], by = c("ingrediants"))



#----------------------------------------------------------------------

#now let's find out the most popular dishes in each cusine
#we need to look into user's preferences

list_recipes_tag<- list(c(), c(), c(), c(), c(), c())
list_of_words<- c("'canadian'","'mexican'","'italian'","indian","'greek'","'french'" )

for(i in 1:length(raw_recipes$tags)){
  
  tag<- raw_recipes$tags[i]
  tag_unlisted<- unlist(strsplit(gsub("\\[|\\]", "", tag), ", "))
  
  matching_indices <- which(list_of_words %in% tag_unlisted)
  
  if(length(matching_indices)==1){
    list_recipes_tag[[ matching_indices  ]]<- union(list_recipes_tag[[ matching_indices  ]], raw_recipes$id[i])
  }
}

#now let's create data frame's for every one of them


list_recipes_tag

canadian_recipes<- data.frame(
  recipes<- list_recipes_tag[[1]],
  vals = rep(0, length(list_recipes_tag[[1]]))
)

mexican_recipes<- data.frame(
  recipes<- list_recipes_tag[[2]],
  vals = rep(0, length(list_recipes_tag[[2]]))
)

italian_recipes<- data.frame(
  recipes<- list_recipes_tag[[3]],
  vals = rep(0, length(list_recipes_tag[[3]]))
)

indian_recipes<- data.frame(
  recipes<- list_recipes_tag[[4]],
  vals = rep(0, length(list_recipes_tag[[4]]))
)

greek_recipes<- data.frame(
  recipes<- list_recipes_tag[[5]],
  vals = rep(0, length(list_recipes_tag[[5]]))
)

french_recipes<- data.frame(
  recipes<- list_recipes_tag[[6]],
  vals = rep(0, length(list_recipes_tag[[6]]))
)


#now let's consider interactions

length(raw_interactions$user_id)
#above 1000000 million entries

recipes_list<- list(canadian_recipes,mexican_recipes, italian_recipes,
                    indian_recipes, greek_recipes, french_recipes)


for(i in 1:length(raw_interactions$user_id)){
  recipe_interacted_with<- raw_interactions$recipe_id[i]
  
  index<-NULL
  
  for (i in 1:length(list_recipes_tag)) {
    if (recipe_interacted_with %in% list_recipes_tag[[i]]) {
      index <- i
      break
    }
  }
  
  if(!is.null(index)){
    index_row<- which(recipe_interacted_with==list_recipes_tag[[index]])
    
    recipes_list[[index]][index_row, 'vals']<- recipes_list[[index]][index_row, 'vals']+1
  }
  
}

recipes_list[[6]]

#most popular canadian dishes
most_1<-recipes_list[[1]] %>% arrange(desc(vals)) %>% slice(1:5)

#most popular mexican dishes
most_2<-recipes_list[[2]] %>% arrange(desc(vals)) %>% slice(1:5)

#most popular italian dishes
most_3<-recipes_list[[3]] %>% arrange(desc(vals)) %>% slice(1:5)

#most popular indian dishes
most_4<-recipes_list[[4]] %>% arrange(desc(vals)) %>% slice(1:5)

#most popular greek dishes
most_5<-recipes_list[[5]] %>% arrange(desc(vals)) %>% slice(1:5)

#most popular french dishes
most_6<-recipes_list[[6]] %>% arrange(desc(vals)) %>% slice(1:5)

most_5


which_1<-which(raw_recipes$id %in% most_1$recipes....list_recipes_tag..1.. )
which_2<-which(raw_recipes$id %in% most_2$recipes....list_recipes_tag..2.. )
which_3<-which(raw_recipes$id %in% most_3$recipes....list_recipes_tag..3.. )
which_4<-which(raw_recipes$id %in% most_4$recipes....list_recipes_tag..4.. )
which_5<-which(raw_recipes$id %in% most_5$recipes....list_recipes_tag..5.. )
which_6<-which(raw_recipes$id %in% most_6$recipes....list_recipes_tag..6.. )



most_popular_recipes_1<- raw_recipes$name[which_1]
most_popular_recipes_2<- raw_recipes$name[which_2]
most_popular_recipes_3<- raw_recipes$name[which_3]
most_popular_recipes_4<- raw_recipes$name[which_4]
most_popular_recipes_5<- raw_recipes$name[which_5]
most_popular_recipes_6<- raw_recipes$name[which_6]


most_popular_recipes_1
most_popular_recipes_2
most_popular_recipes_3
most_popular_recipes_4
most_popular_recipes_5
most_popular_recipes_6
