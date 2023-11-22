#read csv
setwd("/Users/fantasy2fry/Documents/informatyczne/iadstudia/twd/fast_food_data_analysis_project/spider_plot")
df_fast_food <- read.csv("../data/fastfood.csv")
df_restaurants <- read.csv("../data/Fast_Food_Restaurants_US.csv")

library(fmsb)
library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
#library(ggradar)

logos_links=c('https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Arby%27s_logo.svg/640px-Arby%27s_logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Burger_King_logo_%281999%29.svg/2024px-Burger_King_logo_%281999%29.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/Chick-fil-A_Logo.svg/2560px-Chick-fil-A_Logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Dairy_Queen_logo.svg/1200px-Dairy_Queen_logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/McDonald%27s_Golden_Arches.svg/1200px-McDonald%27s_Golden_Arches.svg.png',
              'https://download.logo.wine/logo/Sonic_Drive-In/Sonic_Drive-In-Logo.wine.png',
              'https://1000logos.net/wp-content/uploads/2017/06/Subway-Logo-2002.png',
              'https://static.wikia.nocookie.net/logopedia/images/4/45/Taco_Bell.svg/revision/latest?cb=20200112222740')


df_restaurants_prepaired_for_spider_plot=df_fast_food %>% 
  group_by(restaurant) %>%
  summarise(average_calories=mean(calories),
            average_total_fat=mean(total_fat),
            average_cholesterol=mean(cholesterol),
            average_carbohydrates=mean(total_carb),
            average_sugar=mean(sugar),
            average_protein=mean(protein, na.rm = TRUE))

df_restaurants_prepaired_for_spider_plot=
  df_restaurants_prepaired_for_spider_plot %>% 
  mutate(average_calories=100*average_calories/max(average_calories),
         average_total_fat=100*average_total_fat/max(average_total_fat),
         average_cholesterol=100*average_cholesterol/max(average_cholesterol),
         average_carbohydrates=100*average_carbohydrates/max(average_carbohydrates),
         average_sugar=100*average_sugar/max(average_sugar),
         average_protein=100*average_protein/max(average_protein))

df_max_min_for_spider_plot=
  df_restaurants_prepaired_for_spider_plot %>%
  summarise(max_calories=max(average_calories),
            max_total_fat=max(average_total_fat),
            max_cholesterol=max(average_cholesterol),
            max_carbohydrates=max(average_carbohydrates),
            max_sugar=max(average_sugar),
            max_protein=max(average_protein),
            min_calories=min(average_calories),
            min_total_fat=min(average_total_fat),
            min_cholesterol=min(average_cholesterol),
            min_carbohydrates=min(average_carbohydrates),
            min_sugar=min(average_sugar),
            min_protein=min(average_protein))
df_max_min_for_spider_plot=as.data.frame(t(df_max_min_for_spider_plot))
df_max_min_for_spider_plot=rbind(df_max_min_for_spider_plot[1:6,],
                                 df_max_min_for_spider_plot[7:12,])
# true radar plot
# for every restaurant

# tutaj jest walka
  
# remove total_ from colnames


# tutaj konczy sie walka
for (i in 1:4) {
  df=df_restaurants_prepaired_for_spider_plot[(2*i-1):(2*i),2:7]
  name=df_restaurants_prepaired_for_spider_plot[(2*i-1):(2*i),1]
  #remove average from every colnames
  colnames(df)=gsub("average_","",colnames(df))
  colnames(df_max_min_for_spider_plot)=colnames(df)
  rownames(df)=name[[1]]
  df=rbind(df_max_min_for_spider_plot ,df)
  colnames(df)=gsub("total_","",colnames(df))
  #radar plot
  svg(paste("radar_plot_",name[[1,1]],name[[2,1]],".svg",sep=""), 
      bg="transparent")
  radarchart(df, axistype=0 , plwd=5 , plty=1, pcol=c("mediumpurple2","tomato3"),
             cglcol="black", cglty=2, axislabcol="black", cglwd=2,vlcex=1.1 )
  #bigger lines
    segments(0.6,0.9,0.8,0.9,lwd=5, col='tomato3')
    segments(-0.6,0.9,-0.8,0.9,lwd=5, col='mediumpurple2')
  dev.off()
  }
