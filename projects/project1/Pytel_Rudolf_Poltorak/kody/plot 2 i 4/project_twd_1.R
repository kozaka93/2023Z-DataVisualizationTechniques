library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(treemap)
library(countrycode)
library(ggimage)
library(showtext)

pp_recipes<-read.csv('D:\\R\\PP_recipes.csv')
pp_users<-read.csv('D:\\R\\PP_users.csv')
raw_interactions<-read.csv('D:\\R\\RAW_interactions.csv')
raw_recipes<-read.csv('D:\\R\\RAW_recipes.csv')
interactions_train<-read.csv('D:\\R\\interactions_train.csv')
interactions_test<-read.csv('D:\\R\\interactions_test.csv')
interactions_validation<-read.csv('D:\\R\\interactions_validation.csv')

raw_interactions %>% select(recipe_id,rating) %>% group_by(recipe_id) %>% 
  summarise(count=length(recipe_id),mean_rating=mean(rating)) %>% filter(count>=50) %>% arrange(mean_rating) %>% head(10) 

raw_interactions %>% select(date) %>% mutate(month=strftime(date,"%m"),day=strftime(date,"%d")) -> raw_interactions_only_date

raw_interactions_only_date %>% group_by(month,day) %>% summarise(count=length(month)) %>% arrange(-count)->month_day_groupping

month_day_groupping %>% filter(month=="01") %>% ggplot(aes(x=day,y=count)) + geom_col()

month_day_groupping %>% group_by(month) %>% summarise(mean=mean(count)) %>% select(mean) %>% head(1) -> mean_january_reviews
as.numeric(mean_january_reviews)-> mean_january_reviews

month_day_groupping %>% filter(month=="01") %>% ggplot(aes(x=day,y=count)) + geom_col(fill="lightblue")+ ggtitle("Ilość wystawianych opinii w poszczególnych dniach w styczniu")->plot_data

plot_data + geom_hline(yintercept = mean_january_reviews,color="red") + annotate("text",x=25,y=mean_january_reviews+100,label="Średnia dla pojedynczego dnia w styczniu") -> plot_data_with_first_line

month_day_groupping %>% group_by(month) %>% summarise(mean=mean(count)) -> considered_data 
sum(considered_data$mean)/12 -> mean_year_review

plot_data_with_first_line + geom_hline(yintercept = mean_year_review,color="purple") + annotate("text",x=25,y=mean_year_review+100,label="Średnia dla pojedynczego dnia w roku")

month_day_groupping %>% group_by(month) %>% summarise(mean=mean(count)) %>% select(mean) %>% tail(1) -> mean_december_reviews
as.numeric(mean_december_reviews)-> mean_december_reviews


month_day_groupping %>% filter(month=="12") %>% ggplot(aes(x=day,y=count)) + geom_col(fill="chocolate1")+ ggtitle("Ilość wystawianych opinii w poszczególnych dniach w grudniu") -> plot_december

plot_december + geom_hline(yintercept = mean_december_reviews,color="red") + annotate("text",x=25,y=mean_december_reviews+100,label="Średnia dla pojedynczego dnia w grudniu") -> plot_data_december_first_line

plot_data_december_first_line + geom_hline(yintercept = mean_year_review,color="purple") + annotate("text",x=7,y=mean_year_review+100,label="Średnia dla pojedynczego dnia w roku")

#raw_recipes with delimited_data
raw_recipes$nutrition<-str_sub(raw_recipes$nutrition,2,-2) 

raw_recipes %>% separate_wider_delim(nutrition,delim=",",names=c("calories","total_fat","sugar","sodium","protein","saturated_fat","carbohydrates")) -> raw_recipes_with_delimited_calories
raw_recipes_with_delimited_calories$calories<- as.numeric(raw_recipes_with_delimited_calories$calories)
raw_recipes_with_delimited_calories$total_fat<- as.numeric(raw_recipes_with_delimited_calories$total_fat)
raw_recipes_with_delimited_calories$sugar<- as.numeric(raw_recipes_with_delimited_calories$sugar)
raw_recipes_with_delimited_calories$sodium<- as.numeric(raw_recipes_with_delimited_calories$sodium)
raw_recipes_with_delimited_calories$protein<- as.numeric(raw_recipes_with_delimited_calories$protein)
raw_recipes_with_delimited_calories$saturated_fat<- as.numeric(raw_recipes_with_delimited_calories$saturated_fat)
raw_recipes_with_delimited_calories$carbohydrates<- as.numeric(raw_recipes_with_delimited_calories$carbohydrates)




#minutes per step bardzo dużo alkoholu
raw_recipes_with_delimited_calories %>% mutate(minutes_per_step=minutes/n_steps) %>% arrange(-minutes_per_step) %>% head(10) %>% select(name,minutes_per_step)


#patrzymy po najmniejszym cukrze
raw_recipes_with_delimited_calories %>% arrange(sugar) %>% head(10) %>% select(name)

#posortowane po kaloriach i cukrze
raw_recipes_with_delimited_calories %>% arrange(sugar) %>% filter(sugar==0) %>% arrange(-calories) -> zero_sugar_ordered_calories

#wykresy histogramowe dla białka,... i 0 cukru
zero_sugar_ordered_calories %>% filter(protein<500) %>%  ggplot(aes(x=protein)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(protein<100) %>%  ggplot(aes(x=protein)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(carbohydrates<500) %>%  ggplot(aes(x=carbohydrates)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(carbohydrates<100) %>%  ggplot(aes(x=carbohydrates)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(sodium<500) %>%  ggplot(aes(x=sodium)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(sodium<100) %>%  ggplot(aes(x=sodium)) + geom_histogram(binwidth = 10)
zero_sugar_ordered_calories %>% filter(total_fat<100) %>%  ggplot(aes(x=total_fat)) + geom_histogram(binwidth = 10)


#wykresy gęstości
zero_sugar_ordered_calories %>% filter(protein<100) %>%  ggplot(aes(x=protein)) + geom_density(alpha=.2, fill="#FF6666") +ggtitle("Procent dziennego zapotrzebowania na białko w posiłku a częstość występowania w przepisach")
raw_recipes_with_delimited_calories %>% filter(protein<100) %>%  ggplot(aes(x=protein)) + geom_density(alpha=.2, fill="#FF6666") +ggtitle("Procent dziennego zapotrzebowania na białko w posiłku a częstość występowania w przepisach")
raw_recipes_with_delimited_calories %>% filter(carbohydrates<100) %>%  ggplot(aes(x=carbohydrates)) + geom_density(alpha=.2, fill="#FF6666") +ggtitle("Procent dziennego zapotrzebowania na węglowodany w posiłku a częstość występowania w przepisach")
raw_recipes_with_delimited_calories %>% filter(sugar<100) %>%  ggplot(aes(x=sugar)) + geom_density(alpha=.2, fill="#FF6666") +ggtitle("Procent dziennego zapotrzebowania na cukry w posiłku a częstość występowania w przepisach")

raw_recipes_with_delimited_calories %>% filter(total_fat<100) %>%  ggplot(aes(x=total_fat)) + geom_density(alpha=.2, fill="#FF6666") +ggtitle("Procent dziennego zapotrzebowania na tłuszcze w posiłku a częstość występowania w przepisach")
#korelacje między produktami

cor(raw_recipes_with_delimited_calories$sugar,raw_recipes_with_delimited_calories$protein) #0.19 cukier,białko
cor(raw_recipes_with_delimited_calories$sugar,raw_recipes_with_delimited_calories$calories) #0.86 cukier,kalorie
cor(raw_recipes_with_delimited_calories$protein,raw_recipes_with_delimited_calories$calories) #0.48 białko,kalorie
cor(raw_recipes_with_delimited_calories$total_fat,raw_recipes_with_delimited_calories$calories)

#macierz korelacji
raw_recipes_with_delimited_calories %>% select(calories,total_fat,sugar,sodium,protein,saturated_fat,carbohydrates) %>% cor() %>% round(2)->correlation_data

#wykres korelacji

corrplot(correlation_data, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# białko 50g tłuszcz 70g tłuszcze nasycone 20g wegle 260 cukier 90g sód 1,5
raw_recipes_with_delimited_calories_and_grammed_nutrition <- raw_recipes_with_delimited_calories
raw_recipes_with_delimited_calories_and_grammed_nutrition$total_fat<- raw_recipes_with_delimited_calories_and_grammed_nutrition$total_fat * 70/100
raw_recipes_with_delimited_calories_and_grammed_nutrition$sugar<- raw_recipes_with_delimited_calories_and_grammed_nutrition$sugar * 90/100
raw_recipes_with_delimited_calories_and_grammed_nutrition$sodium<- raw_recipes_with_delimited_calories_and_grammed_nutrition$sodium * 1.5/100
raw_recipes_with_delimited_calories_and_grammed_nutrition$protein<- raw_recipes_with_delimited_calories_and_grammed_nutrition$protein * 50/100
raw_recipes_with_delimited_calories_and_grammed_nutrition$saturated_fat<- raw_recipes_with_delimited_calories_and_grammed_nutrition$saturated_fat * 20/100
raw_recipes_with_delimited_calories_and_grammed_nutrition$carbohydrates<- raw_recipes_with_delimited_calories_and_grammed_nutrition$carbohydrates * 260/100

#macierz korelacji dla danych przeskalowanych
raw_recipes_with_delimited_calories_and_grammed_nutrition %>% select(calories,total_fat,sugar,sodium,protein,saturated_fat,carbohydrates) %>% cor() %>% round(2) -> correlation_data_2
#wychodzi to samo xD

#protein per minute
raw_recipes_with_delimited_calories %>% filter(minutes!=0,protein<=100) %>% mutate(protein_per_minute=protein/minutes) %>% arrange(-protein_per_minute) %>% select(name,protein_per_minute)

#ramka interactions z datą
raw_interactions_with_date<- raw_interactions %>% mutate(year=strftime(date,"%y"),month=strftime(date,"%m"),day=strftime(date,"%d"))

#wykres sredniej oceny w poszczególnych latach
raw_interactions_with_date %>% group_by(year) %>% summarise(mean_rating=mean(rating)) %>% select(year,mean_rating) ->year_rating_mean_plot_data
year_rating_mean_plot_data %>% ggplot(aes(x=year,y=mean_rating)) + geom_bar(stat="identity")

#wykres pokolorowany itd xd

year_rating_mean_plot_data %>% ggplot(aes(x=year,y=mean_rating)) + geom_bar(stat="identity",fill="#69b3a2") + theme_light() + ggtitle("Średnia wartość recenzji dla przepisu w poszczególnych latach")

#sredni rating dla kazdego przepisu
raw_interactions_with_date %>% group_by(recipe_id) %>% summarise(mean_rating=mean(rating),count=length(rating)) -> mean_rating_for_recipe
colnames(mean_rating_for_recipe)[1]<-"id"


#joinowanie przepisów po id
raw_recipes_with_delimited_calories %>% inner_join(mean_rating_for_recipe,by="id") ->merged_id_rating

#dodajemy kolumne zero sugar patrzymy na oceny
merged_id_rating %>% mutate(zero_sugar=ifelse(sugar==0,1,0)) -> merged_id_rating_with_zero_sugar
merged_id_rating_with_zero_sugar %>% group_by(zero_sugar) %>% summarise(mean_rating_for_sugar=mean(mean_rating))
#okazuje sie ze mają podobne oceny
#wykres z tymi danymi
merged_id_rating_with_zero_sugar %>% group_by(zero_sugar) %>% summarise(mean_rating_for_sugar=mean(mean_rating)) %>% ggplot(aes(x=zero_sugar,y=mean_rating_for_sugar)) + geom_bar(stat="identity")

#wykres z srednia ratingu dla poszczegolnych ilosci krokow w przepisie
merged_id_rating %>% group_by(n_steps) %>% summarise(mean_rating_for_steps=mean(mean_rating)) %>% ggplot(aes(x=n_steps,y=mean_rating_for_steps)) + geom_bar(stat="identity")

#dane z ilością przepisów z poszczególnymi krokami
merged_id_rating %>% group_by(n_steps) %>% summarise(mean_rating_for_steps=mean(mean_rating),count_of_recipes=length(mean_rating))

#wykres dla liczby przepisów z poszczególnymi krokami
merged_id_rating %>% group_by(n_steps) %>% summarise(mean_rating_for_steps=mean(mean_rating),count_of_recipes=length(mean_rating)) %>% ggplot(aes(x=n_steps,y=count_of_recipes)) + geom_bar(stat="identity")

#podzielone dane na grupy po 20/cukier
merged_id_rating %>% mutate(number_of_sugar_group=ifelse(sugar<100,sugar %/% 20,5)) ->sugar_20_division
sugar_20_division %>% group_by(number_of_sugar_group) %>% summarise(mean_sugar_rating=mean(mean_rating))
#wszystko podobnie

#podzielone dane na grupy po 10/cukier
merged_id_rating %>% mutate(number_of_sugar_group=ifelse(sugar<100,sugar %/% 10,5)) ->sugar_10_division
sugar_10_division %>% group_by(number_of_sugar_group) %>% summarise(mean_sugar_rating=mean(mean_rating))
#też


#20/węgle
merged_id_rating %>% mutate(number_of_carbs_group=ifelse(carbohydrates<100,carbohydrates %/% 20,5)) ->carbohydrates_20_division
carbohydrates_20_division %>% group_by(number_of_carbs_group) %>% summarise(mean_sugar_rating=mean(mean_rating)) ->carbs_plots

#trochę większe niższe grupy


#10/węgle
merged_id_rating %>% mutate(number_of_sugar_group=ifelse(carbohydrates<100,carbohydrates %/% 10,5)) ->carbohydrates_10_division
carbohydrates_10_division %>% group_by(number_of_sugar_group) %>% summarise(mean_sugar_rating=mean(mean_rating))
#dla 10 podobnie

#liczymy korelację
cor(carbohydrates_10_division$number_of_sugar_group,carbohydrates_10_division$mean_rating)
#-0.03 -> brak korelacji xD

#dodajemy wszystkie grupy (4 najbardziej interesujące pozycje)
merged_id_rating %>% mutate(number_of_sugar_group=ifelse(sugar<100,sugar %/% 20,5),number_of_total_fat_group=ifelse(total_fat<100,total_fat %/% 20,5),number_of_carbohydrates_group=ifelse(carbohydrates<100,carbohydrates %/% 20,5),number_of_protein_group=ifelse(protein<100,protein %/% 20,5)) -> four_interesting_groups_20_division

#posortowane malejąco i dodany mean_combined_rating
four_interesting_groups_20_division %>% group_by(number_of_sugar_group,number_of_total_fat_group,number_of_carbohydrates_group,number_of_protein_group) %>% summarise(mean_combined_rating=mean(mean_rating),count=length(mean_rating)) %>% arrange(-mean_combined_rating)->arranged_four_interesting_groups_20_division

#usuwamy te pozycje w których count jest mniejszy równy 50
arranged_four_interesting_groups_20_division %>% filter(count>50)

#spójrzmy na posiłki o najniższych ocenach
arranged_four_interesting_groups_20_division %>% filter(count>50) %>% arrange(mean_combined_rating)
#okazuje sie ze sporo z nich zawiera dużo cukru 

# z kolei 10 najlepszych charakteryzuje to, że są one z niskiej grupy węglowodanowej
arranged_four_interesting_groups_20_division %>% filter(count>50) %>% arrange(-mean_combined_rating)

#wykres dla wszystkich posiłków z low carb
arranged_four_interesting_groups_20_division %>% filter(count>50) %>% arrange(-mean_combined_rating) %>% filter(number_of_carbohydrates_group<1) %>% ggplot(aes(x=paste(number_of_sugar_group,number_of_total_fat_group,number_of_protein_group),y=mean_combined_rating)) + geom_bar(stat="identity")

#wracamy się do węgli w grupie zero
merged_id_rating %>% mutate(number_of_carb_group=ifelse(carbohydrates<20,0,1)) -> carb_20_division_two_groups
carb_20_division_two_groups %>% group_by(number_of_carb_group) %>% summarise(mean_carb_rating=mean(mean_rating))
#różnica 0,07%

#najpopularniejsze przepisy
merged_id_rating %>% arrange(-count) %>% select(name,mean_rating,count)

#korelacja z ilością ocen a średnią oceną
cor(merged_id_rating$mean_rating,merged_id_rating$count)

#korelacja między ilością ocen a srednią oceną
cor(merged_id_rating$minutes,merged_id_rating$mean_rating)


#podział na poszczególne grupy w tagach
merged_id_rating %>% filter(grepl("chinese",tags)) -> chinese_id_rating
chinese_id_rating %>% mutate(origin="china") -> chinese_id_rating

merged_id_rating %>% filter(grepl("canadian",tags)) -> canadian_id_rating
canadian_id_rating %>% mutate(origin="Canada") -> canadian_id_rating

merged_id_rating %>% filter(grepl("french",tags)) -> french_id_rating
french_id_rating %>% mutate(origin="France")-> french_id_rating

merged_id_rating %>% filter(grepl("indian",tags)) -> indian_id_rating
indian_id_rating %>% mutate(origin="India") -> indian_id_rating

merged_id_rating %>% filter(grepl("mexican",tags)) -> mexican_id_rating
mexican_id_rating %>% mutate(origin="Mexico") -> mexican_id_rating

merged_id_rating %>% filter(grepl("italian",tags)) -> italian_id_rating
italian_id_rating %>% mutate(origin="Italy") ->italian_id_rating

merged_id_rating %>% filter(grepl("greek",tags)) -> greek_id_rating
greek_id_rating %>% mutate(origin="Greece")-> greek_id_rating

rbind(italian_id_rating,mexican_id_rating,indian_id_rating,
      french_id_rating,canadian_id_rating,greek_id_rating)-> summarized_data_cuisine_id_rating

summarized_data_cuisine_id_rating$iso2<-countrycode(summarized_data_cuisine_id_rating$origin,"country.name","iso2c")

summarized_data_cuisine_id_rating %>% group_by(origin,iso2) %>% 
  summarise(mean_sugar=mean(sugar),mean_carbs=mean(carbohydrates),
            mean_total_fat=mean(total_fat),mean_protein=mean(protein),
            mean_saturated_fat=mean(saturated_fat),mean_sodium=mean(sodium),
            mean_calories=mean(calories))-> test_treemap

#lolipop
test_treemap %>% ggplot(aes(x=origin,y=mean_carbs))+ 
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_carbs),size=1,color="blue",linetype="dotdash")+
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+coord_flip()

#lolipop with flagues
test_treemap %>% ggplot(aes(x=origin,y=mean_carbs))+geom_flag(y=-1,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_carbs),size=1,color="blue",linetype="dotdash")+
  geom_point( size=7, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+coord_flip()+ylim(-1,16)

mean_carbs2<- mean(summarized_data_cuisine_id_rating$carbohydrates)


test_treemap %>% ggplot(aes(x=origin,y=mean_sugar))+geom_flag(y=-4,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_sugar),size=1,color="blue",linetype="dotdash")+
  geom_point( size=7, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+coord_flip()+ylim(-4,95)+theme_minimal()

test_treemap$mean_sugar<-round(test_treemap$mean_sugar)

test_treemap %>% ggplot(aes(x=origin,y=mean_sugar))+geom_flag(y=-4,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_sugar),size=1.5,color="blue",)+
  geom_point( size=8, color="blue", fill="blue", shape=21, stroke=2)+ geom_text(aes(label=mean_sugar),color="white",size=4)+coord_flip()+ylim(-4,95)+theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4"),plot.background = element_rect(fill="#fffad4"))+labs(title="Comparison of sugar content in recipes for respective cousines",x="Cousine")


test_treemap %>% ggplot(aes(x=origin,y=mean_sugar))+geom_flag(y=-4,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_sugar),size=1.5,color="blue",)+
  geom_point( size=8, color="blue", fill="blue", shape=21, stroke=2)+ geom_text(aes(label=mean_sugar),color="white",size=4)+coord_flip()+ylim(-4,95)+theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4",colour="#f0e6b4"),panel.border = element_blank(),plot.background = element_rect(fill="#fffad4",colour="#fffad4"),plot.title = element_text(hjust=0.5))+labs(title="Comparison of sugar content in recipes for respective cousines",x="Cousine")

font_add(family="Caveat_Brush",regular="C:\\Users\\jakub\\OneDrive\\Dokumenty\\Caveat_Brush\\CaveatBrush-Regular.ttf")
showtext_auto()



#final for now

test_treemap %>% ggplot(aes(x=origin,y=mean_sugar))+geom_flag(y=-4,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_sugar),size=1.5,color="blue",)+
  geom_point( size=8, color="blue", fill="blue", shape=21, stroke=2)+ geom_text(aes(label=mean_sugar),color="white",size=4)+coord_flip()+ylim(-4,95)+
  theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4",colour="#f0e6b4"),panel.border = element_blank(),plot.background = element_rect(fill="#fffad4",colour="#fffad4"),plot.title = element_text(hjust=0.5,size=35,colour="#4c410c"),text = element_text(family = "Caveat_Brush",size=20),axis.title = element_text(colour = "#4c410c"))+
  labs(title="Comparison of sugar content in recipes for respective cousines",x="Cousine",y="Mean percent of daily sugar requirement in one recipe")



#final for now 2.0
test_treemap %>% ggplot(aes(x=origin,y=mean_sugar))+geom_flag(y=-4,aes(image=iso2),size=0.1)+
  geom_segment(aes(x=origin,xend=origin,y=0,yend=mean_sugar),size=1.5,color="blue",)+
  geom_point( size=8, color="blue", fill="blue", shape=21, stroke=2)+ geom_text(aes(label=mean_sugar),color="white",size=4)+coord_flip()+ylim(-4,95)+theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4",colour="#f0e6b4"),panel.border = element_blank(),plot.background = element_rect(fill="#fffad4",colour="#fffad4"),plot.title = element_text(hjust=0.5,size=35,colour="#4c410c"),text = element_text(family = "Caveat_Brush",size=20),axis.title = element_text(colour = "#4c410c"))+labs(title="Comparison of sugar content in recipes for respective cousines",x="Cousine",y="Mean percent of daily sugar requirement in one recipe")


#checking amount of recipes
mexican_id_rating %>% filter(sugar>100) %>% nrow()-> mexican_over_100
mexican_over_100/nrow(mexican_id_rating)
#6%

italian_id_rating %>% filter(sugar>100) %>% nrow()-> italian_over_100
italian_over_100/nrow(italian_id_rating)
#5%

indian_id_rating %>% filter(sugar>100) %>% nrow() -> indian_over_100
indian_over_100/nrow(indian_id_rating)

#10%

greek_id_rating %>% filter(sugar>100) %>% nrow()-> greek_over_100
greek_over_100/nrow(greek_id_rating)
#6%

french_id_rating %>% filter(sugar>100) %>% nrow()-> french_over_100
french_over_100/nrow(french_id_rating)
#12%

canadian_id_rating %>% filter(sugar>100) %>% nrow() -> canadian_over_100
canadian_over_100/nrow(canadian_id_rating)
#18%


#plot nr 3 with 
#data with colours
data_colours<- as.data.frame(origin=c("Canada","France","Greece","India","Italy","Mexico"))

test_treemap$color<-c("#F37B6B","#5464AF","#B6D078","#F0A830","#78C0A8","#F7966B")
colnames(test_treemap)[10]<-"color1"

#c("#F7966B","#78C0A8","#F0A830","#B6D078","#5464AF","#F37B6B")

#prototyp nr 4
test_treemap %>% ggplot(aes(x=origin,y=mean_sugar,fill=color1))+geom_flag(y=-4,aes(image=iso2),size=0.1)+geom_bar(stat="identity",width = 0.7)+geom_text(aes(label=mean_sugar),color="white",size=4)+coord_flip()+ylim(-4,95)+theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4",colour="#f0e6b4"),panel.border = element_blank(),plot.background = element_rect(fill="#fffad4",colour="#fffad4"),plot.title = element_text(hjust=0.5,size=35,colour="#4c410c"),text = element_text(family = "Caveat_Brush",size=20),axis.title = element_text(colour = "#4c410c"))+labs(title="Comparison of sugar content in recipes for respective cousines",x="Cousine",y="Mean percent of daily sugar requirement in one recipe")+scale_fill_manual(values = c("#F7966B","#78C0A8","#F0A830","#B6D078","#5464AF","#F37B6B"))+theme(legend.position = "none")


#final 5.0
test_treemap %>% ggplot(aes(x=origin,y=mean_sugar,fill=color1))+geom_flag(y=-4,aes(image=iso2),size=0.1)+geom_bar(stat="identity",width = 0.7)+scale_fill_identity()+geom_text(aes(label=mean_sugar),hjust=1.5,color="#4c410c",size=8)+coord_flip()+ylim(-4,95)+theme_minimal()+theme(panel.background = element_rect(fill = "#f0e6b4",colour="#f0e6b4"),panel.border = element_blank(),plot.background = element_rect(fill="#fffad4",colour="#fffad4"),plot.title = element_text(hjust=0.5,size=35,colour="#4c410c"),text = element_text(family = "Caveat_Brush",size=20),axis.title = element_text(colour = "#4c410c"))+labs(title="Comparison of sugar content in recipes for respective cuisines",x="Cousine",y="Mean percent of daily sugar requirement in one recipe")