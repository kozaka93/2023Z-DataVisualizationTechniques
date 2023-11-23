library(rvest)
library(dplyr)
library(stringr)
library(maps)
library(ggplot2)
install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
library(ggflags)
library(countrycode)
library(magick)
library(cowplot)

dataURL<-"https://en.wikipedia.org/wiki/European_Cup_and_UEFA_Champions_League_records_and_statistics"

football_data_frame<-read_html(dataURL) %>% html_node(xpath='//*[@id="mw-content-text"]/div[1]/table[4]') %>%  html_table() 

#obrobka ramki

colnames(football_data_frame)[1]<-"Nation"

football_data_frame %>% group_by(Nation) %>% summarize(number_of_participants=n()) -> football_data_frame_only_count_of_participants
football_data_frame_only_count_of_participants$Nation<-word(football_data_frame_only_count_of_participants$Nation,1) 

#Zmiana nazwy Czech

football_data_frame_only_count_of_participants$Nation[8]<-"Czech Republic"

world<-map_data("world")

#Pominięcie Anglii i Szkocji, je połączymy z ramką w inny sposób

football_data_frame_only_count_of_participants %>% 
  filter(!Nation %in% c("England","Scotland"))->football_without_eng_and_scot

world %>% left_join(football_without_eng_and_scot,by=c("region"="Nation")) -> world_with_participants

#Teraz joinujemy Anglię i Szkocję

football_data_frame_only_count_of_participants %>% 
  filter(Nation %in% c("England","Scotland"))->football_with_eng_and_scot

#Zmiana nazwy aby pasowało do podregionu
football_with_eng_and_scot$Nation[1]<-"Great Britain"

world %>% 
  left_join(football_with_eng_and_scot,by=c("subregion"="Nation")) %>%
  filter(region=="UK") -> uk_data_frame

#Filtrujemy ramkę cały świat, aby nie zawierała UK
world_with_participants %>% filter(region!="UK") -> world_with_participants


#Dodajemy do ramki world_with_participants ramkę z uk
rbind(world_with_participants,uk_data_frame)-> world_with_participants

#countrycodes
world_with_participants %>% mutate(iso2=countrycode(region,"country.name","iso2c")) -> world_with_participants

#plotujemy świat
ggplot(world_with_participants,aes(x=long,y=lat,group=group,fill=number_of_participants))+ geom_polygon(color="white")+coord_sf(xlim=c(-20,50),ylim=c(30,75),expand=FALSE)


urls<-c("https://upload.wikimedia.org/wikipedia/commons/b/be/Flag_of_England.svg",
        "https://upload.wikimedia.org/wikipedia/commons/1/10/Flag_of_Scotland.svg")
lapply(urls,function(x) download.file(x,destfile = fs::path_file(x)))
fn<- fs::path_file(urls)

output_paths <- gsub("\\.svg", ".png", fn)

for (i in seq_along(fn)) {
  testimage <- image_read_svg(fn[i],width=1000)
  image_write(testimage, path = output_paths[i], format = "png")
}
england<-paste0("<img src='", output_paths[1], "' width = '15'/>")
scotland<-paste0("<img src='", output_paths[2], "' width = '15'/>")


ggplot(world_with_participants,aes(x=long,y=lat,group=group,fill=number_of_participants))+ geom_polygon(color="white")+coord_sf(xlim=c(-20,50),ylim=c(30,75),expand=FALSE) + geom_rect(aes(xmin = 0.5, xmax = 5, ymin = 54, ymax = 58), color = "red",fill="red", inherit.aes = FALSE)->plot


#zmiana koncepcji gdzieś tutaj zaczyna się właściwy kod, niestety cieżko mi było wywnioskowac interesujące mnie linijki, więc zostawiłem wszystko
rnaturalearth::ne_countries(scale = "medium",returnclass = "sf") ->test_world_2
colnames(football_data_frame_only_count_of_participants)[1]<-"nation"
left_join(test_world_2,football_data_frame_only_count_of_participants,by=c("name"="nation"))-> test_world_2_with_data

#zmiana nazwy czech

football_data_frame_only_count_of_participants$nation[8]<-"Czech Rep."

#Zmiana nazwy wielkiej brytanii
football_data_frame_only_count_of_participants$nation[10]<-"United Kingdom"

left_join(test_world_2,football_data_frame_only_count_of_participants,by=c("name"="nation"))-> test_world_2_with_data

uefa_members<-data.frame(nation=c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia and Herz.","Bulgaria","Croatia","Czech Rep.","Denmark","United Kingdom","Estonia","Faroe Islands","Finland","France","Georgia","Germany","Gibraltar","Greece","Hungary","Iceland","Israel","Italy","Kazakhstan","Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Malta","Moldova","Montenegro","Netherlands","Macedonia","Northern Ireland","Norway","Poland","Portugal","Ireland","Romania","Russia","San Marino","Scotland","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine","Wales"),
                         uefa_member=c("Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes","Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes" ,"Yes"))

test_world_2_with_data %>% left_join(uefa_members,by=c("name"="nation"))->test_world_2_with_data


test_world_2_with_data %>% mutate(fill_scale=ifelse(uefa_member=="Yes",ifelse(is.na(number_of_participants),"0",ifelse(number_of_participants<=3,"1-3",ifelse(number_of_participants<=6,"4-6",ifelse(number_of_participants<=9,"7-9",ifelse(number_of_participants<=12,"10-12",">12"))))))) -> test_fill_scale

test_fill_scale$fill_scale <- as.character(test_fill_scale$fill_scale)

test_fill_scale$fill_scale<-factor(test_fill_scale$fill_scale,levels = c(">12","10-12","7-9","4-6","1-3","0",'not an UEFA member'))

test_fill_scale %>% filter(is.na(fill_scale)) %>% mutate(fill_scale='not an UEFA member') -> fill_scale_without_uefa_member

test_fill_scale %>% filter(!is.na(fill_scale)) %>% rbind(fill_scale_without_uefa_member) -> test_fill_scale

test_fill_scale$fill_scale<-factor(test_fill_scale$fill_scale,levels = c(">12","10-12","7-9","4-6","1-3","0",'not an UEFA member'))

#plot test
ggplot(test_fill_scale,aes(fill=fill_scale)) +  geom_sf()+
coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)+theme_bw()+scale_fill_manual(values=c("#b30000","#e34a33","#fc8d59","#fdcc8a","#fef0d9","gray62","lightgray"))+ labs(title="Number of participating clubs in Champions League from respective countries")


#final version
ggplot(test_fill_scale,aes(fill=fill_scale)) +  geom_sf()+
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)+theme_bw()+scale_fill_manual(values=c("#b30000","#e34a33","#fc8d59","#fdcc8a","#fef0d9","gray62","lightgray"),name="Number of clubs")+ labs(title="Number of participating clubs in Champions League from respective countries",subtitle = "Total number of teams, which participated in competition between 1992 and 2023")+theme(plot.title = element_text(size=15))->plot

plot

##################################################################################################################################################################
##Jeśli się nie wyświetlają stopnie na skali to trzeba zmienić w ustawieniach Tools->Global Options-> Graphics -> Backend -> wybieramy Cairo z listy##############
##################################################################################################################################################################