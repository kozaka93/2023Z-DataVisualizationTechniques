library(dplyr)

restaurants<-read.csv("C:/mojepliki/PW/TWD/Fast_Food_Restaurants_US.csv")
population<-read.csv("C:/mojepliki/PW/TWD/us_pop_by_state.csv")
restaurants$name[restaurants$name=="McDonald's"| restaurants$name=="Mcdonald's"|restaurants$name=="McDonalds's"|restaurants$name=="McDonalds"]<-"McDonald's"
restaurants$name[restaurants$name=="Sonic"| restaurants$name=="SONIC Drive In"|restaurants$name=="Sonic Drive-In"|restaurants$name=="SONIC Drive-In"]<-"Sonic"
restaurants2<-restaurants %>% 
  group_by(province,name) %>% 
  summarise(rest_num=n())
top_num_rest<-restaurants2 %>% 
  group_by(province) %>% 
  top_n(1,rest_num)
top_num_rest<-merge(top_num_rest,population,by.x="province",by.y="state_code")
top_num_rest<-top_num_rest %>% 
  select("state","name")


options("scipen"=100000000)

library(ggplot2)
library(usmap)

svg("Plot4.svg",bg="transparent")
plot_usmap(data=top_num_rest,values="name")+
  labs(title="Fast food chains with most restaurants by state")+
  theme(legend.position = "right",plot.title = element_text(hjust = 0.5,size = 16),plot.background = element_rect(fill='transparent',colour = NA),legend.background = element_rect(fill='transparent',colour = NA),legend.title =element_text(size = 14),legend.text = element_text(size=10) )+
  scale_fill_manual(limits=c("Arby's","Burger King","McDonald's","Subway","Taco Bell","Wendy's")  ,values=c("tomato3","royalblue","gold1","green4","mediumpurple2","lightpink2"),name="Restaurant")
dev.off()




