setwd("C:\\Users\\rogal\\RStudio-workspace\\Plakat")
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)



ramka<-read.csv("ED.csv")

colnames(ramka)[4]<-"ed" #zmieniam nazwe na troche przyjemniejszą
max(ramka$Year)->maxYear #wybor max roku

ramka %>% 
  filter(Year==maxYear)->filtered1 #wybieram tylko wiersze z wybranego roku ( 1990 )

filtered1 %>% 
  select(Entity, Code)

#Tutaj część przydatna do stworzenia bazowej mapy świata, na którą będziemy nakładać mapę z danymi:

world <- ne_countries(scale = "small",returnclass = "sf")
world2<- filter(world, world$name!="Antarctica") #wyrzucam Antarktyde, bo nie ma tam danych

#wykres z bazową mapą świata:

ggplot()+
  geom_sf(data=world2)+
  theme_void()+
  theme(panel.grid = element_blank(), #usuwam uklad wspolrzednych i dodaje tlo
        plot.background = element_rect(fill="#d5d4bb"))->baseMap

#zauważyłam, że niektóre kody się powtarzają dla różnych krajów przez co są dziury w wykresie, wiec poprawiam manualnie

filtered1$Code <- ifelse(filtered1$Entity == "France", "-99", filtered1$Code)
filtered1$Code <- ifelse(filtered1$Entity == "Norway", "NOR", filtered1$Code)
filtered1$Code <- ifelse(filtered1$Entity == "Cyprus", "CYPR", filtered1$Code)
filtered1$Code <- ifelse(filtered1$Entity == "Somaliland", "SOMALILAND", filtered1$Code)
filtered1$Code <- ifelse(filtered1$Entity == "Kosovo", "KOSOVO", filtered1$Code)
world$iso_a3<- ifelse(world$name=="Norway", "NOR", world$iso_a3)
world$iso_a3<- ifelse(world$name=="N. Cyprus", "CYPR", world$iso_a3)
world$iso_a3<- ifelse(world$name=="Somaliland", "SOMALILAND", world$iso_a3)
world$iso_a3<- ifelse(world$name=="Kosovo", "KOSOVO", world$iso_a3)

#sprawdzam sobie który kod się powtarza i dla jakich krajów
duplicate_iso_codes <- world %>%
  group_by(iso_a3) %>%
  filter(n() > 1) %>%
  pull(iso_a3) %>%
  unique()

world %>% 
  filter(iso_a3=="-99") %>% 
  select(name,iso_a3)


# w naszej ramce mamy kolumne z kodami krajow, łączymy ją z ramką World, aby uzyskać dane potrzebne do zrobienia kolejnej mapy

right_join(world,filtered1,by=c("iso_a3"="Code"))->joined1


# ustawiam ręcznie palete kolorów, którymi będą pokolorowane kraje 
color_pal<- colorRampPalette(c("#618b91", "#c7870b"))(100)

#na bazową mapę nakładamy nasz wykres:

baseMap +
  geom_sf(data= joined1, aes(fill=log(ed)))+ # kolorowanie w zależności od wartości ed dla danego kraju
  
  scale_fill_gradientn(colors=color_pal, # ustawiamy naszą palete do kolorowania
                       na.value ="darkgrey",
                       labels=c("0.001%","0.01%","0.1%","1.0%"))+ #recznie ustawiam opis legendy, bo zmieniłam wartości na log()
  labs(fill ="ED")+ # w tym miejscu normalnie dodalibysmy tytul i tytl legendy, ale postanowiłsyśmy pominąc ten krok, aby użyć jednolitej czcionki już przy plakacie
  
  guides(fill=guide_colorbar(label.theme = element_text(color="navy",size=7), barheight = 0.5, barwidth =7))+ #utawiamy jeszcze kolo czcionki wartosci na legendzie, oraz dlugosc i szerokosc slupka
  theme(legend.position = "bottom",
        legend.title = element_text(color="navy", size =9, margin = margin(t = -10, unit = "pt")))->plot1



plot1 #gotowy wykres zapisujemy 






