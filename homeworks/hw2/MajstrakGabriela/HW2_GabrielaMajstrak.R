library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


df <- data.frame(
  imie_nazwisko = c('Rafał Trzaskowski', 'Szymon Hołownia', 'Andrzej Duda', 'Donald Tusk', 'Władysław Kosiniak Kamysz', 'Robert Biedroń',
                    'Mateusz Morawiecki','Włodzimierz Czarzasty', 'Tomasz Grocki', 'Elżbieta Witek'),
  zdecydowanie_ufa = c(31.2, 11.9, 22.3, 22.5, 11.2, 6.2, 17.4, 8.6, 14.7, 14.7),
  raczej_ufa = c(17.7, 33.1, 19.2, 18.1, 29.2, 28.1, 16.6, 23.1, 16.5, 15.4),
  obojętność = c(13.0, 22.7, 11.9, 10.1, 24.3, 23.1, 7.0, 23.6, 20.1, 14.0),
  raczej_nie_ufa = c(8.6, 13.2, 10.6, 18.1, 16.4, 11.7, 14.2, 11.5, 7.8, 9.9),
  zdecydowanie_nie_ufa = c(26.2, 17.2, 35.9, 31.1, 16.2, 29.0, 44.7, 21.5, 26.3, 38)
  
  
)

df <- df %>%  mutate(nie_udzielono_odpowiedzi = 100 - zdecydowanie_ufa - raczej_ufa - obojętność - raczej_nie_ufa - zdecydowanie_nie_ufa) %>%
  pivot_longer(!imie_nazwisko, names_to = 'poziom_zaufania', values_to = 'count') 

imie_nazwisko_order <- c("Robert Biedroń", "Włodzimierz Czarzasty", "Władysław Kosiniak Kamysz", "Szymon Hołownia", "Elżbieta Witek", "Tomasz Grocki", "Mateusz Morawiecki", "Andrzej Duda", "Donald Tusk", "Rafał Trzaskowski")
df$imie_nazwisko <- factor(df$imie_nazwisko, levels = imie_nazwisko_order)

df <- df %>%
  group_by(imie_nazwisko) %>%
  mutate(total_count = sum(count),
         percentage = count / total_count * 100)



 wykres <- ggplot(df, aes(y = imie_nazwisko, x = count, fill = factor(poziom_zaufania, levels = c('nie_udzielono_odpowiedzi', 'zdecydowanie_nie_ufa', 'raczej_nie_ufa', 'obojętność', 'raczej_ufa','zdecydowanie_ufa'))))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =c('grey','#F24236', '#F8764F', '#FDE74C','#47C8FF', '#4D8B31'))+
  labs(fill ="Poziom zaufania")+
  labs(x = 'poziom zaufania w %', y = "imię i nazwisko polityka", title ='Wykres zaufania do polityków po wyborach parlamentarnych 2023')+
  #geom_text(data = df %>% filter(poziom_zaufania != "nie_udzielono_odpowiedzi"), aes(label = as.character(count)), position = position_stack(vjust = 0.5))
  geom_text(data = df %>% filter(poziom_zaufania != "nie_udzielono_odpowiedzi"), aes(label = paste(as.character(count), "%")), position = position_stack(vjust = 0.55))
   

ggplotly(wykres) 
  
