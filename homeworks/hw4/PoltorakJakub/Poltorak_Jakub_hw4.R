library(dplyr)
library(plotly)

### Link do poprawianego wykresu ###
### https://biznes24.pl/nastroje-w-przemysle-najgorsze-od-konca-2022/ ###

### Elementy wymagające poprawy ###
# - ewidentne zaburzenie wielkości słupków -> słupek z wartością 40.9 jest około
# 2 razy mniejszy niż słupek dla wartości 48.5
# - brak podpisów dla obu osi, zwłaszcza dla osi y, dla której nie znamy dokładnej
# wartości minimalnej
# - brak informacji o roku, z którego pochodzą te dane. Są miesiące, ale niewiadomo,
# których lat dotyczą 


df<-data.frame(miesiac=c("Czerwiec 2022","Lipiec","Sierpień","Wrzesień","Październik","Listopad","Grudzień","Styczeń","Luty","Marzec","Kwiecień","Maj","Czerwiec 2023"),pmi=c(44.4,42.1,40.9,43,42,43.4,45.6,47.5,48.5,48.3,46.6,47.0,45.1))

df$miesiac_plot<-c("Cze'22","Lip","Sie","Wrz","Paź","Lis","Gru","Sty","Lut","Mar","Kwi","Maj","Cze'23")
df$miesiac_plot<-factor(df$miesiac_plot,levels=c("Cze'22","Lip","Sie","Wrz","Paź","Lis","Gru","Sty","Lut","Mar","Kwi","Maj","Cze'23"))


df %>% plot_ly() %>% add_trace(x=~miesiac_plot,y=~pmi,type='bar', marker = list(color = '#9119b8')) %>%  layout(
  title=list(text=paste0('Wartość indeksu PMI dla przemysłu w poszczególnych miesiącach 2022-2023',
               '<br>',
               '<sup>',
               'Rok do roku, dane w punktach',
               '</sup>'),
             font=list(family = "Courier New",
                       size = 20,
                       color = '#ffffff')),
            xaxis = list(           
              title = "miesiąc",    
              showgrid = F,
              color = '#ffffff',
              tickangle = -45),     
            yaxis = list(          
              title = "indeks PMI",
              color = '#ffffff')     
            ,paper_bgcolor='#5875D5',
            plot_bgcolor='#5875D5',font = list(
              family = "Courier New",
              size = 15),margin=10) -> p

p

### Moja wizualizacja ###
# - pozbyto się problemu zaburzonej wielkości słupków, rozpoczynając skalę y od 0
# - usunięto wartości poszczególnych indeksów, które wcale nie były złe, jednak 
# postanowiłem je dać po prostu jako walor interaktywny - po najechaniu na słupek
# wyświetla się wartość indeksu PMI w danym miesiącu.
# - wprowadzono również informacje o latach, które są poruszane w tym zestawieniu,
# dodano rozróżnienie pomiędzy czerwcami, aby nie było wątpliwości, który miesiąc
# dotyczy, którego roku
