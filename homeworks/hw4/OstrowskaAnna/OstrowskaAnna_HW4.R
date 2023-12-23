---
title: "HW4"
author: "Anna Ostrowska"
date: "2023-12-11"
output: html_document
---

1.  Wizualizacja źródłowa: (w oddzielnym pliku w folderze)

-   źródło tej wizualizacji: Fakty TVN, 2020; znalezione na stronie: <https://jezykdanych.pl/wykresy-ktore-moga-wprowadzic-cie-w-blad/>
-   źródło danych do mojego wykresu: <https://github.com/owid/covid-19-data/tree/master/public/data>

2.  Uzasadnienie, jakie elementy wizualizacji wymagają poprawy:

-   wykres ma dwie różne osie Y w zależności od kraju (z prawej do 12.5k, z lewej do 125k), pokazane na tej samej osi - nie wiadomo, do których danych odnosi się która stron. Przez to np wygląda to tak, jakby najwięcej zakażeń w maju było w Polsce (a wcale tak nie było)

-   dane na wykresie nie są przeskalowane ani w żaden sposób porówanane z liczbą mieszkańców danego kraju - nie ma to zbyt dużego sensu, ponieważ jest to oczywiste, że kraj z liczbą mieszkańców o wiele większą od drugiego będzie miał więcej zakażeń, lepiej policzyć liczbę zakażeń podzieloną przez milion mieszkańców w danych kraju

-   dane dla Niemiec przedstawione w kolorze czarnym, na ciemnym tle - nie widać ich

3.  Utworzona wizualizacja wraz z kodem:

```{r}
library(plotly)
library(dplyr)

covid_cases <- read.csv("owid-covid-data.csv")

start_date <- as.Date("2020-02-15")
end_date <- as.Date("2020-05-17")

covid_cases %>% 
  filter(location %in% c("Poland", "Germany", "Spain", "Italy", "France", "World")) %>% 
  filter(date >= start_date, date <= end_date) %>% 
  select(c("location", "date", "total_cases_per_million")) -> covid_cases2

covid_cases2$total_cases_per_million[is.na(covid_cases2$total_cases_per_million)] <- 0

buttons <- list(
  list(args = list(list("mode" = list("lines"))),
       label = "no markers",
       method = "restyle"
  ),
  list(args = list(list("mode" = list("lines+markers"), "marker.symbol" = list("square"), "marker.size" = list(4),  "marker.opacity" = list(1))),
       label = "small squares",
       method = "restyle"
  ),
  list(args = list(list("mode" = list("lines+markers"), "marker.symbol" = list("diamond"), "marker.size" = list(4),  "marker.opacity" = list(0.5))),
       label = "small diamonds with opacity 0.5",
       method = "restyle"
  ),
  list(args = list(list("mode" = list("lines+markers"), "marker.symbol" = list("circle"), "marker.size" = list(4), "marker.opacity" = list(1))),
       label = "small circles",
       method = "restyle"
  ),

  list(args = list(list("mode" = list("lines+markers"), "marker.symbol" = list("circle"), "marker.size" = list(7), "marker.opacity" = list(0.5))),
       label = "big circles with opacity 0.5",
       method = "restyle"
  )
)


plot_ly(
  data = covid_cases2,
  x = ~date,
  y = ~total_cases_per_million,
  color = ~location,
  colors = "Set1",
  type = "scatter",
  mode = "lines"
) %>%   layout(
  title = "Cases of COVID-19 per million people",
  xaxis = list(title = "Date"),
  yaxis = list(title = "Total cases per million"),
  updatemenus = list(
    list(
      x = 1, y = -1,
      buttons=buttons,
      direction = "up"))
)
```

4.  Uzasadnienie, dlaczego przygotowany wykres jest lepszy od oryginalnego:

-   wszystkie kolory są widoczne

-   przedstawione przypadki Covida to ile przypada na milion mieszkańców danego kraju - dzięki temu widzimy, które kraje radziły sobie wtedy lepiej z Covidem, a nie które mają więcej mieszkańcow

-   wszystkie dane pokazane na wykresie w danym momencie mają jedną oś Y - dzięki temu można je porównać i wiadomo, jakie liczby przedstawiają. Jeśli jednak wybierzemy tylko jeden kraj do pokazania (po prawej stronie), oś Y się sama przeskaluje tak, że więcej widać. Dzięki temu dalej można odczytać dokładne dane dla każdego kraju, a nie ma takich problemów, jak na wykresie źródłowym

-   dodałam dla porównania liczbę nowych przypadków Covida na Świecie w przeliczeniu na milion osób, bo przez to, że wybrane jest tylko kilka krajów, ciężko powiedzieć, czy liczba tych przypadków Covida w tych krajach jest duża, czy mała. Po dodaniu wartości dla całego Świata już jest to łatwe do wyczytania

-   można zmieniać typ wykresu tak, aby wizualnie nam się podobał (np jeśli zbliżymy na jakieś wartości to może warto dodać markery, żeby łatwiej były wyczytać dane. Jeśli jednak patrzymy na cały ten przedział czasowy, markery psują czytelność. Dlatego dodałam możliwość wyboru opcji)

5.  Uwagi:
    -   Pomimo tego, że przedział czasowy na wykresie jest specyficzny, zdecydowałam się tego nie zmieniać, ponieważ stwierdziłam, że prawdopodobnie początek tego wykresu to okres pierwszych przypadków Koronawirusa na Świecie, a koniec - moment tworzenia wykresu źródłowego. Chociaż znaleziona przez mnie ramka danych zawierała dane dla dłuższego okresu czasu, zdecydowałam się jedynie odwzorować i poprawić błędne elementy wykresu źródłowego, a nie tworzyć inny z większa liczba danych (bo nie takie było polecenie). Dlatego okres czasu przedstawiony na wykresie jest taki, jaki jest.

    -   plik z danymi jest za duzy, zeby dalo sie go przeslac na githubie (przynajmniej mi sie wyswietla taka informacja), dlatego nie wstawiam do mojego folderu pliku z danymi. Jest on dostepny tutaj: <https://github.com/owid/covid-19-data/tree/master/public/data>.
