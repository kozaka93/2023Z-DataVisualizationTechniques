# Autor: Kacper Rodziewicz

# Wizualizacja źródłowa: https://www.gazetasenior.pl/wybory-2023-wyniki-oficjalne-podzial-mandatow?fbclid=IwAR3wrs1gQaclWMWY9EdJGZ0kFq-1XIY0LmnWbW1UKxrEi00iU9BGR4Rq4B8
# Co wymaga poprawy?
# Procent głosów jest bez sensu podawany z dokładnością do dwóch cyfr po przecinku, poza tym podane procenty w skali są nieco krzywe, powinny być trochę przesunięte w prawo.

#Utworzona wizualizacja:
library(plotly)
library(dplyr)

data <- data.frame(
  Party = c("Prawo i Sprawiedliwość  ", "Koalicja Obywatelska  ", "Trzecia Droga  ", 
            "Nowa Lewica  ", "Konfederacja  ", "Bezpartyjni Samorządowcy", "Polska Jest Jedna"),
  Votes = c(35.4, 30.7, 14.4, 8.61, 7.16, 1.86, 1.63), 
  TotalVotes = c(7640854, 6629402, 3110670, 1859018, 1547364, 401054, 351099),
  Mandates = c(194, 157, 65, 26, 18, 0, 0) 
)

sorted_data <- arrange(data, desc(Votes))

sorted_data$HoverText <- paste("Partia: ", sorted_data$Party, 
                               "<br>Wynik procentowy: ", sorted_data$Votes, "%", 
                               "<br>Liczba głosów: ", sorted_data$TotalVotes, 
                               "<br>Liczba mandatów: ", sorted_data$Mandates, sep="")


fig <- plot_ly(sorted_data, x = ~Votes, y = ~Party, type = 'bar', orientation = 'h',
               hovertext = ~HoverText, hoverinfo = "text")
fig <- fig %>% layout(
  title = "Wyniki głosowania do Sejmu 2023 [procentowe]",
  xaxis = list(
    title = "Procent głosów",
    showgrid = TRUE,
    gridcolor = 'rgba(50,50,50,1)',
    gridwidth = 1
  ),
  yaxis = list(
    title = "",
    categoryorder = "total ascending",
    automargin = TRUE
  ),
  margin = list(l = 170),
  hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12, color = "black"))
)
fig

#Dlaczego ten wykres jest lepszy?
#Jest lepszy tylko nieznacznie, ale wciąż lepszy, nie ma bezsensownego podawania dwóch cyfr po przecinku, dodatkowo
#jest wprowadzona interaktywnosć, która dostarcza wszystkie potrzebne informacje o wyborach.


