###########################################################################################

#   Wizualizacja źródłowa: https://x.com/EuropeElects/status/1730233161995440639?s=20

#   Jakie elementy wymagają poprawy:
#   - na wykresie brakuje legendy
#   - tytuł jest niadekwatyny do teści, nie przedstawia danych z 2024 roku
#   - dane kończą się na Listopadzie 2023, dalsze waartości na osi x są niepotrzebne
#   - nie wiadomo do którego elementu linii są przyporządkowane opisy liczbowe
#   - liczby mające ułatwić odczyt wartości z wykresy obniżają jego czytelność
#   - ciężko przyporządkować fragment łamanej do daty
#   - opis drugiej pionowej linii przerywanej nie jest w pełni widoczyny
#   - szary kolor czcionki na osiach pogarsza czytelność
#   - dwie najniższe linie mają podobny kolor

###########################################################################################


library(plotly)
library(tidyverse)


mandaty <- read.csv("mandaty.csv")

mandaty <- mandaty %>%
  mutate(Date = factor(Date, levels = unique(Date)))


plot_ly(
  data = mandaty,
  x = ~ Date,
  y = ~ Seats,
  color = ~ Parties,
  colors = "Set1",
  type = "scatter",
  mode = "lines+markers"
) %>% layout(
  title = "European Parliament Seat Projestion 2019-2023",
  titlefont = list(size = 30, family = "ArialBold"),
  font = list(size = 14),
  xaxis = list(
    title = "",
    range = c("Election 2019", "Nov-23"),
    showline = T,
    linecolor = 'rgba(204,204,204,1)',
    linewidth = 2,
    tiskmode = "auto",
    dtick = 3,
    fixedrange = T
  ),
  yaxis = list(
    title = "",
    range = c(0, 200.5),
    showline = T,
    linecolor = 'rgba(204,204,204,1)',
    linewidth = 2,
    gridwidth = 2,
    range = c(0, 200.5),
    tickvals = seq(0, 200, 20),
    fixedrange = T
  ),
  legend = list(orientation = "h"),
  shapes = list(
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = "Jan-20",
      x1 = "Jan-20",
      line = list(color = "black",
                  dash = "dot")
    ),
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = "Sep-23",
      x1 = "Sep-23",
      line = list(color = "black",
                  dash = "dot")
    )
  ),
  annotations = list(
    list(
      x = "Jan-20",
      y = 190,
      xref = "x",
      yref = "y",
      text = "Brexit",
      showarrow = FALSE,
      font = list(
        family = "ArialBold",
        size = 16,
        color = "black"
      ),
      textangle = -90,
      align = "left",
      xanchor = "left",
      yanchor = "middle"
    ),
    list(
      x = "Sep-23",
      y = 190,
      xref = "x",
      yref = "y",
      text = "Seat number\nincreased to 720",
      showarrow = FALSE,
      font = list(
        family = "ArialBold",
        size = 16,
        color = "black"
      ),
      textangle = -90,
      align = "left",
      xanchor = "left",
      yanchor = "middle"
    )
  ),
  margin = list(
    l = 50,
    r = 50,
    b = 20,
    t = 50
  )
)

###########################################################################################

#   W czym moja wizualizacja jest lepsza:
#   - dodana została legenda, dzięki plotly można wyświatlić dane dla wybranych partii
#   - poprawa widoczności wartości na osiach
#   - opisy pionowych linni przerywanych są widoczne
#   - wszystkie partie mają znacząco różniące sie od siebie kolory
#   - dodane zostały pionowe linie siatki oraz punty,
#     umożliwiające szybsze przypisanie wartości do daty
#   - interaktywność zapewnia szybki i dokładny odczyt wartości
#   - poprawiony tytuł, unusięcie niepotrzebnej części osi x

###########################################################################################