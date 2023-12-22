#praca domowa numer 4:

#dane źródłowe: https://rpo.slaskie.pl/czytaj/w_co_zainwestujemy_fe_konsultacje_up_2021_2027
#co jest nie tak z tym wykresem:

# Wykres źródłowy jest 3D, co zaburza proporcje poszczególnych części. Wartoby równierz przedstawić te dane
# na innym typie wykresu, na przykład kolumnowym.


#kod do utworzenia poprawnego wykresu:

library(dplyr)
library(plotly)
library(forcats)
text = ~paste("Typ alokacji: ", alokacje, "\nWartość: ", srodki_mln_euro, "mln euro")
alokacje <- c("Bardziej Inteligentna Europa", "Bardziej Zielona Europa", "Lepiej Połączona Europa", "Bardziej społeczna Europa", "Europa Bliżej Obywateli", "Sprawiedliwa Transformacja", "Pomoc Techniczna")
srodki_mln_euro <- c(11784, 20536, 17558, 14768, 4749, 4234, 2343)

dane <- data.frame(alokacje, srodki_mln_euro)
dane <- dane %>% arrange(srodki_mln_euro)
dane$alokacje <- fct_inorder(dane$alokacje)
fig <- plot_ly(
  data = dane,
  y = ~alokacje,
  x = ~srodki_mln_euro,
  type = 'bar',
  text = ~paste("Typ alokacji: ", alokacje, "\nWartość: ", srodki_mln_euro, "mln euro"),
  hoverinfo = 'text',
  marker = list(color = "darkgreen",
                line = list(color = "black",
                            width = 1.5))
) %>%
  layout(
    xaxis = list(tickformat = "d", title = "Środki (w mln euro)"),
    yaxis = list(title = "Typ Alokacji"),
    updatemenus = list(
      list(
        type = 'buttons',
        x = 1, y = 0.4,
        buttons = list(
          list(args = list(list("hoverinfo" = list("text"))),
               label = "włącz hoverinfo",
               method = "restyle"),
          list(args = list(list("hoverinfo" = list('none'))),
               label = "wyłącz hoverinfo",
               method = "restyle")
        )
      ),
      list(
        x = 1, y = 0,
        type = 'buttons',
        buttons = list(
          list(args = list(list("marker.color" = list("black"))),
              label = "czarny",
              method = "restyle"),
          list(args = list(list("marker.color" = list('#253eb8'))),
               label = "niebieski",
               method = "restyle"),
          list(args = list(list("marker.color" = list("#197908"))),
               label = "zielony",
               method = "restyle"),
          list(args = list(list("marker.color" = list("#ff9559"))),
               label = "pomarańczowy",
               method = "restyle")
        )
    )
    ))

fig

#Dlaczego mój wykres jest lepszy:
# Odczytanie danych z wykresu kolumnowego jest o wiele prostsze, a efekt 3D nie zaburza proporcji wykresu.
# Wykres jest interaktywny: po najechaniu na kolumnę można odczytać szczegółowe dane na temat każdej alokacji, można
# równierz wyłączyć tą funkcję czy zmienić kolor wykresu