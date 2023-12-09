library(plotly)
#Link do wizualizacji źródłowej: https://inzynierbudownictwa.pl/images/Agnieszka/IB%2005%202020/ceny%20rur%20instalacyjnych.jpg
#Link do źródła: https://inzynierbudownictwa.pl/ceny-materialow-budowlanych-w-obiektach-kubaturowych-w-2019-r/
#Pierwszy wykres jest mniej poprawny, ponieważ nie ma etykiety osi y (jest podana tylko, że jest to zł/m, ale też jest to w dziwnym miejscu),
#dziwny dobór kolorów oraz nieprzyjazny format daty, a także brak uwzlędnienia miesięcy maj-grudzień w roku 2018 na wykresie, brak tytułu.
df <- data.frame(
  Produkt = c(
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Kroćce PVC - 50 mm, tpyu P do kanalizacji wewnętrzenej",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 25 mm, PE-SDR 17,6",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa",
    "Rury - 16 mm, PVC, bezkielichowe, na ćiśnienie 1,0 MPa"
  ),
  Cena = c(3.38, 3.33, 3.35, 3.33, NA, NA, NA, NA, NA, NA, NA, NA, 3.41, 2.83, 2.92, 2.84, 3.08, NA, NA, NA, NA, NA, NA, NA, NA, 3.20, 2.22, 2.19, 2.17, 2.15, NA, NA, NA, NA, NA, NA, NA, NA, 2.13),
  Data = c(
    "01.2019", "02.2019", "03.2019", "04.2019", "05.2019", "06.2019", "07.2019", "08.2019", "09.2019", "10.2019", "11.2019", "12.2019", "01.2020",
    "01.2019", "02.2019", "03.2019", "04.2019", "05.2019", "06.2019", "07.2019", "08.2019", "09.2019", "10.2019", "11.2019", "12.2019", "01.2020",
    "01.2019", "02.2019", "03.2019", "04.2019", "05.2019", "06.2019", "07.2019", "08.2019", "09.2019", "10.2019", "11.2019", "12.2019", "01.2020"
  )
)

df$Data <- as.Date(paste("01.", df$Data, sep = ""), format = "%d.%m.%Y")


fig <- plot_ly(df, x = ~Data, y = ~Cena, color = ~Produkt, type = 'scatter', mode = 'lines+markers', marker = list(size = 10, opacity = 0.8), connectgaps = TRUE)


fig <- fig %>% 
  layout(
    title = list(text = "Ceny rur instalacyjnych do wody i kanalizacji <br><sup>Na przestrzeni styczeń 2019 - styczeń 2020</sup>", x = 0.05, y = 0.96),
    yaxis = list(title = "Cena [zł/m]"),
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = df$Data,
      ticktext = format(df$Data, "%b %Y"),
      tickangle = -45
    ),
    legend = list(x = 0.96, y = 0.5)
  )

fig

#Mój wykres jest lepszy, ponieważ uzupełniłem etykietę osi y, zmieniłem miesiące na skrócone nazwy i są one wyświetlane w czytelniejszy sposób pod kątem, użyłem bardziej stonowanych kolorów,
#dodałem tytuł oraz oczywiście mój wykres jest interaktwyny.