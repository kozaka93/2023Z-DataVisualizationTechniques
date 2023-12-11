library(tidyr)
library(plotly)

#link do źródła wykresu:
#https://rolniczeabc.pl/432263,W-skali-roku-ceny-zywca-wieprzowego-znacznie-wzrosly.html

cenyZywnosciLata <- data.frame(
  Produkty = c("pszenica", "jęczmień", "owies", "żywiec wołowy", "żywiec drobiowy",
               "żywiec wieprzowy", "żywiec indyczy", "żyto", "kukurydza", "pszenżyto"),
  
  "01-2015" = c(750, 600, 420, 600, 340, 405, 590, 543, 588, 568),
  "02-2015" = c(730, 625, 475, 625, 345, 440, 588, 530, 593, 575),
  "03-2015" = c(740, 610, 460, 623, 350, 445, 590, 510, 587, 550),
  "04-2015" = c(725, 590, 475, 610, 335, 445, 593, 495, 580, 548),
  "05-2015" = c(685, 555, 425, 618, 340, 430, 585, 468, 575, 520),
  "06-2015" = c(675, 565, 450, 634, 360, 450, 580, 475, 578, 525),
  "07-2015" = c(700, 575, 465, 580, 370, 440, 583, 518, 620, 550),
  "08-2015" = c(655, 580, 455, 580, 378, 435, 584, 500, 665, 545),
  "09-2015" = c(660, 580, 500, 584, 355, 460, 578, 510, 655, 565),
  "10-2015" = c(680, 590, 510, 593, 348, 440, 578, 525, 649, 575),
  "11-2015" = c(690, 610, 540, 608, 338, 380, 584, 538, 670, 585),
  "12-2015" = c(690, 625, 543, 620, 315, 370, 583, 545, 685, 600),
  
  "01-2016" = c(675, 618, 555, 620, 330, 400, 575, 540, 695, 600),
  "02-2016" = c(660, 608, 565, 610, 335, 420, 570, 535, 685, 580),
  "03-2016" = c(650, 595, 535, 613, 340, 425, 550, 525, 670, 578),
  "04-2016" = c(650, 595, 554, 605, 338, 418, 540, 520, 664, 584),
  "05-2016" = c(643, 598, 550, 620, 345, 450, 525, 525, 678, 590),
  "06-2016" = c(647, 604, 560, 630, 348, 495, 518, 530, 700, 595),
  "07-2016" = c(638, 550, 550, 610, 349, 538, 500, 500, 718, 575),
  "08-2016" = c(610, 520, 460, 612, 350, 535, 496, 490, 670, 525),
  "09-2016" = c(625, 525, 475, 614, 340, 540, 488, 500, 624, 540),
  "10-2016" = c(635, 535, 484, 610, 325, 518, 494, 520, 570, 550),
  "11-2016" = c(650, 550, 500, 625, 315, 498, 490, 530, 580, 560),
  "12-2016" = c(660, 578, 520, 648, 305, 508, 480, 540, 615, 578),
  
  "01-2017" = c(680, 620, 550, 648, 315, 488, 484, 550, 648, 615),
  "02-2017" = c(690, 625, 525, 640, 325, 480, 495, 575, 649, 625),
  "03-2017" = c(695, 623, 549, 635, 345, 495, 505, 580, 670, 649),
  
  check.names = FALSE
)

cenyZywnosciPelne <- gather(cenyZywnosciLata, key = "Miesiac", value = "Value", -Produkty)
cenyZywnosciPelne$Miesiac <- as.Date(paste0(cenyZywnosciPelne$Miesiac, "-01"), format = "%m-%Y-%d")

wykres_cen <- plot_ly(cenyZywnosciPelne, x = ~Miesiac, y = ~Value, type = 'scatter', 
                      mode = 'lines+markers', color = ~Produkty) %>%
  layout(title = list(text="Średnie miesięczne ceny skupu podstawowych zbóż", y=0.984, x=0.45),
         xaxis = list(title = "Dany miesiąc", tickmode = 'array', tickvals = cenyZywnosciPelne$Miesiac,
                      ticktext = format(cenyZywnosciPelne$Miesiac, "%m-%Y"), rangeslider = list(visible = TRUE)),
         yaxis = list(title = "Średni koszt"), legend= list(orientation = "v", x = 1, y = 0.8),
         annotations = list(
           list(
             x = 0.525,
             y = 0.985,
             xref = "paper",
             yref = "paper",
             text = "oraz żywca wołowego, wieprzowego i drobiowego w okresie lat 2015-2017",
             showarrow = FALSE,
             font = list(size = 12)
           ),
           list(
             x = 1.084,
             y = 0.825,
             xref = "paper",
             yref = "paper",
             text = "Produkt:",
             showarrow = FALSE,
             font = list(size = 12)
           ) 
         )) %>% add_trace(type="scatter", mode="markers", marker = list(color = "rgba(0, 0, 0, 0)"),
                          text = ~paste("Produkt: ", Produkty, "<br>Koszt: ", 
                                        ifelse(grepl("żywiec", Produkty), 
                                               paste(Value, "zł/100kg"), paste(Value, "zł/1t")),
                                        "<br>Dane z: ", Miesiac ),
                          hoverinfo = "text",showlegend = FALSE)

wykres_cen

