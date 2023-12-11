library(plotly)

# Źródło poprawianej wizualizacji: 
# https://thefederalist.com/2015/09/30/at-planned-parenthood-abortion-is-up-health-care-is-down/

# Powyższa wizualizacja jest myląca, ponieważ przedstawia dwie krzyżujące się linie -
# jedna pokazuje spadek liczby badań w kierunku nowotworu i usług profilaktycznych
# w roku 2013 w stosunku do roku 2006,
# a druga - wzrost liczby aborcji dla tych samych lat.
# Na wykresie brakuje osi Y. Co więcej, zostały na nim użyte dwie różne skale dla
# porównywalnych wielkości - liczby usług świadczonych przez organizację Planned 
# Parenthood - bez podania informacji o tym. Daje to mylne wrażenie, że liczba
# aborcji wzrosła w podobnym stopniu, w jakim zmalała liczba badań i że w 2013 r.
# liczba aborcji przewyższyła liczbę wykonanych badań, co jest nieprawdą.

# Dane pochodzą z corocznych raportów organizacji Planned Parenthood
Year <- seq(2006, 2013, by = 1)
Cancer.scr <- c(2007371, 1900850, 1849691, 1830811, 1596741, 1307570, 1121580, 935573)
Abortions <- c(289750, 305310, 324008, 331796, 329445, 333964, 327166, 327653)

data <- data.frame(Year, Cancer.scr, Abortions)

plot <- plot_ly(
  data, 
  x = ~Year, 
  y = ~Cancer.scr, 
  name = 'Cancer screenings and prevention services',
  type = 'scatter',
  mode = 'lines+markers',
  hovertemplate = '<b>%{y}</b>'
  ) %>% 
  add_trace(
    y = ~Abortions, 
    name = 'Abortions',
    type = 'scatter',
    mode = 'lines+markers',
    hovertemplate = '<b>%{y}</b>'
  ) %>% 
  layout(
    title = "<b>Planned Parenthood Federation of America: Abortions vs. Cancer and 
    prevention services</b>",
    xaxis = list(showgrid = FALSE),
    yaxis = list(title = "Number of services performed",
                 range = c(0, 2250000),
                 tickvals = c(0, 500000, 1000000, 1500000, 2000000),
                 tickformat = ',.0f'),
    legend = list(
      y = 0.5
    ),
    margin = list(t=50, b=70, l=80, r=80),
    hovermode = "x unified"
  )
plot

# Utworzony wykres jest lepszy od poprzedniego, ponieważ zawiera jedną oś Y 
# z podpisem i ujednoliconą skalą dla obu przedstawianych wielkości. Uwzględnione
# są na nim również wartości dla poszególnych lat z zakresu 2006-2013, a nie tylko
# dla lat granicznych, jak było na poprzednim wykresie. Daje to pełniejszy obraz
# sytuacji. Ponadto, aktualny wykres nie zawiera strzałek, które na poprzednim 
# wykresie sugerowały silną kontynuację pokazanych trendów, o których nie było 
# wiadomo, czy są prawdziwe.