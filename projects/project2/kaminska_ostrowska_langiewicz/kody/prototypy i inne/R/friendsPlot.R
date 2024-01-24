library(dplyr)
library(ggplot2)
library(plotly)

friendsPlot_data_f <- read.csv("../app/KomunikacJA/appData/friendsPlot/friends_mg_f.csv") %>% 
  mutate(date = as.Date.character(strDate, tryFormats = "%d-%m-%Y")) %>% 
  select(person, app, date)
friendsPlot_data_z <- read.csv("../app/KomunikacJA/appData/friendsPlot/friends_mg_z.csv") %>% 
  mutate(date = as.Date.character(strDate, tryFormats = "%d-%m-%Y")) %>% 
  select(person, app, date)
friendsPlot_data_a <- read.csv("../app/KomunikacJA/appData/friendsPlot/friends_mg_a.csv") %>% 
  mutate(date = as.Date.character(strDate, tryFormats = "%d-%m-%Y")) %>% 
  select(person, app, date)

friendsPlot_data <- bind_rows(friendsPlot_data_f, friendsPlot_data_a, friendsPlot_data_z) %>% 
  select(person, date)

write.csv(friendsPlot_data, "../app/KomunikacJA/appData/friendsPlot/friendsData.csv", row.names = FALSE)

# ggplotly(
#   friendsPlot_data %>% 
#     group_by(person, date) %>% 
#     summarise(liczba_znajomych = n()) %>% 
#     mutate(sumaryczna_liczba_znajomych = cumsum(liczba_znajomych)) %>% 
#     ggplot(aes(x = date, y = sumaryczna_liczba_znajomych, color = person)) +
#     geom_line() +
#     theme_minimal()
# )

friendsPlot_data %>%
  group_by(person, date) %>%
  summarise(liczba_znajomych = n()) %>%
  mutate(sumaryczna_liczba_znajomych = cumsum(liczba_znajomych)) %>%
  plot_ly(x = ~date, y = ~sumaryczna_liczba_znajomych, color = ~person, type = "scatter", mode = "lines") %>%
  layout(title = "Sumaryczna liczba znajomych w czasie",
         xaxis = list(title = "Data"),
         yaxis = list(title = "Sumaryczna liczba znajomych"),
         showlegend = TRUE) 
