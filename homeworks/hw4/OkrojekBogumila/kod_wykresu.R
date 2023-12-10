library(tidyr)
library(dplyr)
library(stringr)
library(plotly)

df<-read.csv("TFR_data.csv",header=T) 


df2 <- pivot_longer(df, 2:9, names_to = "kategoria", values_to = "total_fertility_rate") %>% 
  mutate(Typ = str_split_fixed(kategoria, "_", 2)[,1], 
         Miejsce = str_split_fixed(kategoria, "_", 2)[,2]) %>% 
  mutate(Typ = ifelse(Rok<2013, "Dane rzeczywiste", Typ)) %>% 
  select(-kategoria)

rows <- df2[17:18,] %>% 
  mutate(Typ = "Dane rzeczywiste")

df3 <- df2 %>% 
  rbind(df2, rows)

colors = c("#3cb44b","#000000","#42d4f4","#ffe119","#e6194B" )

df4 <- df3 %>% 
  group_by(Rok,Miejsce,Typ) %>% 
  summarise(total_fertility_rate = mean(total_fertility_rate)) %>% 
  pivot_wider(names_from = Miejsce, values_from = total_fertility_rate) %>% 
  mutate(mean_total_fertility_rate = (wies+miasta)/2) %>% 
  mutate(Typ = ifelse(Typ == "Bardzowysoki", "Bardzo wysoki", Typ)) %>% 
  ungroup()

fig <- plot_ly(df4, x = ~Rok)

fig <- fig %>% add_trace(y = ~mean_total_fertility_rate, color = ~Typ, 
                         colors = colors, visible = T, 
                         type = "scatter",
                         mode = "lines",
                         name = ~Typ,
                         line = list(width = 5)
                        )

fig <- fig %>% add_trace(y = ~wies, color = ~Typ, 
                         colors = colors, visible = F, 
                         type = "scatter",
                         mode = "lines",
                         name = ~Typ,
                         line = list(width = 5)
                         )

fig <- fig %>% add_trace(y = ~miasta, color = ~Typ, 
                         colors = colors, visible = F, 
                         type = "scatter",
                         mode = "lines",
                         name = ~Typ,
                         line = list(width = 5)
                         )

fig <- fig %>% layout(
  title = list(text = "Prognozowany współczynnik dzietności (TFR) w miastach i na wsi w latach 2014-2070", y=1.8,
               yanchor = "top", x = 0.5, font = list(size = 30), yref='container'),
  
  xaxis = list(title = "Rok",showgrid = FALSE,showline= T, linewidth=2, linecolor='black', 
               titlefont = list(size = 20), tickfont = list(size = 15)),
  
  yaxis = list(range = c(1, 2), title = "Współczynnik dzietności (TFR)", 
               tickvals = list(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9,2),
               gridcolor = "gray", showline= T, linewidth=2, linecolor='black', 
               titlefont = list(size = 20), tickfont = list(size = 15)),
  
  hovermode = "x unified",
  
  autosize = T,
  
  margin=list( l = 50, r = 50, b = 170, t = 100,  pad = 5),
  
  legend = list(
    orientation = "h",
    xanchor = "center",
    x = 0.5,
    yanchor = "bottom",
    y = -0.3,
    font = list(size = 15),
    title = list(text = "Wariant:", font = list(size = 20)), 
    pad = list('r'= 0, 't'= 40, 'b' = 10),
    bgcolor = "#e6e4e7"
  ),
  
  updatemenus = list(
    list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      pad = list('r'= 0, 't'= 10, 'b' = 50),
      x = 0.5,
      y = 1,
      font = list(size = 20),
      buttons = list(
        list(method = "restyle",
             args = list("visible", 
                         list(TRUE, TRUE, TRUE, TRUE, TRUE, 
                              FALSE, FALSE, FALSE, FALSE, FALSE, 
                              FALSE, FALSE, FALSE, FALSE, FALSE)),
             label = "W Polsce"),
        
        list(method = "restyle",
             args = list("visible", 
                         list(FALSE, FALSE, FALSE, FALSE, FALSE, 
                              FALSE, FALSE, FALSE, FALSE, FALSE, 
                              TRUE, TRUE, TRUE, TRUE, TRUE)),
             label = "Na wsi"),
        
        list(method = "restyle",
             args = list("visible", 
                         list(FALSE, FALSE, FALSE, FALSE, FALSE, 
                              TRUE, TRUE, TRUE, TRUE, TRUE, 
                              FALSE, FALSE, FALSE, FALSE, FALSE)),
             label = "W miastach")))
  )
)
fig


