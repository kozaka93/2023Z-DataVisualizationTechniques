# loading libraries
library(tidyr)
library(plotly)

# creating data frames
df <- data.frame(
  Producer = c("Swisspor", "Austrotherm", "Termo Organika"),
  "11-2016" = c(182, 194, 188),
  "12-2016" = c(179, 194, 188),
  "01-2017" = c(199, 209, 209),
  "02-2017" = c(199, 213, 221),
  "03-2017" = c(215, 220, 221),
  "04-2017" = c(215, 220, 221),
  "05-2017" = c(198, 205, 210),
  "06-2017" = c(184, 196, 202),
  "07-2017" = c(184, 196, 190),
  "08-2017" = c(188, 200, 194),
  "09-2017" = c(205, 211, 216),
  check.names = FALSE
)

df_pred <- data.frame(
  Producer = c("Swisspor", "Austrotherm", "Termo Organika"),
  "09-2017" = c(205, 211, 216),
  "10-2017" = c(215, 219, 220),
  check.names = FALSE
)


date_labels <- names(df)[-1]
df_long <- gather(df, key = "Date", value = "Price", -Producer)
df_long$Date <- factor(df_long$Date, levels = date_labels)

date_labels_pred <- names(df_pred)[-1]
df_pred_long <- gather(df_pred, key = "Date", value = "Price", -Producer)
df_pred_long$Date <- factor(df_pred_long$Date, levels = date_labels_pred)


# annotations
legend_anno <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.02,
  y = 206,
  text = "Przerywaną linią zaznaczono\nprognozowaną cenę",
  font = list(size = 10),
  showarrow = F) 

subtitle <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.8,
  y = 222,
  text = "Źródło: www.hurtowniastyropianu.pl",
  font = list(size = 12),
  showarrow = F)

range_slider_x <- list(
  x = 0.03,
  y = -0.2,
  xanchor = "left",
  yanchor = "top",
  bgcolor = '#e2e2e2',
  bordercolor = '#ffffff',
  borderwidth = 2,
  thickness = 0.05,
  len = 0.75,
  pad = 10
)

# creating the plot
plot_ly(df_long, x = ~Date, y = ~Price, color = ~Producer,
        type = "scatter", mode = "lines+markers", line = list(width = 2.5),
        colors = c("#bc4749", "#386641", "#0077b6"),
        hoverinfo ="text", text = ~paste("</br><b>Producent:</b>", 
                                         Producer, "</br><b>Miesiąc:</b> ",
                                         Date, "</br><b>Cena:</b> ", 
                                         Price, "zł/m^3")) %>%
  add_trace(data = df_pred_long,
            x = ~Date, y = ~Price, color = ~Producer,
            type = "scatter", mode = "lines", line = list(dash = "dash"),
            showlegend = F) %>% 
  layout(title = list(text = "Zmiana cen styropianu w latach 2016 - 2017",
                      y = 0.98, x = 0.5, xanchor = "center", yanchor = "top",
                      font = list(size = 24)),
         xaxis = list(title = list(text = "Miesiąc", font = list(size = 14))),
         yaxis = list(title = list(text = "Cena [zł/m^3]", font = list(17)),
                      range = c(175, 225)),
         legend = list(
           x = 0.02, y = 0.9, 
           title = list(text = "Producent"),
           font = list(size = 12),
           bgcolor = "transparent"
         ),
         margin = list(b = 100),
         height = 600,
         width = 900,
         annotations = legend_anno
  ) %>% layout(
    xaxis = list(
      rangeslider = list(visible = T)
    )
  ) %>% layout(
    annotations = subtitle
)








