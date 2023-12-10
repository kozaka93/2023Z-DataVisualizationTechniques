library(plotly)

df <- read.csv("dane.csv")

df$discount <- df$discount / 100

plot_ly(data = df,
        x = ~category,
        y = ~discount,
        color = ~factor(year),
        colors = c('#03045E', '#006399', '#48CAE4'),
        type = 'bar') %>%
  layout(title = "Average Black Friday discounts for each year",
         titlefont = list(size = 25),
         xaxis = list(title = "Category", tickfont = list(size = 16)),
         yaxis = list(title = "Discount", tickfont = list(size = 16),
                      tickformat = ".0%", range = c(0, 0.35)),
         font = list(size = 14),
         margin = list(l = 50, r = 50, b = 20, t = 50),
         legend = list(title = list(text = "Year"), bgcolor = "transparent"),
         barmode = 'group')
