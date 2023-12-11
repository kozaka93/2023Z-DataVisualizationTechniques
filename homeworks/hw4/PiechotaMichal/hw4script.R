library(plotly)
library(dplyr)

data <- data.frame(Country = c("United Kingdom", "United States", "Switzerland", "Norway", "Other"),
                   paper = c(47, 20, 14, 12, 7),
                   plastic = c(45, 8, 7, 5, 35),
                   glass = c(47, 14, 9, 8, 22))

data$Country <- factor(data$Country, levels = rev(c("United Kingdom", "United States", "Switzerland", "Norway", "Other")))


fig <- plot_ly(data, y = ~Country, x = ~paper, type = "bar", name = "Paper", 
               hovertext = ~paste(Country, ": ", paper, "%", sep=""),
               marker = list(color = '#C3B299')) %>%
  add_trace(x = ~plastic, name = "Plastic", hovertext = ~paste(Country, ": ", plastic, "%", sep=""),
            marker = list(color = '#815355')) %>%
  add_trace(x = ~glass, name = "Glass", hovertext = ~paste(Country, ": ", glass, "%", sep=""),
            marker = list(color = '#523249')) %>%
  layout(xaxis = list(title = "Percentage in import of recyclables"), barmode = "group",
         title = list(text = "Extra-EU trade partners for imports of recyclables, 2022",
                      y = 0.98),
         legend = list(title = list(text = "Material Type")),
         annotations = list(
           x = 1,
           y = 0,
           xref = 'paper',
           yref = 'paper',
           text = 'Source: https://europa.eu/!Txdtdw',
           showarrow = FALSE,
           xanchor = 'right',
           yanchor = 'bottom'
         ))

fig