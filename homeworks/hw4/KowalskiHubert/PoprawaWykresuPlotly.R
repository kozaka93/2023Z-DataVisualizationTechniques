library(dplyr)
library(plotly)
library(wesanderson)

# Prepare the data
government <- c(1099.1, 1207.7)
financing <- c(125.1, 127.6)
subs <- c(131.2, 127.2)
digital <- c(36.6, 42.6)
television <- c(264.0, 275.7)
years <- c("2016-2017", "2017-2018")
sum <- government + financing + subs + digital + television

governmentF <- paste0("<b>$", government, "M</b>")
financingF <- paste0("<b>$", financing, "M</b>")
subsF <- paste0("<b>$", subs, "M</b>")
digitalF <- paste0("<b>$", digital, "M</b>")
televisionF <- paste0("<b>$", television, "M</b>")
sumF <- paste0("<b>$", sum, "M</b>")

colnames <- c("Government funding", "Financing and other income", "Subscriber fees", "Digital advertising", "Television advertising", "Year")

df <- data.frame(government, financing, subs, digital, television, years)
df <- df %>% 
  mutate(years = factor(years, levels = c("2016-2017", "2017-2018")))
colnames(df) <- colnames

m <- list(
  l = 100,
  r = 100,
  b = 100,
  t = 100
)

# Labels for hovertext
labels <- c("Government funding", "Financing and other income", "Subscriber fees", "Digital advertising", "Television advertising")
categories <- c("Government", "Self-generated revenue", "Self-generated revenue", "Advertising", "Advertising")
full_labels <- paste0("<b>Source: </b>", labels, "<br><b>Category: </b>", categories)

# Plot
plot_ly(df, x = ~years, y = ~television, type = 'bar', name = 'Television advertising', text = televisionF, textfont = t, textposition = 'auto', 
        hoverinfo = 'text', hovertext = paste0(full_labels[5], "<br><b>Value: </b>", televisionF ), 
        marker = list(color = "#0066CC", line = list(color = 'rgb(8,48,107)', width = 3)) )  %>%
  add_trace(y = ~digital, name = 'Digital advertising', text = digitalF, hoverinfo = 'text', hovertext = paste0(full_labels[4], "<br><b>Value: </b>", digitalF ),
            marker = list(color =  "#A00080")) %>%
  add_trace(y = ~subs, name = 'Subscriber fees', text = subsF,hoverinfo = 'text', hovertext = paste0(full_labels[3], "<br><b>Value: </b>", subsF),
            marker = list(color =  "#F37295")) %>%
  add_trace(y = ~financing, name = 'Financing and other income', text = financingF, hoverinfo = 'text', hovertext = paste0(full_labels[2], "<br><b>Value: </b>", financingF ), 
            marker = list(color =  "cornsilk")) %>%
  add_trace(y = ~government, name = 'Government funding', text = governmentF, hoverinfo = 'text', hovertext = paste0(full_labels[1], "<br><b>Value: </b>", governmentF ),
            marker = list(color =  "#CD2626")) %>%
  add_annotations(x = ~years, y = ~sum+50, text = paste0("Total revenue: ",sumF), showarrow = FALSE, font = list(size = 16)) %>%
  layout(title = '<b>Revenue and other sources of funds for the <i>Canadian National Broadcasting Company (CBC)</i></b>',
         font = list(size = 16),
         xaxis = list(title = 'Year'),
         yaxis = list(title = list(text = "Value (in dollars) for each source of funds", standoff = 10), gridcolor = "darkgrey"),
         barmode = 'stack',
         margin = m,
         legend = list(x = 100, y = 0.5, title = list(text="<b>Type of source</b>") ),
         bargap = 0.6,
         hoverlabel = list(font = list(size = 16))
         ) %>%
  config(displayModeBar = FALSE)




