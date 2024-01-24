library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)


heatMap_data <- read.csv("../app/KomunikacJA/appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))
 


# pierwsza próba 
#
# ggplotly(
#   df %>% 
#     filter(year == "2023") %>% 
#     filter(app == "sp") %>% 
#     filter(person == "f") %>% 
#     group_by(month, day) %>% 
#     summarise(liczba_wiadomosci = n()) %>% 
#     arrange(-liczba_wiadomosci) %>% 
#     ggplot(aes(x = day, y = month, fill = liczba_wiadomosci)) +
#     geom_tile() +
#     theme_minimal() +
#     theme(panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_line(color = "black"))
# )


heatMap_data <- dff %>%
  mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day))) %>% 
  mutate(liczba = 1)

ggplotly(
  heatMap_data %>% 
    filter(person == "f") %>% 
    filter(app == "mg") %>% 
    right_join(data.frame(date = seq((min(heatMap_data %>% 
                                           filter(person == "f",
                                                  app %in% "mg") %>% 
                                           .$date)),
                                     as.Date("2023-12-31"),
                                     by = "day")), 
               by = "date") %>% 
    filter(year(date) == 2023) %>% 
    group_by(date) %>% 
    summarise(liczba_wiadomosci = sum(liczba, na.rm = TRUE)) %>% 
    ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci)) +
    geom_tile() +
    # scale_fill_gradient(high = "darkgreen",
    #                   low = "lightgreen") +
    scale_y_continuous(limits = c(12.5, 0.5),
                       breaks = 1:12,
                       labels = month.name,
                       trans = "reverse",
                       expand = expansion(c(0, 0), c(0.3, 0))) +
    scale_x_continuous(limits = c(0.5, 31.5),
                       breaks = 1:31,
                       expand = expansion(c(0, 0), c(0.5, 0))) +
    labs(title = paste("Heatmap for year"),
         x = "Day of Month",
         y = "Month") +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank()) +
    geom_hline(yintercept = 0.5:12.5,
               linewidth = 0.3) +
    geom_vline(xintercept = 0.5:31.5,
               linewidth = 0.3)
) %>% 
  layout(
    xaxis = list(fixedrange = TRUE), 
    yaxis = list(fixedrange = TRUE)) -> p
  p
  p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
  p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'
  p

  
  
  colorScale <- data.frame(c(0,0.5,0.5,1), c("red","red","#FDE624","#FDE624"))
  

  p[["x"]][["data"]][[1]][["colorscale"]] = colorScale
  names(p[["x"]][["data"]][[1]][["colorscale"]]) = NULL
  p[["x"]][["data"]][[4]][["marker"]][["colorscale"]] = colorScale
  names(p[["x"]][["data"]][[4]][["marker"]][["colorscale"]]) = NULL
  
  p
  






