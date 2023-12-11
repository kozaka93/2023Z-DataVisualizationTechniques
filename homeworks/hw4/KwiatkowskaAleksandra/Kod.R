Category <- c("Influenza A", "Influenza B", "SARS-CoV-2", "Rhinovirus", "RSV", "Other", "Confirmed ARI", "Reported ARI")

Weeks <- 14:39

values_list <- list(
  "14" = c(2, 1, 5, 22, 6, 12, 65, 160),
  "15" = c(4, 3, 6, 20, 8, 13, 72, 95),
  "16" = c(3, 2, 5, 18, 7, 17, 53, 90),
  "17" = c(4, 0, 10, 19, 3, 16, 54, 88),
  "18" = c(7, 5, 9, 22, 10, 16, 60, 95),
  "19" = c(2, 4, 10, 40, 5, 19, 73, 120),
  "20" = c(8, 5, 15, 43, 7, 15, 70, 130),
  "21" = c(9, 3, 17, 40, 20, 22, 74, 125),
  "22" = c(2, 4, 21, 60, 18, 18, 70, 129),
  "23" = c(2, 1, 10, 55, 15, 20, 70, 124),
  "24" = c(8, 5, 20, 65, 40, 22, 75, 130),
  "25" = c(20, 2, 10, 50, 25, 25, 72, 135),
  "26" = c(22, 3, 7, 40, 20, 23, 60, 132),
  "27" = c(24, 3, 7, 38, 21, 24, 65, 140),
  "28" = c(10, 15, 9, 30, 18, 22, 66, 120),
  "29" = c(7, 5, 10, 28, 16, 22, 62, 100),
  "30" = c(8, 4, 7, 34, 16, 21, 80, 110),
  "31" = c(2, 3, 8, 50, 15, 27, 83, 140),
  "32" = c(9, 0, 15, 45, 17, 35, 87, 141),
  "33" = c(12, 0, 20, 30, 10, 32, 85, 130),
  "34" = c(10, 0, 19, 30, 10, 32, 82, 128),
  "35" = c(10, 4, 15, 32, 6, 40, 78, 110),
  "36" = c(10, 2, 14, 33, 4, 30, 75, 115),
  "37" = c(15, 0, 22, 35, 4, 36, 70, 116),
  "38" = c(14, 2, 21, 42, 2, 37, 75, 113),
  "39" = c(13, 0, 19, 40, 5, 38, 82, 121)
)

data <- data.frame(
  Week = rep(Weeks, each = length(Category)),
  Category = rep(Category, times = length(Weeks)),
  Value = unlist(values_list)
)

data_summarized <- data %>%
  filter(Category != "Reported ARI" & Category != "Confirmed ARI") %>% 
  group_by(Week) %>%
  summarize(Value = sum(Value)) %>%
  mutate(Category = "Total")

data <- bind_rows(data, data_summarized)


library(plotly)
library(dplyr)

data_1 <- data %>% 
  filter(Category != "Reported ARI" & Category != "Confirmed ARI")
  

fig1 <- plot_ly(
  data = data_1,
  x = ~Week,
  y = ~Value,
  color = ~Category,
  type = "bar",
  text = paste("Week: ", data_1$Week, "<br>Category: ", data_1$Category, "<br>Count: ", data_1$Value),
  hoverinfo = 'text'
) %>%
  layout(
    barmode = "group",
    title = "Weekly ILI and Virus positive cases among WellKiwis participants since April 2023",
    xaxis = list(title = "Week 2023 season", type = "category", tickangle = 0),
    yaxis = list(title = "Count of Viruses"),
    legend = list(x = 0.9, y = 0.5)
  )
  

data_2 <- data %>% 
  filter(Category == "Reported ARI" | Category == "Confirmed ARI") 

fig2 <- plot_ly(
  data = data_2,
  x = ~Week,
  y = ~Value,
  color = ~Category,
  colors = c("royalblue", "orange"),
  type = "scatter",
  mode = "lines+markers",
  text = paste("Week: ", data_2$Week, "<br>Count: ", data_2$Value),
  hoverinfo = 'text'
) %>%
  layout(
    xaxis = list(title = "Week 2023 season", type = "category", tickangle = 0),
    yaxis = list(title = "Rate ARI per per 1000 participants", range = c(0, 160)),
    legend = list(x = 0.9, y = 1)
  )

combined_fig <- subplot(fig2, fig1, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.05)


combined_fig

