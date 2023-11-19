library(ggplot2)

df <- data.frame(
  time_online = c(418, 418, 413, 409, 397, 396, 395, 400, 401),
  quarter = c("Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022", "Q2 2022", 
              "Q3 2022", "Q4 2022", "Q1 2023", "Q2 2023"))

ggplot(df,aes(x = quarter, y = time_online, group = 1)) +
  geom_line(color = "#72AFF2", linewidth = 1.5) +
  geom_point(color = "black", size = 4) +
  geom_text(aes(label = time_online), vjust = -2, size = 4, fontface = "bold") +
  
  labs(title = "DAILY TIME SPENT USING THE INTERNET",
       subtitle = "the average amount of time (in minutes) that internet users aged 16 to 64 spend using the internet each day",
       x = "quarter",
       y = "time online (minutes)") +
  
  ylim(380, 430) +
  theme_minimal() +
  scale_x_discrete(limits = df$quarter) +  
  
  theme(axis.text.x = element_text(size = 11),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color = "#72AFF2"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
  

