library(ggplot2)
options(scipen = 12)

country <- c("France", "Germany")
value <- c(358117, 414758)
txt <- c("23% of the EU total", "27% of the EU total")
df <- data.frame(country, value, txt)


ggplot(df, aes(country, value)) + 
  geom_col(fill = "#b09120") +
  geom_text(aes(label = txt),
                       angle = 0,
                       vjust = -0.5) +
  labs(title = element_text("Main importing EU members"),
       subtitle= "2022",
       y = "tonnes", x = element_blank())

                    