library(ggplot2)

# Create a data frame from the provided data
savings_data <- data.frame(
  Savings_Range = factor(c('Do 1 000 zł', '1 001 – 3 000 zł', '3 001 – 5 000 zł', 
                           '5 001 – 10 000 zł', '10 001 – 30 000 zł', '30 001 – 50 000 zł', 
                           '50 001 – 100 000 zł', 'Powyżej 100 000 zł'), 
                         levels = c('Powyżej 100 000 zł', '50 001 – 100 000 zł', '30 001 – 50 000 zł', 
                                    '10 001 – 30 000 zł', '5 001 – 10 000 zł', '3 001 – 5 000 zł', 
                                    '1 001 – 3 000 zł', 'Do 1 000 zł')),
  Percentage = c(13, 13, 13, 15, 14, 10, 11, 11) / 100
)

get_color <- function(val) {
  ifelse(val == .15, "#58D68D", ifelse(val == .10, "#F08080", "gray"))
}
colors = get_color(savings_data$Percentage)

# Create the barplot using ggplot2
p <- ggplot(savings_data, aes(x = Savings_Range, y = Percentage, fill=colors)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Savings Distribution of Polish Population",
       x = "Savings Range (PLN)",
       y = "Percentage of Population") +
  theme_minimal() +
  scale_fill_manual(values = c("#58D68D", "#F08080", "gray")) + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        legend.position = "none")

ggsave(filename = "fixed_chart.png", plot = p, width = 6, height = 4, dpi = 300)
