library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv('dane.csv')

df %>%
  mutate(Data = as.Date(Data, '%d-%m-%Y')) %>%
  pivot_longer(
    cols = c(Wzrostowy, Boczny, Spadkowy),
    names_to = "Variable",
    values_to = "Value"
  ) -> df

ggplot(df, aes(x = Data, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_date(breaks = df$Data) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     expand = c(0, 0)) +
  labs(title = "Indeks Nastrojów Inwestorów (INI) - pięć ostatnich odczytów",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Wzrostowy" = "#188510",
                               "Spadkowy" = "#cf1f1f",
                               "Boczny" = "#8f8585"),
                    name = "",) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0.5)) +
  geom_text(aes(label = paste0(Value, "%")),
            position = position_dodge(6.6),
            size = 5,
            vjust = -0.4)
  
