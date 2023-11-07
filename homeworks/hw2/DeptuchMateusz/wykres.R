library("ggplot2")
DF<- data.frame(
  inflacja_w_procentach = c(25.9, 26.7,26.6, 27.6, 28.5, 28.9, 28.9, 28.7, 28.8, 28.1, 27.8, 26.8, 25.6),
  data = c("X 2022", "XI 2022","XII 2022", "I 2023", "II 2023", "III 2023", "IV 2023", "V 2023", "VI 2023", "VII 2023", "VIII 2023", "IX 2023", "X 2023")
)

DF$data <- factor(DF$data, levels = DF$data)

ggplot(DF, aes(x = data,
                 y = inflacja_w_procentach,
               group = 1)) + 
  geom_line(color = "navy") + 
  labs(title = "2-letnia inflacja CPI w Polsce (skumulowana)", 
       x = "Miesiąc i rok",
       y = "Inflacja CPI skumulowana (%)") + 
  coord_cartesian(ylim = c(24,30)) + 
  geom_label(aes(label = inflacja_w_procentach), vjust =1.3) + 
  geom_point()

