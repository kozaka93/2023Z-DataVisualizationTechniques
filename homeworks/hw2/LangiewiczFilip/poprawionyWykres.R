library(ggplot2)
library(forcats)
library(scales)

df <- data.frame("Jednostka" = c("ICHiP",
                             "DS P-T", 
                             "DS Akademik", 
                             "DS Babilon", 
                             "DS Riviera",
                             "DS Ustronie"),
                 "Procent" = c(37,
                               26,
                               21,
                               21,
                               21,
                               20))

ggplot(df, aes(x = fct_reorder(Jednostka, -Procent), y = Procent)) +
  geom_col(fill = "orange",
           color = "navy") + 
  geom_text(aes(label = paste(Procent, "%", sep = "")), 
            vjust = -0.5) +
  labs(title = "Dotychczasowa frekwencja w niektórych jednostkach Politechniki Warszawskiej",
       x = "Jednostka",
       y = "Frekwencja w procentach") +
  scale_y_continuous(expand = expansion(c(0, 0), c(0, 5)),
                     labels = percent_format(scale = 1)) +
  theme_bw()
  
  
