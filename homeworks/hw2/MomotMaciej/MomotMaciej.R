library(ggplot2)
library(tidyr)

df<-data.frame('2019' = c(93.7,	100.1,	103.9,	99.3,	97.3,	100.6,	101.5,	98.9,	99.2,	102.2,	100.2,	106.3),
            '2020' = c(93.6,	100.3,	102.8,	96.4,	97.2,	102.6,	101.9,	99.3,	100.3,	101.4,	100.4,	108.8),
           '2021' = c(91.6,	100.1,	105.4,	97.1,	96.8,	102.8,	100.5,	99.6,	99.3,	100.2,	100.8,	109.3),
           '2022' = c(89.8, 102.9, 103.8, 97.5, 95.0, 100.9, 100.9, 96.4, 100.1, 98.2, 101.8, 106.7),
           '2023' = c(91.8, 101.3, 105.1, 98.3, 96.6, 102.1, 102.2, 98.3, 100.5,NA,NA,NA)
           
              )
colnames(df) <- c(2019:2023)
df$month <- c(1:12)

library(reshape2)
melted_df <- melt(df, id.vars = "month")
colnames(melted_df) <- c("Miesiąc","Rok","Wartość")
ggplot(melted_df, aes(x=Miesiąc, y=Wartość, color = Rok)) + geom_hline(yintercept = 100, color = "black")+
  geom_line(size = 1.3) + geom_point(size=1.4) + 
  scale_x_continuous(breaks = seq(min(melted_df$Miesiąc), max(melted_df$Miesiąc), by = 1)) +
  scale_y_continuous(limits = c(89, NA), breaks = seq(89, 110)) + 
  scale_color_manual(values = c("#66c2a5","#fc8d62","#8da0cb","#de2d26","#ffeda0"))+
  theme_minimal() 

