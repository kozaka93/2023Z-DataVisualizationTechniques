library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv("DaneHW2.csv")
colnames(df) <- c("Partia", "2011", "2015", "2019", "2023")


df %>% 
  pivot_longer(cols = -Partia, names_to = "Rok", values_to = "Procent_głosów") %>% 
  arrange(Rok, desc(Procent_głosów)) -> data


ggplot(data, aes(x = Rok, y = Procent_głosów, fill = Partia)) +
  geom_col(position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("darkorchid3", "red2", "royalblue2", "gold2", "springgreen3", "gray44")) +
  labs(title = "Wykres - transformacja sceny politycznej na przestrzeni lat w okręgu nr 19",
       x = "",
       y = "Poparcie",
       fill = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 51),
                     breaks = seq(0, 50, 10),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5)) +
  geom_text(aes(label = paste0(Procent_głosów, "%")),
            position = position_dodge(0.9),
            vjust = -0.4,
            size = 3.5) +
  geom_vline(aes(xintercept = lag(as.numeric(as.factor(Rok)), default = 0) + 0.5), color = "gray19", linetype = "dashed")
  
  
