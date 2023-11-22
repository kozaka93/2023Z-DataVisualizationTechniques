setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych")

df <- read.csv("eating-disorders-prevalence-by-age.csv")

colnames(df) <- c("Entity","Code","Year","5-14", "15-19", "20-24","25-29","30-34","35-39","40-44","45-49","All", "standardized")

#df %>% mutate(Entity = forcats::fct_reorder(df$Entity, df$All)) -> df

df %>% filter(Year == 2019) %>%
  arrange(-All) %>% head(6) %>%
  select(-Code,-Year,-standardized) %>%
  pivot_longer(!Entity, names_to = "age", values_to = "share") -> df_modified

df_modified %>% mutate(Entity = forcats::fct_reorder(df_modified$Entity, df_modified$share)) -> df_modified

df_modified$age <- factor(df_modified$age,levels = rev(c("5-14", "15-19", "20-24","25-29","30-34","35-39","40-44","45-49", "All")))

df_modified %>% ggplot(aes(y = Entity, x=share, fill = age))+
  geom_col(position = 'dodge2',width=1) + scale_fill_manual(values = c("#A8231E","#B4443B", "#BC5A22", "#C67306", "#9C6A0F","#645921","#3D4D2E","#053C41","#032A2E"), guide = guide_legend(reverse = TRUE)) +
  labs(title = "Percent of population in age groups affected by EDs by country", x = "Population percent",
       y = "Country") + theme(plot.background = element_rect(fill = "#D5D4BB")) +
  theme(panel.background = element_rect(fill = "#D5D4BB"))+
  theme(legend.background = element_rect(fill = "#D5D4BB"))+
  theme(panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"),
        text = element_text(size =15),
        axis.text = element_text(size = 15))

df_modified %>% group_by(age) %>% summarise(mean_share = mean(share)) %>%
  ggplot(aes(x = age, y = mean_share, fill = mean_share)) + geom_col() + 
  scale_fill_gradient(low = "#6C857D", high = "#BB8200") +
  coord_flip() + theme(legend.background = element_rect(fill = "#D5D4BB"))
df <- data.frame(x = c(0.1,1.1,1.6), y = c("[0%,1%)", "[1%,1.5%)", ">1.5%"))
ggplot(df,aes(x,y,fill = y)) + geom_col() + scale_fill_manual(values = c("#7B9B49","#7D7423","#C7870B"),guide = guide_legend(reverse = TRUE)) +
  theme(legend.background = element_rect(fill = "#D5D4BB")) +labs(fill = "Mean share")

df_modified %>% ggplot(aes(x = age, y = share, fill = Entity)) +
  geom_col(position = "dodge2") + coord_flip()+
  scale_fill_manual(values = c("#A8231E","#B4443B", "#BC5A22", "#C67306", "#9C6A0F","#645921","#3D4D2E"), guide = guide_legend(reverse = TRUE))

#scale_fill_manual(values = c("red","black", "midnightblue", "blue3","blue", "slateblue4", "mediumpurple4","slateblue","lightslateblue")