library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych")

df<- read_excel("dataset_table.xlsx")
colnames(df)<-c("citation","case_name","years","ages","sex","female_perc","prevalence","se","sample_size","recall",
                "case_definition","location")
View(df)
df <- df %>% fill(citation, .direction = "down")%>% 
  unique() %>% 
  filter(case_name!="OSFED + BED") %>% 
  mutate(case_name=case_when(
    case_name %in% c("Bulimia","Bulimia Nerviosa") ~ "Bulimia Nervosa",
    case_name %in% c("Anorexia","Anorexia Nerviosa") ~ "Anorexia Nervosa",
    TRUE ~ case_name ),
  female=as.numeric(female_perc)*sample_size,
  male=(100-as.numeric(female_perc))*sample_size)

df_summary <- df %>%
  group_by(case_name) %>%
  summarise(mean_prevalence = mean(as.numeric(prevalence), na.rm = TRUE),
            se = mean(as.numeric(se), na.rm = TRUE),)

ggplot(df_summary, aes(x = case_name, y = mean_prevalence)) +
  geom_bar(width = 0.7,stat = "identity", position = position_dodge(), fill = "#c7870b") +
  geom_errorbar(aes(ymin = mean_prevalence - se, ymax = mean_prevalence + se), 
                position = position_dodge(0.9), width = 0.4) +
  theme_minimal() +
  labs(x = "Disease", y = "Mean prevalence per 100,000") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
        plot.background = element_rect(fill="#d5d4bb"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_line(color="grey"),
        text = element_text(size = 18),
        axis.text.y = element_text(size = 15))

#-------------------------------------------------------------------------
#stosunek zachorowań ze względu na płeć

df<-read.csv("eating-disorders-prevalence-males-vs-females.csv")
colnames(df)<-c("entity","code","year","male","female","population","continet")

df %>% 
  group_by(entity) %>% 
  filter(year=="2019") %>% 
  na.omit() %>% 
  mutate(femaleCount = female*population,
         maleCount = male*population,
         female_to_male=femaleCount/maleCount)->df_modified
mean(df_modified$female_to_male) #~2.09
