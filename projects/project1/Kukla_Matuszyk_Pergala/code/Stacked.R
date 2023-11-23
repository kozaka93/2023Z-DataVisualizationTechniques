#https://www.kaggle.com/code/alexteboul/diabetes-health-indicators-dataset-notebook
library(dplyr)
library(ggplot2)
library(tidyr)


df <- read.csv("../data/diabetes_012_health_indicators_BRFSS2015.csv")

numeric_columns <- sapply(df, function(x) all(is.numeric(x))) # Usuwam nie numeryczne kolumne
numeric_column_names <- names(numeric_columns[numeric_columns])
df <- df[, numeric_column_names]

barplot_data <- data.frame()

for (col in numeric_column_names) {
  all_patients <- length(df[[col]])
  with_diabetes <- length(which(df[[col]] %in% c(1, 2)))
  without_diabetes <- all_patients - with_diabetes
  all_patients_scaled <- all_patients / sum(all_patients) * 100 # przeskalowywuje
  with_diabetes_scaled <- with_diabetes / sum(all_patients)* 100
  without_diabetes_scaled <- without_diabetes / sum(all_patients)* 100
  
  condition_data <- data.frame(
    Condition = factor(col),
    Group = factor(c("With Diabetes", "Without Diabetes")),
    Count = c(with_diabetes_scaled, without_diabetes_scaled)
  )
  barplot_data <- rbind(barplot_data, condition_data)
}

barplot_data <- barplot_data %>%
  filter(!Condition %in% c("Age", "Sex", "Diabetes_012", "PhysActivity", "DiffWalk", "Education", "BMI", "AnyHealthcar", "GenHlth", "Income"),) # wybieram najciekawsze kolumny
barplot_data <- barplot_data %>%
  mutate(Condition = case_when( #zmieniam nazwy kolumn
    Condition == "CholCheck" ~ "Chol checked within 5 years",
    Condition == "Veggies" ~ "Vegetable > 1 a day",
    Condition == "Smoking" ~ "100 cigarettes in your entire life",
    Condition == "HighChol" ~ "High Cholesterol",
    Condition == "PhysHlth" ~ "In good physical health",
    Condition == "MentHlth" ~ "In good mental health",
    Condition == "NoDocbcCost" ~ "No doctor because of cost",
    Condition == "Stroke" ~ "Had a stroke",
    Condition == "Fruits" ~ "Eats > 1 fruit a day",
    Condition == "HeartDiseaseorAttack" ~ "Heart problems",
    Condition == "HvyAlcoholConsump" ~ "Heavy alcohol consumption",
    Condition == "AnyHealthcare" ~ "Has free access to basic healthcare",
    Condition == "HighBP" ~ "High blood pressure",
    TRUE ~ Condition  # Keep other conditions unchanged
  ))


barplot_data_order <- barplot_data %>% filter(Group == "With Diabetes") %>% arrange(desc(Count)) %>% mutate(order = row_number()) %>% select(Condition, order)
barplot_data <- left_join(barplot_data, barplot_data_order, by = "Condition")
barplot_data

condition_levels <- barplot_data_order$Condition
barplot_data$Condition <- factor(barplot_data$Condition, levels = condition_levels)

colors <- c("white", "#b802f0")
stacked_barplot <- ggplot(barplot_data %>% arrange(desc(order)), aes(x = Condition, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") + 
  ylab("Percent") +
  theme_minimal() +
  scale_fill_manual(values = colors) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10, color = "white"),
    axis.text.y = element_text(color = "white", size = 15),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(color = "white"),
    plot.margin = margin(b = 50, l = 200)
  )
stacked_barplot
ggsave("c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/projekt_1/stacked2.svg", plot = stacked_barplot, width = 30, height = 10, units = "cm")

