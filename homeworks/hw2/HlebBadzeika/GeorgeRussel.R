
library(dplyr)
library(ggplot2)





# Prepare the dataset 'russells_data' with necessary transformations and summaries
russells_data <- driver_standings %>%
  filter(driverId == 847 & positionText !="R") %>%
  mutate(  
         Position = as.numeric(position)) %>% 
  group_by(constructorId, Position) %>%     
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Position) %>%
  left_join(constructors, by = "constructorId")

# Plot the data with ggplot2
ggplot(russells_data, aes(x = Count, y = as.factor(Position), fill = name)) +
  geom_bar(stat = "identity") +
 theme_minimal()+
  labs(title = "George Russell's F1 Finish Positions by Team",
       y = "Position",
       x = "Count",
       fill = "Team") +
    
  scale_fill_manual(values=c("black", "blue")) + scale_x_continuous(breaks = c(0:10))