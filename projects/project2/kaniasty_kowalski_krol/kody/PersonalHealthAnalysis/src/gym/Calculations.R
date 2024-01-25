library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)


gym_calculations <- function(filtered_data) {
  
  number_of_workouts <- n_distinct(filtered_data$start_time)
  
  filtered_data_distinct <- filtered_data %>%
    distinct(start_time, .keep_all = TRUE)
  

  total_duration <- sum(difftime(filtered_data_distinct$end_time, 
                                 filtered_data_distinct$start_time, 
                                 units = "hours"), 
                        na.rm = TRUE)  
  total_volume <- sum(filtered_data$weight_kg * filtered_data$reps, na.rm = TRUE)
  total_reps <- sum(filtered_data$reps, na.rm = TRUE)
  
  
  
  return(list(
    number_of_workouts = number_of_workouts,
    total_duration = round(total_duration,2),
    total_volume = round(total_volume / 1000,2),
    total_reps = total_reps
  ))
}
