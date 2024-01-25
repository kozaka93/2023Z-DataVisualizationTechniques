library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemr)
library(htmltools)
library(chron)

## Preparing the data

# File paths
input_file <- "./data/sleep-export/sleep-export-H2.csv"

# Open the file for reading
con <- file(input_file, "r")

# Initialize variables
file_number <- 1
data_list <- list()
current_data <- NULL
start_reading <- FALSE

# Process the file line by line
while (TRUE) {
  line <- readLines(con, n = 1)
  if (length(line) == 0) {
    break
  }
  
  if (grepl("^Id,Tz,From,To", line)) {
    if (start_reading) {
      data_list[[paste0("sd", file_number)]] <- current_data
      file_number <- file_number + 1
    }
    current_data <- NULL
    start_reading <- TRUE
    
    # Store the column names and remove the double quotes
    col_names <- gsub('"', '', unlist(strsplit(line, ",")))
    
    # Read the next line for values
    line <- readLines(con, n = 1)
    values <- gsub('"', '', unlist(strsplit(line, ",")))
    
    # Create a dataframe with column names and values
    current_data <- as.data.frame(matrix(values, nrow = 1, byrow = TRUE))
    names(current_data) <- col_names
  }
}

# Save the last part
if (!is.null(current_data)) {
  data_list[[paste0("sd", file_number)]] <- current_data
}

# Close the file
close(con)

# Format dates and times

for (i in 1:length(data_list)) {
  data_list[[paste0("sd", i)]] <- data_list[[paste0("sd", i)]] %>%
    select(-Event) %>%
    pivot_longer(cols = -c(Id, Tz, From, To, Sched, Hours, Rating, Comment, Framerate, Snore, Noise, Cycles, DeepSleep, LenAdjust, Geo), names_to = "Time", values_to = "Value")    
  
  data_list[[paste0("sd", i)]]$To <- as.POSIXct(data_list[[paste0("sd", i)]]$To, format = "%d. %m. %Y %H:%M")
  data_list[[paste0("sd", i)]]$From <- as.POSIXct(data_list[[paste0("sd", i)]]$From, format = "%d. %m. %Y %H:%M")
  # Format Value to numeric
  data_list[[paste0("sd", i)]]$Value <- as.numeric(data_list[[paste0("sd", i)]]$Value)
  
  data_list[[paste0("sd", i)]]$Time <- format(as.POSIXct(data_list[[paste0("sd", i)]]$Time, format = "%H:%M"), format = "%H:%M")
}

# Diagram for the first day of sleep
ggthemr::ggthemr(palette = 'earth')
data_list[[1]] %>%
  select(Time, Value) %>%
  ggplot(aes(x = Time, y = Value, group = 1)) +
  geom_area(alpha = 0.5, fill = "#DDEEEE") + 
  stat_smooth(
     geom = 'area', method = 'loess', span = 0.15, formula = 'y~x', se = FALSE, fill = 'blue', alpha = 0.8
  ) + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Sleep activity on 20.12.2023",
    x = "Time during the night",
    y = "Sleep activity",
    caption = htmltools::HTML("10 - fully awake, 0 - deeply asleep")
  ) + 
  scale_x_discrete(
    breaks = c("00:35", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00")
  )

# Second day of sleep

data_list[[2]] %>%
  select(Time, Value) %>%
  ggplot(aes(x = Time, y = Value, group = 1)) +
  geom_area(alpha = 0.5, fill = "#DDEEEE") + 
  stat_smooth(
    geom = 'area', method = 'loess', span = 0.15, formula = 'y~x', se = FALSE, fill = 'blue', alpha = 0.8
  ) + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Sleep activity on 19.12.2023",
    x = "Time during the night",
    y = "Sleep activity",
    caption = htmltools::HTML("10 - fully awake, 0 - deeply asleep")
  ) + 
  scale_x_discrete(
    breaks = c("01:27", "02:02", "03:01", "04:01", "05:00", "06:00", "07:00", "08:09")
  )


# Time of going to bed on each day
# Prepare data
sleeptime <- data_list[[1]] %>%
  select(From) %>%
  head(1)

for(i in 2:length(data_list)) {
  sleeptime <- rbind(sleeptime, data_list[[paste0("sd", i)]] %>%
    select(From) %>%
    head(1))
}

sleeptime %>% 
  arrange(From) -> sleeptime

sleeptime$Date <- as.Date(sleeptime$From, format = "%Y-%m-%d %H:%M:%S")
sleeptime$Time <- times(format(as.POSIXct(sleeptime$From, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")) # tymczasowo do czasu pójścia spać dopisuje się też data
sleeptime$From <- NULL
sleeptime$TimeN <- as.numeric(sleeptime$Time)

# Line plot
sleeptime %>%
  head(8) %>%
  ggplot(aes(x = Date, y = TimeN - 0.04166667)) +
  geom_point() + 
  geom_line() + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Time of going to sleep each day",
    x = "Date",
    y = "Time of going to sleep"
  ) +
  scale_y_chron(
    format = "%H:%M"
  )
  

## Emotions and mood during the week
# Prepare data
daylio <- read.csv("./data/daylio-export/daylio-export.csv", header = TRUE)

# Create a column indicating if there was stress value in activities column
act <- daylio$activities
act <- as.character(act)
stres <- grepl("stres", act)

daylio$stres <- stres
daylio %>%
  group_by(weekday) %>%
  summarise(stres = sum(stres)) -> dni_stres

# Order weekdays
dni_stres$weekday <- factor(dni_stres$weekday, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))

# Bar plot
ggthemr::ggthemr(palette = 'earth')
dni_stres %>%
  ggplot(aes(x = weekday, y = stres)) +
  geom_bar(stat = "identity", fill = "#DDEEEE") + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
    axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Number of stressful days per weekday",
    x = "Day of the week",
    y = "Number of stressful days"
  )

# Relax weekdays

relax <- grepl("relaks", act)

daylio$relaks <- relax
daylio %>%
  group_by(weekday) %>%
  summarise(relaks = sum(relaks)) -> dni_relaks

# Order weekdays
dni_relaks$weekday <- factor(dni_relaks$weekday, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))

dni_relaks %>%
  ggplot(aes(x = weekday, y = relaks)) +
  geom_bar(stat = "identity", fill = "#DDEEEE") + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Number of relax days per weekday",
    x = "Day of the week",
    y = "Number of relax days"
  )


# Mapping moods to integers:
map_mood <- data.frame(
  mood = c("wspaniale", "dobrze", "tak sobie", "źle", "bardzo źle"),
  value = c(5, 4, 3, 2, 1)
)

daylio_moods <- merge(daylio, map_mood, by = "mood")


# Change of mood in time (lollipo plot)
daylio_moods %>%
  ggplot(aes(x = as.Date(full_date), y = value)) +
  geom_linerange(aes(ymin = 1, ymax = value), size = 1) + 
  geom_point(aes(color = value),size = 4) + 
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Mood change in time",
    x = "Date",
    y = "Mood"
  ) + 
  scale_x_date(
    date_breaks = "2 days",
    date_labels = "%d %b"
  ) + 
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("bardzo źle", "źle", "tak sobie", "dobrze", "wspaniale"),
    limits = c(1,5.5)
  ) + 
  scale_color_gradientn(
    colours = c("#FF0000", "#FFA500", "#FFFF00", "#00FF00", "#008000")
  )
  
