library(tidyverse)
library(shiny)
library(plotly)
library(shinydashboard)
library(dashboardthemes)


combined_emoji <-
  read_csv("processed_data/combined_emoji.csv")

combined_count_messages <-
  read_csv("processed_data/combined_count_messages.csv")
name_gender <- combined_count_messages %>% distinct(name,gender) %>%
  na.omit()

top_users <-
  read_csv("processed_data/top_users.csv")

pipi_count_messages <-
  read_csv("processed_data/pipi_count_messages.csv")

misiu_count_messages <-
  read_csv("processed_data/misiu_count_messages.csv")

kiddo_count_messages <-
  read_csv("processed_data/kiddo_count_messages.csv")

line3_count_messages <-
  read_csv("processed_data/line3_count_messages.csv")

combined_time_respond <-
  read_csv("processed_data/combined_time_respond.csv")
combined_time_respond$day_send <- factor(combined_time_respond$day_send, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
combined_time_respond$round_hour_send = format(combined_time_respond$round_time_send, "%H:%M")

line_time_respond_df <-
  read_csv("processed_data/line_time_respond_df.csv")

combined_common_strings_1 <-
  read_csv("processed_data/combined_common_strings_1.csv")

combined_common_strings_2 <-
  read_csv("processed_data/combined_common_strings_2.csv")

combined_common_strings_3 <-
  read_csv("processed_data/combined_common_strings_3.csv")

hours <- c("00:00", "00:15", "00:30", "00:45", "01:00", "01:15", "01:30", "01:45", "02:00",
           "02:15", "02:30", "02:45", "03:00", "03:15", "03:30", "03:45", "04:00", "04:15",
           "04:30", "04:45", "05:00", "05:15", "05:30", "05:45", "06:00", "06:15", "06:30",
           "06:45", "07:00", "07:15", "07:30", "07:45", "08:00", "08:15", "08:30", "08:45",
           "09:00", "09:15", "09:30", "09:45", "10:00", "10:15", "10:30", "10:45", "11:00",
           "11:15", "11:30", "11:45", "12:00", "12:15", "12:30", "12:45", "13:00", "13:15",
           "13:30", "13:45", "14:00", "14:15", "14:30", "14:45", "15:00", "15:15", "15:30",
           "15:45", "16:00", "16:15", "16:30", "16:45", "17:00", "17:15", "17:30", "17:45",
           "18:00", "18:15", "18:30", "18:45", "19:00", "19:15", "19:30", "19:45", "20:00",
           "20:15", "20:30", "20:45", "21:00", "21:15", "21:30", "21:45", "22:00", "22:15",
           "22:30", "22:45", "23:00", "23:15", "23:30", "23:45")

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
days_hours <- expand.grid(days,hours)
