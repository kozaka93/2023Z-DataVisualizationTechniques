
library(dplyr)
library(tidyverse)
library(stringi)
library(jsonlite)
library(tidytext)
data("stop_words")

convert <- function(path){
  data <- fromJSON(path,simplifyDataFrame = TRUE) 
  data %>% 
    rowwise() %>% 
    mutate(
      channel = ifelse(length(subtitles) > 0, subtitles$name, NA ),
      channelURl = ifelse(length(subtitles) > 0, subtitles[2], NA ),
      details = ifelse(length(details) > 0, details[1], NA )) -> fixed
  fixed %>%
    filter(is.character(details))%>%
    separate_wider_delim(time,"T", names = c("date", "time")) %>%
    mutate(
      title = str_replace(title,"Obejrzano: ", ""),
      zone = substr(time,13,13),
      hour = as.double(substr(time,1,2)),
      minute = as.double(substr(time,4,5)),
      date = as.Date(date),
      weekday = weekdays.Date(date)) %>% 
    select(header,title,titleUrl,channel,channelURl,date,weekday,
           time,hour,minute,zone,products,details,description) -> adds
  return(fixed)
}
get_adds_Name <- function(data){
  data %>%
    filter(is.character(details))%>%
    separate_wider_delim(time,"T", names = c("date", "time")) %>%
    mutate(
      title = str_replace(title,"Obejrzano: ", ""),
      zone = substr(time,13,13),
      hour = as.double(substr(time,1,2)),
      minute = as.double(substr(time,4,5)),
      date = as.Date(date),
      weekday = weekdays.Date(date)) %>% 
    select(header,title,titleUrl,channel,channelURl,date,weekday,
           time,hour,minute,zone,products,details,description,name) -> adds
  return(adds)
}
get_clean_data <- function(data){
  data %>% 
    filter(!is.na(channel))%>% #removes ads and deleted films
    separate_wider_delim(time,"T", names = c("date", "time")) %>%
    mutate(
      title = str_replace(title,"Obejrzano: ", ""),
      zone = substr(time,13,13),
      hour = as.double(substr(time,1,2)),
      minute = as.double(substr(time,4,5)),
      date = as.Date(date),
      weekday = weekdays.Date(date)) %>% 
    select(header,title,titleUrl,channel,channelURl,date,weekday,
           time,hour,minute,zone,products,details,description)-> clean
  return(clean)
}
get_clean_data_Name <- function(data){
  data %>% 
    filter(!is.na(channel))%>% #removes ads and deleted films
    separate_wider_delim(time,"T", names = c("date", "time")) %>%
    mutate(
      title = str_replace(title,"Obejrzano: ", ""),
      zone = substr(time,13,13),
      hour = as.double(substr(time,1,2)),
      minute = as.double(substr(time,4,5)),
      date = as.Date(date),
      weekday = weekdays.Date(date)) %>% 
    select(header,title,titleUrl,channel,channelURl,date,weekday,
           time,hour,minute,zone,products,details,description,name)-> clean
  return(clean)
}

get_word_stats <- function(df){
  tmp <- df$title %>%
    str_split(boundary("word")) %>%
    unlist() %>%
    tolower() %>%
    data.frame(word = ., stringsAsFactors = FALSE) %>%
    mutate(word = gsub("[^[:alnum:]]", "", word)) %>% 
    filter(nchar(word) >= 4) %>% 
    group_by(word) %>%
    summarise(number = n()) %>%
    filter(!(word %in% stop_words$word)) %>% 
    arrange(desc(number))
}

