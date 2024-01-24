library(tidyverse)
library(shiny)
library(plotly)
library(shinydashboard)
library(dashboardthemes)




#####combined_emoji#####
pipi_emoji <- read_csv("data/pozor/0_query_emoji.csv")
kiddo_emoji <- read_csv("data/kiddo/k_query_emoji.csv")
misiu_emoji <- read_csv("data/misiu/m_query_emoji.csv")

combined_emoji <- rbind(pipi_emoji,kiddo_emoji,misiu_emoji) 





#####count_messages#####
pipi_count_messages <- read_csv("data/pozor/0_query_count_messages.csv") %>%
  mutate(df_name = "Paweł Pozorski")

pipi_count_messages$gender <- ifelse(pipi_count_messages$gender == "unknown","male",pipi_count_messages$gender)
pipi_count_messages$round_2week <- cut(pipi_count_messages$date, breaks = "2 weeks")

misiu_count_messages <- read_csv("data/misiu/m_query_count_messages.csv") %>%
  mutate(df_name = "Michał Iwaniuk")

misiu_count_messages$gender <- ifelse(misiu_count_messages$gender == "unknown","male",misiu_count_messages$gender)
misiu_count_messages$round_2week <- cut(misiu_count_messages$date, breaks = "2 weeks")

kiddo_count_messages <- read_csv("data/kiddo/k_query_count_messages.csv") %>%
  mutate(df_name = "Krzysiek Adamczyk")

kiddo_count_messages$gender <- ifelse(kiddo_count_messages$gender == "unknown","male",kiddo_count_messages$gender)
kiddo_count_messages$round_2week <- cut(kiddo_count_messages$date, breaks = "2 weeks")


combined_count_messages <- rbind(pipi_count_messages,misiu_count_messages,kiddo_count_messages)

name_gender <- combined_count_messages %>% distinct(name,gender) %>%
  na.omit()




male_names <- unique(combined_count_messages$name[combined_count_messages$gender=="male"])
female_names <- unique(combined_count_messages$name[combined_count_messages$gender=="female"])
new_male_names <- sample(male_names)
new_female_names <- sample(female_names)


dn1 <- data.frame(old_name = c(male_names,female_names), 
                  new_name = c(new_male_names, new_female_names))
dn1 <- dn1 %>% mutate(new_name = case_when(old_name == "Paweł Pozorski" ~ "Paweł Pozorski",
                                           old_name == "Michał Iwaniuk" ~ "Michał Iwaniuk",
                                           old_name == "Krzysiek Adamczyk" ~ "Krzysiek Adamczyk",
                                           T ~ new_name))

pipi_count_messages <- pipi_count_messages %>% rename(old_name = name)
pipi_count_messages <- pipi_count_messages %>% left_join(dn1) 
pipi_count_messages <- pipi_count_messages %>% rename(name = new_name)

misiu_count_messages <- misiu_count_messages %>% rename(old_name = name)
misiu_count_messages <- misiu_count_messages %>% left_join(dn1) 
misiu_count_messages <- misiu_count_messages %>% rename(name = new_name)

kiddo_count_messages <- kiddo_count_messages %>% rename(old_name = name)
kiddo_count_messages <- kiddo_count_messages %>% left_join(dn1) 
kiddo_count_messages <- kiddo_count_messages %>% rename(name = new_name)



combined_count_messages <- rbind(pipi_count_messages,misiu_count_messages,kiddo_count_messages)





pipi_line3_count_messages <- pipi_count_messages %>% 
  filter(name == "Paweł Pozorski")

kiddo_line3_count_messages <- kiddo_count_messages %>% 
  filter(name == "Krzysiek Adamczyk")

misiu_line3_count_messages <- misiu_count_messages %>% 
  filter(name == "Michał Iwaniuk")


line3_date_sequence <- seq(as.Date("2017-01-01"),as.Date("2024-01-01"),"14 days")

pipi_n_messages <- sapply(line3_date_sequence,
                          function(d){
                            wartosc <- pipi_line3_count_messages %>%
                              filter(date <= d) %>%
                              pull(`min_messages_is_0`) %>% sum()
                          }) 


misiu_n_messages <- sapply(line3_date_sequence,
                          function(d){
                            wartosc <- misiu_line3_count_messages %>%
                              filter(date <= d) %>%
                              pull(`min_messages_is_0`) %>% sum()
                          }) 

kiddo_n_messages <- sapply(line3_date_sequence,
                           function(d){
                             wartosc <- kiddo_line3_count_messages %>%
                               filter(date <= d) %>%
                               pull(`min_messages_is_0`) %>% sum()
                           }) 

line3_count_messages <- rbind(tibble(n_messages = pipi_n_messages, date = line3_date_sequence, name = "Paweł Pozorski"),
                              tibble(n_messages = misiu_n_messages, date = line3_date_sequence, name = "Michał Iwaniuk"),
                              tibble(n_messages = kiddo_n_messages, date = line3_date_sequence, name = "Krzysiek Adamczyk"))








datalistPP <- lapply(seq(as.Date("2017-01-01"), as.Date("2024-01-01"), by = "15 days"), 
                    function(d) {
                      pipi_count_messages %>% filter(!is_group) %>%
                        filter(date <= d) %>%
                        group_by(conversation_id,name) %>%
                        summarise(n = sum(`min_messages_is_0`)) %>%
                        summarise(name = paste(name,collapse = ""),n = sum(n)) %>%
                        ungroup() %>%
                        mutate(name = str_replace_all(name,
                                                      as.character("Paweł Pozorski"),
                                                      "")) %>%
                        mutate(date = d) 
                    })

pipi_top_users <- do.call(rbind, datalistPP)
pipi_top_users <- left_join(pipi_top_users,name_gender)


datalistMM <- lapply(seq(as.Date("2017-01-01"), as.Date("2024-01-01"), by = "15 days"), 
                     function(d) {
                       misiu_count_messages %>% filter(!is_group) %>%
                         filter(date <= d) %>%
                         group_by(conversation_id,name) %>%
                         summarise(n = sum(`min_messages_is_0`)) %>%
                         summarise(name = paste(name,collapse = ""),n = sum(n)) %>%
                         ungroup() %>%
                         mutate(name = str_replace_all(name,
                                                       as.character("Michał Iwaniuk"),
                                                       "")) %>%
                         mutate(date = d) 
                     })

misiu_top_users <- do.call(rbind, datalistMM)
misiu_top_users <- left_join(misiu_top_users,name_gender)


datalistKK <- lapply(seq(as.Date("2017-01-01"), as.Date("2024-01-01"), by = "15 days"), 
                     function(d) {
                       kiddo_count_messages %>% filter(!is_group) %>%
                         filter(date <= d) %>%
                         group_by(conversation_id,name) %>%
                         summarise(n = sum(`min_messages_is_0`)) %>%
                         summarise(name = paste(name,collapse = ""),n = sum(n)) %>%
                         ungroup() %>%
                         mutate(name = str_replace_all(name,
                                                       as.character("Krzysiek Adamczyk"),
                                                       "")) %>%
                         mutate(date = d) 
                     })

kiddo_top_users <- do.call(rbind, datalistKK)
kiddo_top_users <- left_join(kiddo_top_users,name_gender) 

kiddo_top_users$df_name = "Krzysiek Adamczyk"
misiu_top_users$df_name = "Michał Iwaniuk"
pipi_top_users$df_name = "Paweł Pozorski"

top_users = rbind(kiddo_top_users,misiu_top_users,pipi_top_users) %>% na.omit()






#####common_strings#####
pipi_common_strings_1 <- read_csv("data/pozor/0_query_most_common_strings_1.csv")
pipi_common_strings_2 <- read_csv("data/pozor/0_query_most_common_strings_2.csv")
pipi_common_strings_3 <- read_csv("data/pozor/0_query_most_common_strings_3.csv")

misiu_common_strings_1 <- read_csv("data/misiu/m_query_most_common_strings_1.csv")
misiu_common_strings_2 <- read_csv("data/misiu/m_query_most_common_strings_2.csv")
misiu_common_strings_3 <- read_csv("data/misiu/m_query_most_common_strings_3.csv")

kiddo_common_strings_1 <- read_csv("data/kiddo/k_query_most_common_strings_1.csv")
kiddo_common_strings_2 <- read_csv("data/kiddo/k_query_most_common_strings_2.csv")
kiddo_common_strings_3 <- read_csv("data/kiddo/k_query_most_common_strings_3.csv")

combined_common_strings_1 <- rbind(pipi_common_strings_1,
                                   misiu_common_strings_1,
                                   kiddo_common_strings_1) 

combined_common_strings_2 <- rbind(pipi_common_strings_2,
                                   misiu_common_strings_2,
                                   kiddo_common_strings_2)
combined_common_strings_2 <- combined_common_strings_2[-1,]

combined_common_strings_3 <- rbind(pipi_common_strings_3,
                                   misiu_common_strings_3,
                                   kiddo_common_strings_3) 

combined_common_strings_1$name = ifelse(combined_common_strings_1$name == "Mateusz Rapa", "Paweł Pozorski", combined_common_strings_1$name)
combined_common_strings_2$name = ifelse(combined_common_strings_2$name == "Mateusz Rapa", "Paweł Pozorski", combined_common_strings_2$name)
combined_common_strings_3$name = ifelse(combined_common_strings_3$name == "Mateusz Rapa", "Paweł Pozorski", combined_common_strings_3$name)

combined_common_strings_1 <- combined_common_strings_1 %>%
  filter(!str_detect(sequence_of_strings,"rozmawiać|odebrać|zadzwonić|nick|użytkownik|dodać|grupa|rozmowa|załącznik|zagrać|fish|reakcja"))%>%
  filter(str_detect(sequence_of_strings,"^\\w+$"))

combined_common_strings_2 <- combined_common_strings_2 %>%
  filter(!str_detect(sequence_of_strings,"rozmawiać|odebrać|zadzwonić|nick|użytkownik|dodać|grupa|rozmowa|załącznik|zagrać|fish|reakcja"))%>%
  filter(str_detect(sequence_of_strings,"^\\w+\\s\\w+$"))

combined_common_strings_3 <- combined_common_strings_3 %>%
  filter(!str_detect(sequence_of_strings,"rozmawiać|odebrać|zadzwonić|nick|użytkownik|dodać|grupa|rozmowa|załącznik|zagrać|fish|reakcja"))%>%
  filter(str_detect(sequence_of_strings,"^\\w+\\s\\w+\\s\\w+$"))



#####time_respond####

pipi_time_respond <- read_csv("data/pozor/0_query_time_to_responde.csv") %>% 
  mutate(day_send = format(time_send,"%A"), 
         round_time_send = round_date(time_send, "15 mins"),
         name = "Paweł Pozorski",
         delta = as.numeric(delta)/60) %>%
  mutate(round_hour_send = format(round_time_send, "%H:%M")) %>%
  rename(delta_min = delta) 



kiddo_time_respond <- read_csv("data/kiddo/k_query_time_to_responde.csv") %>% 
  mutate(day_send = format(time_send,"%A"), 
         round_time_send = round_date(time_send, "15 mins"),
         name = "Krzysiek Adamczyk",
         delta = as.numeric(delta)/60) %>%
  mutate(round_hour_send = format(round_time_send, "%H:%M")) %>%
  rename(delta_min = delta)



misiu_time_respond <- read_csv("data/misiu/m_query_time_to_responde.csv") %>% 
  mutate(day_send = format(time_send,"%A"), 
         round_time_send = round_date(time_send, "15 mins"),
         name = "Michał Iwaniuk",
         delta = as.numeric(delta)/60) %>%
  mutate(round_hour_send = format(round_time_send, "%H:%M")) %>%
  rename(delta_min = delta)


pipi_time_respond <- pipi_time_respond %>% select(time_send,delta_min,day_send,round_time_send,name,round_hour_send)
misiu_time_respond <- misiu_time_respond %>% select(time_send,delta_min,day_send,round_time_send,name,round_hour_send)
kiddo_time_respond <- kiddo_time_respond %>% select(time_send,delta_min,day_send,round_time_send,name,round_hour_send)

combined_time_respond <-rbind(pipi_time_respond,kiddo_time_respond,misiu_time_respond) %>% 
  filter(time_send >= as.Date("2017-01-01"),
         delta_min <=720)

combined_time_respond$round_2week <- cut(combined_time_respond$time_send, breaks = "2 weeks")


combined_time_respond$day_send <- factor(combined_time_respond$day_send, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
combined_time_respond$name <- factor(combined_time_respond$name)
combined_time_respond$round_2week <- factor(combined_time_respond$round_2week)


line_time_respond <- combined_time_respond %>% 
  group_by(name,round_2week) %>%
  summarise(mean_delta = round(mean(delta_min),digits = 2))



line_time_respond_df <- line_time_respond %>%
  mutate(label = paste("Zakres dat : ",round_2week," +-7 dni","\n", "Średni czas odpowiedzi: ", mean_delta, " min", sep=""))






#####other####



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



#####processed_data####

combined_emoji[,c(-1,-3)] %>% 
  write_csv("processed_data/combined_emoji.csv")

combined_count_messages[,c(1,3,5,6,8,12,14)] %>% 
  write_csv("processed_data/combined_count_messages.csv")

top_users[,c(-1)] %>%
  write_csv("processed_data/top_users.csv")

pipi_count_messages[,c(3,6,8,12,13,14)] %>% 
  write_csv("processed_data/pipi_count_messages.csv")

misiu_count_messages[,c(3,6,8,12,13,14)] %>% 
  write_csv("processed_data/misiu_count_messages.csv")

kiddo_count_messages[,c(3,6,8,12,13,14)] %>% 
  write_csv("processed_data/kiddo_count_messages.csv")

combined_time_respond[,c(-7)] %>%
  write_csv("processed_data/combined_time_respond.csv")

line_time_respond_df %>%
  write_csv("processed_data/line_time_respond_df.csv")

combined_common_strings_1[,c(2,4,5)]%>%
  write_csv("processed_data/combined_common_strings_1.csv")

combined_common_strings_2[,c(2,4,5)]%>%
  write_csv("processed_data/combined_common_strings_2.csv")

combined_common_strings_3[,c(2,4,5)]%>%
  write_csv("processed_data/combined_common_strings_3.csv")

line3_count_messages %>%
  write_csv("processed_data/line3_count_messages.csv")



