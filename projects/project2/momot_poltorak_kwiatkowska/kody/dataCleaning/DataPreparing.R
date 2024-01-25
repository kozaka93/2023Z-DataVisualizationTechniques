library(dplyr)
library(hms)
library(plotly)

activities_Ola <- read.csv("ActivitiesOla.csv")
activities_Kuba <- read.csv("activities_JP.csv")
activities_Maciek <- read.csv("ActivitiesMaciek.csv")

# Ola

activities_Ola <- activities_Ola %>% 
  select(Data, Dystans, Średnia.prędkość, Czas) %>% 
  mutate(Osoba = "Ola")

activities_Ola$Data <- as.POSIXct(activities_Ola$Data, format = "%Y-%m-%d %H:%M:%S")

activities_Ola <- activities_Ola %>%
  mutate(Year = format(Data, "%Y"),
         Month = format(Data, "%m"),
         Day = format(Data, "%d"),
         Hour = format(Data, "%H"),
         Minute = format(Data, "%M"),
         Second = format(Data, "%S"))  

activities_Ola$Czas <- as_hms(activities_Ola$Czas)
activities_Ola$DurationMinutes <- as.numeric(activities_Ola$Czas) / 60

activities_Ola %>% 
  mutate(Type = case_when(Dystans < 40 ~ "Short",
                          Dystans < 70 ~ "Medium",
                          TRUE ~ "Long")) -> activities_Ola

activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long")) 

activities_Ola <- activities_Ola %>% 
  rename(Date = Data, AvgSpeed = Średnia.prędkość, Distance = Dystans) %>% 
  select(-Czas) 


# Kuba

activities_Kuba <- activities_Kuba %>% 
  filter(Activity.Type == "Ride") %>% 
  select(startTime, Distance, srednia_predkosc_km_h, Czas, Osoba)


activities_Kuba$startTime <- as.POSIXct(activities_Kuba$startTime, format = "%Y-%m-%d %H:%M:%S")

activities_Kuba <- transform(activities_Kuba,
                            Year = format(startTime, "%Y"),
                            Month = format(startTime, "%m"),
                            Day = format(startTime, "%d"),
                            Hour = format(startTime, "%H"),
                            Minute = format(startTime, "%M"),
                            Second = format(startTime, "%S"))

activities_Kuba <- activities_Kuba %>% 
  rename(Date = startTime, DurationMinutes = Czas, AvgSpeed = srednia_predkosc_km_h)

activities_Kuba %>% 
  mutate(Type = case_when(Distance < 40 ~ "Short",
                          Distance < 70 ~ "Medium",
                          TRUE ~ "Long")) -> activities_Kuba

activities_Kuba$Type <- factor(activities_Kuba$Type, levels = c("Short", "Medium", "Long")) 

  


# Maciek

activities_Maciek$Czas <- as_hms(activities_Maciek$Czas)
activities_Maciek$DurationMinutes <- as.numeric(activities_Maciek$Czas) / 60

activities_Maciek <- activities_Maciek %>% 
  rename(Distance = Dystans, Date = startTime, AvgSpeed = avgSpeed) %>% 
  select(-Kalorie, - Czas, - endTime)

activities_Maciek$Date <- as.POSIXct(activities_Maciek$Date, format = "%Y-%m-%d %H:%M:%S")

activities_Maciek <- activities_Maciek %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m"),
         Day = format(Date, "%d"),
         Hour = format(Date, "%H"),
         Minute = format(Date, "%M"),
         Second = format(Date, "%S")) 

activities_Maciek %>% 
  mutate(Type = case_when(Distance < 40 ~ "Short",
                          Distance < 70 ~ "Medium",
                          TRUE ~ "Long")) -> activities_Maciek

activities_Maciek$Type <- factor(activities_Maciek$Type, levels = c("Short", "Medium", "Long")) 


# TOGETHER

activities <- bind_rows(activities_Kuba, activities_Maciek, activities_Ola)

write.csv(activities, file = "activities.csv", row.names = FALSE)

