library(sf)
library(spData)
library(svglite)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(extrafont)
library(patchwork)

#Mapa przedstawiająca średnią ilość emitowanych gazów w 21 wieku na świecie

emission_data <- read_csv("C:/Users/milos/Desktop/DaneProjektR/Emissions_Totals_E_All_Data_NOFLAG.csv")
emission_data_groups <- read_csv("C:/Users/milos/Desktop/DaneProjektR/Emissions_Totals_E_All_Area_Groups_NOFLAG.csv")
emission_data <- emission_data %>% anti_join(emission_data_groups %>% distinct(Area),join_by(Area))
col_names = names(emission_data)
new_col_names <- case_when(grepl("^Y\\d{4}", col_names) ~ substr(col_names,2,nchar(col_names)),
                           TRUE ~ col_names)
colnames(emission_data) <- new_col_names
emission_data <- emission_data %>% pivot_longer("1961":"2050") %>% filter(name >= 2000 & name <= 2020 & !is.na(value)) %>% group_by(Area) %>%
  summarize(meanEmission = log(mean(value)))
mapdata <- map_data("world")
emission_data$Area[emission_data$Area == 'United States of America'] <- 'USA'
emission_data$Area[emission_data$Area == 'Russian Federation'] <- 'Russia'
emission_data$Area[emission_data$Area == 'Bolivia (Plurinational State of)'] <- 'Bolivia'
emission_data$Area[emission_data$Area == ''] <- 'Turkey'
emission_data$Area[emission_data$Area == 'Venezuela (Bolivarian Republic of)'] <- 'Venezuela'
emission_data$Area[emission_data$Area == 'Iran (Islamic Republic of)'] <- 'Iran'
emission_data <- emission_data %>% full_join(mapdata,join_by(Area==region))
emission_data <- emission_data %>% mutate(meanEmission = ifelse((is.na(meanEmission) | meanEmission <= 0),0,meanEmission))
emission_data <- emission_data %>% mutate(value_cut = cut(meanEmission, breaks=c(0,1,4,9,10,11,12,14),include.lowest = TRUE)) 
emission_data <- emission_data %>% filter(Area != "Antarctica")

moja_paleta <- colorRampPalette(c("yellow", "red1"))(8)

map <- ggplot(emission_data, aes(x = long, y = lat, group = group)) +
       geom_polygon(aes(fill = value_cut), color = "black", size = 0.2) +
       viridis::scale_fill_viridis(discrete = TRUE,direction=-1) +
       scale_fill_manual(values = moja_paleta) +
       ggtitle("Average emission from food production in the world in the 21st century") +
       guides(fill = guide_legend(title = "Average emission in kilotonnes (log)")) +
       theme_minimal() +
       theme(plot.title = element_text(hjust=0.5,vjust=0.5,size=24,family="Trebuchet MS"),
            legend.title = element_text(size=20,family="Trebuchet MS"),
            legend.text = element_text(size = 16),
            axis.text = element_blank(),
            legend.position = "bottom",
            axis.title = element_blank())
map

ggsave("mapaFoodProduction.svg", plot = map, width = 14, height = 8, units = "in")

#Wykres przedstawiający jak zmienia się ilość produkowanych gazów w kolejnych latach 21 wieku

emission_data <- read_csv("C:/Users/milos/Desktop/DaneProjektR/Emissions_Totals_E_All_Data_NOFLAG.csv")
View(emission_data)
emission_data_groups <- read_csv("C:/Users/milos/Desktop/DaneProjektR/Emissions_Totals_E_All_Area_Groups_NOFLAG.csv")
emission_data <- emission_data %>% anti_join(emission_data_groups %>% distinct(Area),join_by(Area))
col_names = names(emission_data)
new_col_names <- case_when(grepl("^Y\\d{4}", col_names) ~ substr(col_names,2,nchar(col_names)),
                           TRUE ~ col_names)
colnames(emission_data) <- new_col_names
emission_data <- emission_data %>% pivot_longer("1961":"2050") %>% filter(name >= 2000 & name <= 2020 & !is.na(value)) %>%
  rename(year = name, emission = value)
emission_data <- emission_data %>% group_by(year) %>% summarize(totalEmission = sum(emission))
emission_data <- as.data.frame(emission_data)






dt <- readr::read_csv("C:/Users/milos/Downloads/Environment_Emissions_intensities_E_All_Data_NOFLAG.csv")

View(dt)

col_names <-  colnames(dt)
new_col_names <- case_when(grepl("^Y\\d{4}", col_names) ~ substr(col_names,2,nchar(col_names)),
                           TRUE ~ col_names)
colnames(dt) <- new_col_names
years <- 2000:2020

mean_intensity_by_item_and_country <- dt %>%
  filter(Element == "Emissions intensity") %>%
  select(Area, Item, as.character(years)) %>%
  pivot_wider(names_from = Item,
              values_from = as.character(years)) %>%
  pivot_longer(cols = -Area,
               names_to = "year_item",
               values_to = "intensity") %>%
  separate(year_item, 
           into = c("Year", "Item"),
           sep = "_") %>% 
  group_by(Area, Item) %>%
  summarise(mean_intestity = mean(intensity))

# emission intensity by country in 21st century

emission_production_by_year <- dt %>%
  filter(Element == "Production") %>% 
  select(Area, Element, as.character(years)) %>%
  pivot_longer(cols = "2000":"2020",values_to="production",names_to = "year") %>% 
  group_by(year) %>% summarize(totalProduction = sum(production,na.rm = TRUE))

df <- emission_data %>% cbind(emission_production_by_year) %>% select(-3)

 
pkpd <- ggplot(df, aes(x = year, y = totalEmission, group = 0)) +
  geom_line(aes(color = "Total emission"),size=3) +
  geom_line(aes(y = totalProduction/50,color = "Total production", group = 0),size=3) +
  scale_y_continuous(sec.axis = sec_axis(~.*50, name="Total production (tones)",labels = scales::label_number(scale = 1e-9,suffix = " B")),labels = scales::label_number(scale = 1e-6,suffix=" B")) +
  labs(x = "Year", y = "Total emission (tones)", color = "",title = "Total emission and production in the 21st century") +
  scale_color_manual(values = c("red", "orange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5,vjust=0.5,size=35,family="Trebuchet MS"),
        axis.title = element_text(size=23,family="Trebuchet MS"),
        legend.position = "bottom",
        legend.key.width = unit(4, "cm"),   # Zmiana szerokości legendy
        legend.key.height = unit(1.5, "cm"),
        legend.text = element_text(size = 12,"Trebuchet MS"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        legend.text = element_text(size = 22)) +
  scale_x_discrete(breaks = df$year[seq_along(df$year) %% 2 == 1])

 
pkpd

ggsave("lineFoodProduction.svg", plot = pkpd, width = 14, height = 8, units = "in")


             