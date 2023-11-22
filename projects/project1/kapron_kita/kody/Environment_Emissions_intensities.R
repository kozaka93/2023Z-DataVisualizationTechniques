library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(mapproj)
library(svglite)
library(ggrepel)
library(extrafont)
# font_import()
# loadfonts(device = "win")

dt <- readr::read_csv("Environment_Emissions_intensities_E_All_Data/Environment_Emissions_intensities_E_All_Data_NOFLAG.csv")

col_names <-  colnames(dt)
new_col_names <- case_when(grepl("^Y\\d{4}", col_names) ~ substr(col_names,2,nchar(col_names)),
                           TRUE ~ col_names)
colnames(dt) <- new_col_names
# years <- 2000:2020
# 
# mean_intensity_by_item_and_country <- dt %>%
#   filter(Element == "Emissions intensity") %>%
#   select(Area, Item, as.character(years)) %>%
#   pivot_wider(names_from = Item,
#               values_from = as.character(years)) %>%
#   pivot_longer(cols = -Area,
#                names_to = "year_item",
#                values_to = "intensity") %>%
#   separate(year_item, 
#            into = c("Year", "Item"),
#            sep = "_") %>% 
#   group_by(Area, Item) %>%
#   summarise(mean_intestity = mean(intensity))

# emission intensity by country in 21st century

emission_intensity_by_country <- dt %>%
  filter(Element %in% c("Emissions (CO2eq) (AR5)", "Production")) %>% 
  select(`Area Code`, Element, as.character(years)) %>%   
  pivot_wider(names_from = Element,
              values_from = as.character(years)) %>% 
  pivot_longer(cols = -`Area Code`,
               names_to = "year_element",
               values_to = "value") %>% 
  mutate(value = sapply(value, na.omit)) %>% 
  mutate(value = sapply(value, sum),
         element = gsub(".*_(Production|Emissions).*", "\\1", year_element)) %>%
  group_by(`Area Code`, element) %>%
  summarise(total_value = sum(value)) %>%
  pivot_wider(names_from = element,
              values_from = total_value) %>%
  mutate(Production = Production / (1000 * 21),
         Emissions = Emissions / 21,
         intensity = Emissions/Production) %>% 
  na.omit()

# emission intensity by product
emission_intensity_by_product <- dt %>% 
  filter(Element %in% c("Emissions (CO2eq) (AR5)", "Production")) %>% 
  select(Item, Element, as.character(years)) %>% 
  pivot_wider(names_from = Element,
              values_from = as.character(years)) %>% 
  pivot_longer(cols = -Item,
               names_to = "year_element",
               values_to = "value") %>% 
  mutate(value = sapply(value, na.omit)) %>% 
  mutate(value = sapply(value, sum),
         element = gsub(".*_(Production|Emissions).*", "\\1", year_element)) %>%
  mutate(Item = case_when(grepl("^Raw milk (.+)$", Item) ~ "Milk",
                   TRUE ~ Item)) %>% 
  group_by(Item, element) %>% 
  summarise(total_value = sum(value)) %>%
  pivot_wider(names_from = element,
              values_from = total_value) %>% 
  mutate(Production = Production / (1000 * 21),
         Emissions = Emissions / 21,
         intensity = Emissions/Production) %>% 
  mutate(Item = sub(", fresh.*", "", Item)) %>% 
  mutate(Item = case_when(Item == "Hen eggs in shell" ~ "Eggs",
                          Item == "Meat of cattle with the bone" ~ "Beef",
                          Item == "Meat of pig with the bone" ~ "Pork",
                          Item == "Meat of chickens" ~ "Chicken",
                          TRUE ~ Item))

# distribution
country_groups <- readr::read_csv("Environment_Emissions_intensities_E_All_Data/FAOSTAT_data_11-9-2023.csv")
country_groups <- country_groups %>% 
  filter(`Country Group` %in% c("Africa", "Americas", "Asia", "Europe",
                                "Oceania")) %>% 
  select(`Country Group`, `Country Code`)

distr <- emission_intensity_by_country %>% 
  full_join(country_groups, join_by(`Area Code` == `Country Code`)) %>% 
  ungroup() %>% 
  select(`Country Group`, intensity) %>% 
  na.omit() %>% 
  mutate(`Country Group` = fct_reorder(`Country Group`, intensity)) %>% 
  ggplot(aes(x = `Country Group`, y = intensity)) +
  geom_violin(color = "orange", fill = "orange", alpha = 0.5) +
  geom_boxplot(width=0.3, color="grey30", fill = NA) +
  scale_y_log10(expand = c(0, 0), breaks = c(0, 0.3, 1, 3, 10, 30)) +
  scale_fill_brewer(type = 'qual', palette = 'Set3') +
  scale_color_brewer(type = 'qual', palette = 'Set3') +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(size = 18, family = "Trebuchet MS"),
        axis.text = element_text(size = 12, family = "Trebuchet MS"),
        axis.title = element_text(size = 14, family = "Trebuchet MS")) +
  labs(x = element_blank(),
       y = "intensity [log kg CO2eq/kg product]",
       title = "Distribution of emission intensity of countries in the 21st century") +
  coord_flip()

ggsave("distr.svg", plot = distr, height = 5*1.1, width = 7.7*1.1)

# map

# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'United States of America'] <- 'USA'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'Russian Federation'] <- 'Russia'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'Bolivia (Plurinational State of)'] <- 'Bolivia'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == ''] <- 'Turkey'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'Venezuela (Bolivarian Republic of)'] <- 'Venezuela'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'Iran (Islamic Republic of)'] <- 'Iran'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'Czechia'] <- 'Czech Republic'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'United Kingdom of Great Britain and Northern Ireland'] <- 'UK'
# emission_intensity_by_country$Area[emission_intensity_by_country$Area == 'United Republic of Tanzania'] <- 'Tanzania'
# emission_intensity_by_country <- emission_intensity_by_country %>% full_join(map_data("world"),join_by(Area==region))
#   
# map <- emission_intensity_by_country %>% 
#   filter(! Area %in% c('Guadeloupe', 'Faroe Islands', 'Tuvalu', 'Vanuatu',
#                        'Samoa', 'Micronesia (Federated States of)')) %>% 
#   ggplot(aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = intensity), color = "black") + 
#   scale_fill_gradient(name = "intensity [kg CO2eq/kg product]", low = "#FBFF00", high = "#FF0000", na.value = "gray80") +
#   labs(x = "Longitude", y = "Latitude") +
#   ggtitle("Greenhouse gas emissions intensity of global agriculture in the 21st century") +
#   coord_map(xlim = c(-180, 180), ylim = c(-50, 90)) +
#   theme_map()
# 
# ggsave("map.png", plot = map, height =  5, width = 7.7)

# product intensity bubble
  
bubble <- emission_intensity_by_product %>% 
  ggplot(aes(x = Production, y = Emissions, size = intensity,
             label = Item)) +
  geom_point(color = "orange", alpha = 0.5) +
  geom_text_repel(aes(size = 2.5),
                  family = "Trebuchet MS",
                  show.legend = F) +
  labs(x = "production [log kt]",
       y = "emissions [log kt]",
       color = "Product",
       title = "Average annual production of agricultural products 
and associated greenhouse gas emissions in the 21st century") +
  scale_x_log10(labels = scales::comma_format(scale = 1e-6, suffix = " M")) +
  scale_y_log10(labels = scales::comma_format(scale = 1e-6, suffix = " M")) +
  scale_size(range = c(2, 12), name="intensity
[kg CO2eq/kg product]") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, family = "Trebuchet MS"),
        axis.text = element_text(size = 12, family = "Trebuchet MS"),
        axis.title = element_text(size = 14, family = "Trebuchet MS"),
        legend.title = element_text(size = 14, family = "Trebuchet MS"),
        legend.text = element_text(size = 12, family = "Trebuchet MS"))

ggsave("bubble.svg", plot = bubble, height =  5.2, width = 7.7)

