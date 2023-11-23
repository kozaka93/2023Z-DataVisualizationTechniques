library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

world_data <- data.frame(read.csv("Food_Security_Data_E_All_Data_(Normalized).csv"))
countries <- data.frame(read.csv(text = "Area,Continent
Afghanistan,Asia
Albania,Europe
Algeria,Africa
Andorra,Europe
Angola,Africa
Antigua and Barbuda,North America
Argentina,South America
Armenia,Asia
Australia,Australia
Austria,Europe
Azerbaijan,Asia
Bahamas,North America
Bahrain,Asia
Bangladesh,Asia
Barbados,North America
Belarus,Europe
Belgium,Europe
Belize,North America
Benin,Africa
Bhutan,Asia
Bolivia,South America
Bosnia and Herzegovina,Europe
Botswana,Africa
Brazil,South America
Brunei,Asia
Bulgaria,Europe
Burkina Faso,Africa
Burundi,Africa
Cambodia,Asia
Cameroon,Africa
Canada,North America
Cape Verde,Africa
Central African Republic,Africa
Chad,Africa
Chile,South America
China,Asia
Colombia,South America
Comoros,Africa
Congo (Brazzaville),Africa
Congo (Kinshasa),Africa
Costa Rica,North America
Croatia,Europe
Cuba,North America
Cyprus,Asia
Czech Republic,Europe
Denmark,Europe
Djibouti,Africa
Dominica,North America
Dominican Republic,North America
East Timor,Asia
Ecuador,South America
Egypt,Africa
El Salvador,North America
Equatorial Guinea,Africa
Eritrea,Africa
Estonia,Europe
Eswatini,Africa
Ethiopia,Africa
Fiji,Australia
Finland,Europe
France,Europe
Gabon,Africa
Gambia,Africa
Georgia,Asia
Germany,Europe
Ghana,Africa
Greece,Europe
Grenada,North America
Guatemala,North America
Guinea,Africa
Guinea-Bissau,Africa
Guyana,South America
Haiti,North America
Honduras,North America
Hungary,Europe
Iceland,Europe
India,Asia
Indonesia,Asia
Iran,Asia
Iraq,Asia
Ireland,Europe
Israel,Asia
Italy,Europe
Jamaica,North America
Japan,Asia
Jordan,Asia
Kazakhstan,Asia
Kenya,Africa
Kiribati,Australia
Korea, North,Asia
Korea, South,Asia
Kosovo,Europe
Kuwait,Asia
Kyrgyzstan,Asia
Laos,Asia
Latvia,Europe
Lebanon,Asia
Lesotho,Africa
Liberia,Africa
Libya,Africa
Liechtenstein,Europe
Lithuania,Europe
Luxembourg,Europe
Madagascar,Africa
Malawi,Africa
Malaysia,Asia
Maldives,Asia
Mali,Africa
Malta,Europe
Marshall Islands,Australia
Mauritania,Africa
Mauritius,Africa
Mexico,North America
Micronesia,Australia
Moldova,Europe
Monaco,Europe
Mongolia,Asia
Montenegro,Europe
Morocco,Africa
Mozambique,Africa
Myanmar,Asia
Namibia,Africa
Nauru,Australia
Nepal,Asia
Netherlands,Europe
New Zealand,Australia
Nicaragua,North America
Niger,Africa
Nigeria,Africa
North Macedonia,Europe
Norway,Europe
Oman,Asia
Pakistan,Asia
Palau,Australia
Palestine,Asia
Panama,North America
Papua New Guinea,Australia
Paraguay,South America
Peru,South America
Philippines,Asia
Poland,Europe
Portugal,Europe
Qatar,Asia
Romania,Europe
Russia,Asia
Rwanda,Africa
Saint Kitts and Nevis,North America
Saint Lucia,North America
Saint Vincent and the Grenadines,North America
Samoa,Australia
San Marino,Europe
Sao Tome and Principe,Africa
Saudi Arabia,Asia
Senegal,Africa
Serbia,Europe
Seychelles,Africa
Sierra Leone,Africa
Singapore,Asia
Slovakia,Europe
Slovenia,Europe
Solomon Islands,Australia
Somalia,Africa
South Africa,Africa
South Sudan,Africa
Spain,Europe
Sri Lanka,Asia
Sudan,Africa
Suriname,South America
Sweden,Europe
Switzerland,Europe
Syria,Asia
Taiwan,Asia
Tajikistan,Asia
Tanzania,Africa
Thailand,Asia
Togo,Africa
Tonga,Australia
Trinidad and Tobago,North America
Tunisia,Africa
Turkey,Asia
Turkmenistan,Asia
Tuvalu,Australia
Uganda,Africa
Ukraine,Europe
United Arab Emirates,Asia
United Kingdom,Europe
United States,North America
Uruguay,South America
Uzbekistan,Asia
Vanuatu,Australia
Vatican City,Europe
Venezuela,South America
Vietnam,Asia
Yemen,Asia
Zambia,Africa
Zimbabwe,Africa
"))


townrural <- world_data %>% 
  filter(Item == "Prevalence of severe food insecurity in the rural adult population (percent) (annual value)" |
           Item== "Prevalence of severe food insecurity in the town and semi-dense area adult population (percent) (annual value)"|
           Item ==  "Prevalence of severe food insecurity in the urban adult population (percent) (annual value)" 
  ) %>% 
  mutate(Item = case_when(Item=="Prevalence of severe food insecurity in the rural adult population (percent) (annual value)"~"Rural",
                          Item ==  "Prevalence of severe food insecurity in the town and semi-dense area adult population (percent) (annual value)"~"Town",
                          Item == "Prevalence of severe food insecurity in the urban adult population (percent) (annual value)"~"Urban")) %>% 
  filter(!is.na(Value),Value!="",Element=="Value",Area %in% c("Africa",
                                                              "Asia",
                                                              "Europe",
                                                              "Northern America",
                                                              "Oceania",
                                                              "South America")) %>% 
  mutate(Value = as.numeric(Value))

townrural$Area <- factor(townrural$Area,levels= c("Africa","South America",
                                                  "Asia","Oceania","Europe",
                                                  "Northern America"))



townrural %>% 
  ggplot(aes(x = Area, y = Value,fill=Item,color=Item))+
  geom_col(position="dodge")+
  scale_fill_manual(values = c("darkblue","seagreen","palegoldenrod"))+
  scale_color_manual(values =c("black","black","black"))+
  scale_y_continuous(breaks = seq(0, 30, 10),
                     labels = scales::number_format(suffix = "%"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.grid.major = element_line(
      color = "black",
      size = 0.5,
      linetype = "dotted"
    ),
    panel.grid.minor = element_line(
      color = "black",
      size = 0.5,
      linetype = "dotted"
    ),panel.background = element_rect(fill = "#afc1d0"),
    plot.background = element_rect(fill = "#afc1d0", color = NA), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )+
  labs(title= "Prevalence of undernourishment depending on the area",
       x= "Continent",
       y ="")+
  guides(fill=guide_legend(title="Area"),color = "none")



