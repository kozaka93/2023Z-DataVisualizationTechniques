# Instalowanie pakietów --------------------------------------------------------
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("plotly")

# Wszytywanie bibliotek --------------------------------------------------------
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

# Wczytywanie danych danych ----------------------------------------------------
life_expectancy_df <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2_5994853.csv")
# Dane użyte na licencji:
# Creative Commons Attribution 4.0 (CC-BY 4.0)
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN?end=2021&start=1960&view=chart&year=1964

# Przygotowywanie danych -------------------------------------------------------
df1 <- life_expectancy_df %>%
  select(-one_of(c("Indicator.Code", "Indicator.Name", "X", "X2022")))
colnames(df1)[3:64] <- gsub("X", "", colnames(df1)[3:64])
df1 <- df1 %>%
  pivot_longer(
    cols = `1960`:`2021`,
    names_to = "year",
    values_to = "Life expectancy")
df1$hover <- with(df1, paste0("Life expectancy\nin ", Country.Name, "\nin year ", year, ":\n", round(`Life expectancy`, 2), " years"))

# Tworzenie wykresu ------------------------------------------------------------
plot_geo(
  data = df1,
  locationmode = "country codes",
  locations = ~Country.Code,
  frame = ~year
) %>%
  add_trace(
    z = ~`Life expectancy`,
    zmin = min(df1$`Life expectancy`, na.rm = TRUE),
    zmax = max(df1$`Life expectancy`, na.rm = TRUE),
    color = ~`Life expectancy`,
    colorscale = "Viridis",
    text = ~hover,
    hoverinfo = "text",
    marker = list(line = list(color = "grey", width = 0.6))
  ) %>% 
  layout(
    title = 'Life expectancy by country in 1960-2021\n(Hover for details)',
    font = list(family = "DM Sans"),
    geo = list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'),
      landcolor = 'lightgrey',
      showland = TRUE,
      showcountries = TRUE,
      countrycolor = 'grey')
  ) %>%
  style(
    hoverlabel = list(
    bgcolor = "#c9eeff",
    bordercolor = "black",
    font = list(
      family = "DM Sans",
      size = 15,
      color = "black"
    ))
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(family = "DM Sans", color="black"))
  ) %>%
  config(displayModeBar = FALSE) 