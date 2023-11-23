library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(countrycode)

data <- as.data.frame(read_excel("dane_lata.xlsx"))
colnames(data) <- as.character(data[1, ])
data <- data[-c(1),]

get_continent <- function(data) {
  iso_codes <- countrycode(data$Country, "country.name", "iso3c")
  continent_info <- countrycode(iso_codes, "iso3c", "continent")
  return(data %>% mutate(Continent = continent_info))
}

data <- data %>%
  select(c("Country", "2000", "2008", "2015", "2023")) %>%
  mutate(`2023` = ifelse(`2023` == "35–49.9*", 45, `2023`)) %>%
  mutate(
    `2000` = ifelse(`2000` == "<5", 0, as.numeric(`2000`)),
    `2008` = ifelse(`2008` == "<5", 0, as.numeric(`2008`)),
    `2015` = ifelse(`2015` == "<5", 0, as.numeric(`2015`)),
    `2023` = ifelse(`2023` == "<5", 0, as.numeric(`2023`))
  )

data <- get_continent(data) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "HungerIndex")

choosen <- c("Madagascar", "Somalia", "Uganda", "Egypt")

blue_red_palette <- c("#FF6666", "#FFB266", "#B266FF", "#99CCFF")
plot_lata <- ggplot(plot_data, aes(
  x = as.factor(`Year`),
  y = `HungerIndex`,
  fill = `Country`
)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hunger Index Trends in Africa", x = "", y = "Hunger Index Value") +
  scale_fill_manual(values = blue_red_palette) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 14),
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(title = NULL))

ggsave("plot_lata.png",
       plot = plot_lata,
       width = 7,
       height = 5)
