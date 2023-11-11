library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(extrafont)

font_import()
loadfonts(device = "win")

stats <- read.csv("tour_cap_nat__custom_8097699_page_linear.csv")

stats %>% select(geo, OBS_VALUE) %>% 
  arrange(-OBS_VALUE) %>% 
  filter(!is.na(OBS_VALUE)) -> Bedplaces_2022_tmp

Bedplaces_2022_tmp %>% top_n(1) -> tmp

estimated_sum <- tmp$OBS_VALUE

Bedplaces_2022_tmp %>% 
  filter(OBS_VALUE != estimated_sum) %>% 
  mutate(percent = OBS_VALUE / estimated_sum * 100) -> Bedplaces_2022

Bedplaces_2022 %>% 
  summarise(all = sum(OBS_VALUE)) -> tmp2

sum_no_ireland <- tmp2$all

estimated_ireland <- estimated_sum - sum_no_ireland

Bedplaces_2022 %>% 
  filter(OBS_VALUE > 700000) %>% 
  summarise(SUM = sum(percent))

Bedplaces_2022 %>% 
  mutate(percent = round(percent, digits = 1),
         geo = toupper(geo),
         geo = fct_reorder(geo, -OBS_VALUE)) %>% 
  filter(OBS_VALUE > 600000) %>% 
  mutate(percent = paste0(as.character(percent), "%"))-> for_plot


ggplot(for_plot, aes(x = geo,
                     y = OBS_VALUE)) +
  geom_col(width = 0.7, fill = "#2644a8") +
  scale_x_discrete(guide = guide_axis(title = "",
                                      angle = 60)) +
  scale_y_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000),
                     labels = c(0,
                                format(1000000, big.mark = " ", scientific = FALSE),
                                format(2000000, big.mark = " ", scientific = FALSE),
                                format(3000000, big.mark = " ", scientific = FALSE),
                                format(4000000, big.mark = " ", scientific = FALSE),
                                format(5000000, big.mark = " ", scientific = FALSE),
                                format(6000000, big.mark = " ", scientific = FALSE)),
                     limits = c(0, 6500000),
                     expand = c(0,0),
                     guide = guide_axis(title = "")) +
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow"),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30,
                                  face = "bold"),
        plot.subtitle = element_text(size = 18),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed",
                                          colour = "darkgrey"),
        axis.line.x = element_line()) +
  labs(title = "EU countries with more than 2% of all bed places in tourist accommodation in 2022\ntotal: 28.9 million",
       subtitle = "(absolute numbers; % of total)") +
  geom_text(aes(label = percent),
            position = position_dodge(width = 0.5),
            vjust = - 0.5,
            family = "Arial Narrow",
            size = 8) 


