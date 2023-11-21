library(tidyr)
library(dplyr)
library(ggplot2)
library(stringi)
library(forcats)
library(extrafont)
library(patchwork)
library(Cairo)

loadfonts(device = "win")



Cairo::CairoPNG("final.png", width = 1920, height = 1080, bg = "transparent")


deforestarion <- read.csv("Total forest area replaced by oil palm globally (directly and indirectly), 2001-2015.csv")

deforestarion %>% 
  mutate(sum = (Nonprimary.forest + Primary.forest) / 1000) %>% 
  arrange(-Year) %>%
  mutate(Year = as.character(Year)) -> first_graph

first_graph$Year <- factor(first_graph$Year, levels = unique(first_graph$Year))


ggplot(first_graph, aes(x = Year,
                  y = sum)) +
  geom_col(fill = "#b30000", color = "#b30000") +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5),
                     limits = c(0, 11.2),
                     expand = c(0,0),
                     guide = guide_axis(angle = 0,
                                        title = ""),
                     position = "right") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "#606060", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "#606060", linewidth = 1),
    axis.text = element_text(color = "white"),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    text = element_text(color = "white")) +
  coord_flip() -> first


####################################################################################################



deforestarion %>% 
  mutate(sum = Nonprimary.forest + Primary.forest) %>% 
  summarise(sum = sum(sum) / 1000) -> second_graph

second_graph$Year = "Total"

second_graph %>% 
  add_row(Year = "Area of\nHungary", sum = 9.303) -> second_graph

second_graph$Year <- factor(second_graph$Year, levels = unique(second_graph$Year))

second_graph$Highlight <- second_graph$Year == "Area of\nHungary"

  

ggplot(second_graph, aes(x = Year,
                  y = sum,
                  fill = Highlight)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "#b30000", "TRUE" = "#8EA604")) +
  scale_y_continuous(limits = c(0, 11.2),
                     expand = c(0,0),
                     position = "right") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 30),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.line.y = element_line(color = "#606060", linewidth = 1),
    axis.line.x = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_blank(),
    text = element_text(color = "white")) +
  geom_text(aes(label = sum),
            position = position_dodge(width = 0.5),
            family = "Arial Narrow",
            hjust = -0.1,
            size = 12,
            color = "white") +
  coord_flip() -> second



#######################################################################################


(first / second) + plot_layout(heights = c(17, 3)) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_blank(),
    panel.spacing = unit(0, "lines")))

dev.off()
