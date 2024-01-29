library(dplyr)
library(igraph)
library(readr)
library(tidyr)
library(viridis)



# Wzajemne dawanie sobie punktów w finale Eurowizji 2016

data <- read_csv("C:/Users/micha/OneDrive/Desktop/Michal studia-LAPTOP-6IKBDHP7/Techniki wizualizacji danych/Laboratoria/KuklaMichal/2016_televote_results.csv")

data <- data %>%    # Bez "Televoting score"
  select("Contestant", "Belgium", "Czech Republic", "Netherlands", "Azerbaijan", "Hungary", "Italy", "Israel",
         "Bulgaria", "Sweden", "Germany", "France", "Poland", "Australia", "Cyprus", "Serbia", "Lithuania", "Croatia", "Russia",
         "Spain", "Latvia", "Ukraine", "Malta", "Georgia", "Austria", "United Kingdom", "Armenia")

data_long <- data %>%
  pivot_longer(cols = -Contestant, names_to = "Giver", values_to = "Weight") %>%
  mutate(Getter = Contestant) %>% 
  select(Giver, Getter, Weight) %>% 
  filter(Giver != Getter) %>%
  mutate(Weight = ifelse(is.na(Weight), 0, Weight))

result_data <- data_long %>%
  group_by(Country_one = pmin(Giver, Getter), Country_two = pmax(Giver, Getter)) %>%
  summarize(Weight = sum(Weight))



# result_data to gotowa ramka, z której zrobię wykres



graph <- graph_from_data_frame(result_data, directed = FALSE)

V(graph)$label <- V(graph)$name
edge_colors <- scales::col_numeric(viridis(100, option = "mako"), domain = c(min(result_data$Weight), max(result_data$Weight)), reverse = TRUE)
edge_widths <- result_data$Weight * 0.35
E(graph)$color <- edge_colors(result_data$Weight)
E(graph)$width <- edge_widths
layout_on_circle <- layout.circle(graph)
graph <- delete_edges(graph, E(graph)[result_data$Weight == 0])

plot(graph, layout = layout_on_circle, edge.width = E(graph)$width, 
     vertex.size = 20, vertex.label.cex = 0.6, edge.label = NA,
     main = "Kraje przyznające sobie nawzajem punkty telewidzów w finale Eurowizji 2016")

mtext("Im ciemniejszy kolor i grubsza krawędź, tym więcej punktów dane kraje przydzieliły sobie.",
      side = 1, line = -1, adj = 0.5, cex = 0.7)

