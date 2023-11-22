library(ggplot2)
library(dplyr)
library(scales)
library(maps)

exports_calendar_year <- read.csv("./projekty/coffee/data/ICO_Coffee_Dataset/exports-calendar-year.csv")
imports <- read.csv("./projekty/coffee/data/ICO_Coffee_Dataset/imports.csv")

world_map <- map_data("world")

exporters <- exports_calendar_year %>%
  rowwise() %>%
  mutate(
    value = mean(c_across(X2010:X2018), na.rm = TRUE),
    exports = trimws(exports)
  ) %>%
  arrange(desc(value)) %>%
  select(exports, value) %>%
  rename(country = exports) %>%
  mutate(type = "exporter")

value_max <- max(exporters$value)
value_min <- min(exporters$value)
exporters <- exporters %>%
  mutate(normalized_value = (value - value_min) / (value_max - value_min))

importers <- imports %>%
  rowwise() %>%
  mutate(
    value = mean(c_across(X2010:X2018), na.rm = TRUE),
    imports = trimws(imports)
  ) %>%
  arrange(desc(value)) %>%
  select(imports, value) %>%
  rename(country = imports) %>%
  mutate(type = "importer")

value_max <- max(importers$value, na.rm = TRUE)
value_min <- min(importers$value, na.rm = TRUE)
importers <- importers %>%
  mutate(normalized_value = (value - value_min) / (value_max - value_min))


world_map_exporters <- world_map %>%
  left_join(exporters, by = c("region" = "country"))

world_map_importers <- world_map %>%
  left_join(importers, by = c("region" = "country"))

min_import_value <- min(importers$value)
max_import_value <- max(importers$value)

imp <- ggplot(data = world_map_importers) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = normalized_value), color = "transparent") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "#f2ead7", limits = c(0, 1)) +
  coord_fixed(1.1) +
  labs(fill = "Import Value") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "left"
  )


exp <- ggplot(data = world_map_exporters) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = normalized_value), color = "transparent") +
  scale_fill_gradient(low = "pink", high = "darkred", na.value = "#f2ead7", limits = c(0, 1)) +
  coord_fixed(1.1) +
  labs(fill = "Export Value") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "left"
  )


ggsave("./projekty/coffee/plots/predkie_ploty/world_map_importers.png", plot = imp, width = 20, height = 15, units = "in", dpi = 300, bg = "transparent")
ggsave("./projekty/coffee/plots/predkie_ploty/world_map_exporters.png", plot = exp, width = 20, height = 15, units = "in", dpi = 300, bg = "transparent")
