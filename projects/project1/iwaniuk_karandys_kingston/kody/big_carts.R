library(dplyr, warn = FALSE)
library(ggplot2, warn = FALSE)
library(forcats, warn = FALSE)
library(ggthemes, warn = FALSE)
library(ggdark, warn = FALSE)

orders <- read.csv("data/processed-data/medium_orders.csv")
cart_sizes <- read.csv("data/processed-data/medium.carts.csv")
departments <- read.csv("data/raw-data/departments.csv")

full_orders <- orders %>%
  inner_join(cart_sizes, by = "order_id", relationship = "many-to-one") %>%
  inner_join(departments, by = "department_id", relationship = "many-to-one") %>%
  select(-department_id, -distinct_departments) %>%
  mutate(add_to_cart_normalized = (add_to_cart_order-1) / (cart_size-1))



final_plot <- ggplot(filter(full_orders, cart_size > 80, department != "missing"),
       mapping = aes(x = fct_reorder(department,
                                     add_to_cart_normalized),
                     y = add_to_cart_normalized,
                     fill = TRUE,
                     color = TRUE)) +
         geom_boxplot() +
         labs(x = "Department",
              y = "Shopping stage",
              title = "Products placement in carts") +
         scale_y_continuous(
           expand = c(0, 0),
           limits = c(0, 1),
           breaks = seq(0, 1, by = 0.5),
           labels = c("Beggining", "Midway", "End")) +
         scale_fill_manual(values = "gold2") +
         scale_color_manual(values = "white") +
         dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
         theme(axis.text = element_text(size = 12),
               axis.title = element_text(size = 16),
               axis.text.x = element_text(angle = -45, hjust = 0),
               plot.title = element_text(family = "Fira Sans Condensed", size = 22, margin = margin(10, 0, 30, 0)),
               plot.background = element_rect(fill = "transparent"),
               panel.background = element_rect(fill = "transparent"),
               panel.grid.major = element_line(color = "#393E46", size = 0.2),
               panel.grid.minor = element_line(color = "#393E46", size = 0.2),
               legend.position = "none",
               legend.background = element_blank(),
               axis.ticks = element_blank())
final_plot
ggsave("final.png", bg = "transparent", width = 3000, height = 2000, units = "px")
       `