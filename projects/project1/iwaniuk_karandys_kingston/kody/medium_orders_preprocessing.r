library(dplyr)

# IMPORTANT: Be sure to have project's root set to OnlineGroceryBasketAnalysis/

orders_raw <- read.csv("./data/raw-data/order_products__prior.csv")
products_raw <- read.csv("./data/raw-data/products.csv")
departments_raw <- read.csv("./data/raw-data/departments.csv")



products_departments <- inner_join(products_raw, departments_raw,
                                   by = "department_id",
                                   relationship = "many-to-one") %>%
  select(product_id, department_id)

products_raw <- NULL
departments_raw <- NULL

orders <- orders_raw %>%
  select(-reordered) %>%
  inner_join(products_departments, by = "product_id",
             relationship = "many-to-one")

orders_raw <- NULL
products_departments <- NULL


medium_carts <- orders %>%
  group_by(order_id) %>%
  summarise(cart_size = n(),
            distinct_departments = n_distinct(department_id)) %>%
  filter(distinct_departments > 5) %>%
  slice_max(order_by = cart_size, prop = 0.4)

medium_orders <- orders %>% 
  filter(order_id %in% medium_carts$order_id)

orders <- NULL

if (!file.exists("./data/processed-data/medium_orders.csv")) {
  write.csv(medium_orders, file = "./data/processed-data/medium_orders.csv", row.names = FALSE)
}

if (!file.exists("./data/processed-data/medium.carts.csv")) {
  write.csv(medium_carts, file = "./data/processed-data/medium.carts.csv", row.names = FALSE)
}
