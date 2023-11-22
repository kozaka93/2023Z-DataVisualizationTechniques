library(waffle)
library(ggplot2)
library(dplyr)
library(stringr)
library(hrbrthemes)
library(RColorBrewer)
library(ggthemr)
library(scales)
library(sysfonts)
library(showtextdb)
library(showtext)

# Replace the path with your own
# font_add(family = "FontAwesome", regular = "/Users/hubert/Library/Fonts/coffee icons.ttf")

# Read and clean the data from CQI
df <- read.csv("./data/merged_data_cleaned.csv", header = TRUE)
df <- df %>%
  filter(!is.na(Variety) & !is.na(Country.of.Origin) & Variety != "") %>%
  select(Species, Country.of.Origin, Farm.Name, Company, Region, Producer, 
         Variety, Processing.Method, Aroma, Flavor, Aftertaste, Acidity, Body, Balance, Total.Cup.Points, Color)

# Read and clean data from coffeereview.com
dfpricesfull <- read.csv("./data/coffee_scrapped.csv", header = TRUE)

# Read and clean data from coffeereview.com
dfprices <- read.csv("./data/coffee_analysis.csv", header = TRUE)


# Most common varieties - Typica, Bourbon, Caturra, Catuai, Mundo Novo, Kona (Hawaiian Kona), Gesha
# table(df$Variety) %>% sort(decreasing = TRUE)

# Note: varieties written above appear in the price comparison later on

# Note: Countries which are worth considering due to the quality of coffee produced in them
# Columbia, Guatemala, Costa Rica, Ethiopia, Jamaica, Brazil

# Waffle chart for Mexico, Colombia i Guatemala due to the thier importance in the coffee industry
# table(df$Country.of.Origin) %>% sort(decreasing = TRUE)

# MEEXICO:

df_Mex <- df %>%
  filter(Country.of.Origin == "Mexico")  %>% 
  select(Country.of.Origin, Variety)

df_Mex %>%
  group_by(Variety) %>%
  summarise(count = n()) 

df_Mex <- df_Mex %>%
  mutate(Variety = case_when(
    Variety == "Typica" ~ "Typica",
    Variety =="Bourbon" ~ "Bourbon",
    Variety =="Caturra" ~ "Caturra",
    Variety =="Mundo Novo" ~ "Mundo Novo",
    T ~ "Other"
  ))

df_Mex_count <- df_Mex %>%
  group_by(Variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(Variety = forcats::fct_reorder(Variety, n))


df_Mex_count %>%
  ggplot(aes(fill = Variety, values = n)) +
  geom_waffle(color = "white", size = 2, n_rows = 10, flip = TRUE, make_proportional = T, tile_shape = 'circle') +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 18)) +
  scale_fill_manual(
    name = NULL,
    values = c("#8C5100", "#01665E", "#DFC27D", "#2C3E50", "#4A2E0D"),
  ) +
  labs(title = "Variety of coffee produced in Mexico",
       caption = "Source: Coffee Quality Institute") -> plot_Mex

# With icons:

showtext_auto()

mex_for_waffle <- df_Mex_count %>%
  mutate(n = n / sum(df_Mex_count$n)) %>%
  mutate(
    remainder = n * 100 - floor(n * 100),
    floored = floor(n * 100)
  ) %>%
  arrange(desc(remainder)) %>%
  mutate(number = ifelse(100 - sum(floored) >= row_number(), floored + 1, floored)) %>%
  arrange(n)


plot_Mex2 <- expand.grid(x = 0:9,
                         y = 0:9) %>%
  rowwise() %>%
  mutate(index = 1 + sum(10 * y + x >= cumsum(mex_for_waffle$number)),
         col = mex_for_waffle$Variety[[index]]) %>%
  ggplot(mapping = aes(x, y, colour = forcats::fct_inorder(col))) +
  geom_text(aes(label = "\u007b"), family = "FontAwesome", size = 17) +
  scale_colour_manual(values =  c("#8C5100", "#01665E", "#DFC27D", "#2C3E50", "#4A2E0D")) +
  coord_equal() +
  theme_void() + 
  labs(color = '', title = "Varieties of coffee produced in Mexico") + 
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 25, margin = margin(0, 0, 0.5, 0, "cm")),
        text = element_text(size = 14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
  scale_y_reverse() + 
  scale_x_reverse()


plot_Mex2

# COLOMBIA:

df_Kol <- df %>%
  filter(Country.of.Origin == "Colombia")  %>% 
  select(Country.of.Origin, Variety)

df_Kol %>%
  group_by(Variety) %>%
  summarise(count = n()) 

df_Kol <- df_Kol %>%
  mutate(Variety = case_when(
    Variety == "Typica" ~ "Typica",
    Variety =="Caturra" ~ "Caturra",
    T ~ "Other"
  ))

df_Kol_count <- df_Kol %>%
  group_by(Variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(Variety = forcats::fct_reorder(Variety, n))

df_Kol_count %>%
  ggplot(aes(fill = Variety, values = n)) +
  geom_waffle(color = "white", size = 2, n_rows = 10, flip = TRUE, make_proportional = T, tile_shape = 'circle') +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 18)) +
  scale_fill_manual(
    name = NULL,
    values = c("#4A2E0D","#DFC27D",  "#01665E"),
  ) +
  labs(title = "Variety of coffee produced in Colombia",
       caption = "Source: Coffee Quality Institute") -> plot_Col

# With icons:

col_for_waffle <- df_Kol_count %>%
  mutate(n = n / sum(df_Kol_count$n)) %>%
  mutate(
    remainder = n * 100 - floor(n * 100),
    floored = floor(n * 100)
  ) %>%
  arrange(desc(remainder)) %>%
  mutate(number = ifelse(100 - sum(floored) >= row_number(), floored + 1, floored)) %>%
  arrange(n)

plot_Col2 <- expand.grid(x = 0:9,
                         y = 0:9) %>%
  rowwise() %>%
  mutate(index = 1 + sum(10 * y + x >= cumsum(col_for_waffle$number)),
         col = col_for_waffle$Variety[[index]]) %>%
  ggplot(mapping = aes(x, y, colour = forcats::fct_inorder(col))) +
  geom_text(aes(label = "\u007b"), family = "FontAwesome", size = 17) +
  scale_colour_manual(values = c("#4A2E0D","#DFC27D", "#01665E")) +
  coord_equal() +
  theme_void() + 
  labs(color = '', title = "Varieties of coffee produced in Colombia") + 
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 25, margin = margin(0, 0, 0.5, 0, "cm")),
        text = element_text(size = 14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
  scale_y_reverse() + 
  scale_x_reverse()

plot_Col2

# GUATEMALA

df_Gua <- df %>%
  filter(Country.of.Origin == "Guatemala")  %>% 
  select(Country.of.Origin, Variety)

df_Gua %>%
  group_by(Variety) %>%
  summarise(count = n()) 

df_Gua <- df_Gua %>%
  mutate(Variety = case_when(
    Variety =="Bourbon" ~ "Bourbon",
    Variety =="Caturra" ~ "Caturra",
    Variety =="Catuai" ~ "Catuai",
    T ~ "Other"
    ))

df_Gua_count <- df_Gua %>%
  group_by(Variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(Variety = forcats::fct_reorder(Variety, n))

plot_Gua <- df_Gua_count %>%
  ggplot(aes(fill = Variety, values = n)) +
  geom_waffle(color = "white", size = 2, n_rows = 10, flip = TRUE, make_proportional = T, tile_shape = 'circle') +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 18)) +
  scale_fill_manual(
    name = NULL,
    values = c("#7E2E2D","#DFC27D", "#01665E", "#2C3E50"),
  ) +
  labs(title = "Variety of coffee produced in Guatemala",
       caption = "Source: Coffee Quality Institute")

# With icons:

gua_for_waffle <- df_Gua_count %>%
  mutate(n = n / sum(df_Gua_count$n)) %>%
  mutate(
    remainder = n * 100 - floor(n * 100),
    floored = floor(n * 100)
  ) %>%
  arrange(desc(remainder)) %>%
  mutate(number = ifelse(100 - sum(floored) >= row_number(), floored + 1, floored)) %>%
  arrange(n)  # proportions mapped to percents that sum up to 100


plot_Gua2 <- expand.grid(x = 0:9,
                         y = 0:9) %>%
  rowwise() %>%
  mutate(index = 1 + sum(10 * y + x >= cumsum(gua_for_waffle$number)),
         col = gua_for_waffle$Variety[[index]]) %>%
  ggplot(mapping = aes(x, y, colour = forcats::fct_inorder(col))) +
  geom_text(aes(label = "\u007b"), family = "FontAwesome", size = 17) +
  scale_colour_manual(values = c("#7E2E2D","#DFC27D", "#01665E", "#2C3E50")) +
  coord_equal() +
  theme_void() + 
  labs(color = '', title = "Varieties of coffee produced in Guatemala") + 
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 25, margin = margin(0, 0, 0.5, 0, "cm")),
        text = element_text(size = 14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
  scale_y_reverse() + 
  scale_x_reverse()


plot_Gua2

# VIOLIN PLOT - price distribution for selected coffee varieties

# Data preparation

dfprices %>%
  mutate(Variety = case_when(
    (str_detect(name, "Typica") | str_detect(desc_1, "Typica") | str_detect(desc_2, "Typica") | str_detect(desc_3, "Typica")) ~ "Typica",
    (str_detect(name, "Bourbon") | str_detect(desc_1, "Bourbon") | str_detect(desc_2, "Bourbon") | str_detect(desc_3, "Bourbon")) ~ "Bourbon",
    (str_detect(name, "Caturra") | str_detect(desc_1, "Caturra") | str_detect(desc_2, "Caturra") | str_detect(desc_3, "Caturra")) ~ "Caturra",
    (str_detect(name, "Gesha") | str_detect(desc_1, "Gesha") | str_detect(desc_2, "Gesha") | str_detect(desc_3, "Gesha")) ~ "Gesha",
    (str_detect(name, "Kona") | str_detect(desc_1, "Kona") | str_detect(desc_2, "Kona") | str_detect(desc_3, "Kona")) ~ "Kona",
    T ~ "Other"
  )) -> dfprices_var

var_price <- dfprices_var %>%
  select(Variety, Price = `X100g_USD`) %>%
  mutate(Variety = forcats::fct_reorder(Variety, Price, .fun = median)) %>%
  filter(Price > 1 & Price < 100)

ggthemr::ggthemr("dust")

data_summary_median_sd <- function(x) {
  m <- median(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  if(ymin < 0) {
    ymin = 0
  }
  return(c(y=m,ymin=ymin,ymax=ymax))
}


var_price %>%
  group_by(Variety) %>%
  summarise(y = median(Price),
            sd = sd(Price)
  ) -> coord_for_annotate_violin

coord_for_annotate_violin %>%
  mutate(ymin = y-sd, ymax = y+sd) %>%
  mutate(ymin = ifelse(ymin < 1, 1, ymin)) -> coord_for_annotate_violin

coord_for_annotate_violin %>%
  mutate(y = unlist(y), ymin = unlist(ymin), ymax = unlist(ymax)) -> coord_for_annotate_violin

# Make plots

plot <- ggplot(var_price, aes(x = Variety, y = Price)) + 
  geom_violin(aes(fill = Variety), alpha = 0.9) +
  scale_colour_manual(values = c("darkred", "blue"),
                      name = "") +
  labs(title = "Price distribution for selected varieties of coffee",
       caption = "") +
  ylab("Price for 100g of coffee") +
  xlab("Variety") +
  guides(fill = F) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(0.5,0,0.5,0,"cm")),
    axis.title = element_text(size = 14, margin = margin(0.8,.8,.8,.8,"cm")),
    axis.text.x = element_text(size = 14, vjust = 0.5, angle = 30),
    axis.text.y = element_text(size = 14),
    plot.margin = margin(1,1,1,2,"cm"),
    plot.caption = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed"),
  ) + 
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks('log10', function(x) round(10^x, 2) ) )+
  scale_fill_manual(
    values = c("#01665E", "#DFC27D", "#2C3E50", "#4A2E0D", "chocolate4","bisque4"),
  )



plot2 <- ggplot(var_price, aes(x = Variety, y = Price)) + 
  geom_violin(aes(fill = Variety), alpha = 0.9) +
  scale_colour_manual(values = c("darkred"),
                      name = "") +
  labs(title = "Price distribution for selected varieties of coffee",
       caption = "") +
  ylab("Price for 100g of coffee") +
  xlab("Variety") +
  guides(fill = F) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(0.5,0,0.5,0,"cm")),
    axis.title = element_text(size = 14, margin = margin(0.8,.8,.8,.8,"cm")),
    axis.text.x = element_text(size = 14, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    plot.margin = margin(1,1,1,2,"cm"),
    plot.caption = element_text(size = 14),
    legend.text = element_text(size = 14),
  ) + 
  scale_y_log10(expand = c(0,0), limits = c(1, 100), breaks = c(1, 5, 7, 10, 20, 50, 80, 100)) +
  scale_fill_manual(
    values = c("#01665E", "#DFC27D", "#2C3E50", "#4A2E0D", "chocolate4","bisque4"),
  ) +
  geom_label(data = coord_for_annotate_violin, aes(x = Variety, y = y, label = y), fill = "azure", color = "black", fontface = "bold", label.size = 0.75)      
  
# Final violin plot:
plot2

