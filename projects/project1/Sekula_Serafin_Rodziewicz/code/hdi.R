### title (HDI and Foodwaste)

### libraries and packages 
library(dplyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)
### source data 
DATA <- read_excel('HDIFOODWASTEGDP.xlsx')
### data frames 
col_names <- colnames(DATA)
DATA<- rbind(col_names, DATA)
names(DATA)<-c("countries1","foodwaste","countries2","HDI","countries3","GDP")
df1 <- data.frame(DATA$countries1,DATA$foodwaste)
names(df1) <- c("countries","foodwaste")
df2<-data.frame(DATA$countries2,DATA$HDI)
names(df2) <- c("countries","HDI")
df <- full_join(df1,df2,by='countries')
df[215,3]=0.804 #this data is from english wikipedia, it is HDI for couple of countries
df[194,3]=0.892
df[188,3]=0.901
df[187,3]=0.774
df[60,3]=0.838
df[7,3]=0.549
df[12,3]=0.479
df[11,3]=0.577
df[129,3]=0.703
df<-mutate(df,foodwaste=as.numeric(foodwaste),HDI=as.numeric(HDI))
df_clean <- na.omit(df)
df_clean$HDI_category <- cut(df_clean$HDI, breaks=c(0.4, 0.6, 0.8, 1), 
                             labels=c("0.4-0.6", "0.6-0.8", "0.8-1"), 
                             include.lowest=TRUE)
category_colors <- c("0.4-0.6" = "#338843", 
                     "0.6-0.8" = "tomato2", "0.8-1" = "gold1")

total_countries <- nrow(df_clean)

### plot

p <- ggplot(df_clean, aes(y=HDI_category, x=foodwaste, fill=HDI_category)) +
  geom_violin(scale = "area", width=1, adjust=1.5, outlier.shape = NA) +
  geom_boxplot(width=0.18, color="black", fill="grey60" , alpha=1, outlier.shape = NA) +
  scale_fill_manual(values = category_colors) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14, angle=0, hjust=0.5),
    axis.line = element_line(color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.minor = element_line(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "black", size = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  labs(
    title = "Foodwaste by HDI Category",
    x = "Foodwaste (kg)",
    y = "HDI Category",
    caption = paste("Total countries:", total_countries)
  ) +
  scale_y_discrete(limits = c("0.4-0.6", "0.6-0.8", "0.8-1"))

# Save the plot with a transparent background
ggsave("violin_plot.png", plot = p, bg = "transparent", width = 8, height = 6, dpi = 250)
