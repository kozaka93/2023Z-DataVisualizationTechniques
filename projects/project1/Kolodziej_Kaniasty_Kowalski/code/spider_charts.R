library(dplyr)
library(fmsb)
library(RColorBrewer)
library(scales)

# Color palette
coul <- c("#470D21", "#D67D3E", "#361500")
colors_border <- coul
colors_in <- alpha(coul,0.3)

df <- read.csv("./data/merged_data_cleaned.csv")

characteristics <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness")

prepare_data <- function (df1, df2) {
  df1_vs_df2 <- rbind(df1, df2)
  df1_vs_df2 <- rbind(rep(6, length(df1_vs_df2)), df1_vs_df2)
  df1_vs_df2 <- rbind(rep(10, length(df1_vs_df2)), df1_vs_df2)
  df1_vs_df2 <- data.frame(df1_vs_df2)
  df1_vs_df2 <- df1_vs_df2[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

  return(df1_vs_df2)
}

average_variety <- df %>%
  select(Variety, characteristics) %>%
  filter(!(Variety %in% c("", "Other"))) %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_country <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(!(Country.of.Origin %in% c("", "Other"))) %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

# COMPARISION BY TOTAL CUP POINTS
average_coffee_below_70 <- df %>%
  filter(`Total.Cup.Points` < 70) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_over_90 <- df %>%
  filter(`Total.Cup.Points` >= 90) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_between_70_and_90 <- df %>%
  filter(`Total.Cup.Points` >= 70 & `Total.Cup.Points` < 90) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_70_vs_90 <- rbind(average_coffee_below_70, average_coffee_over_90, average_coffee_between_70_and_90)
average_coffee_70_vs_90 <- rbind(rep(3, length(average_coffee_70_vs_90)), average_coffee_70_vs_90)
average_coffee_70_vs_90 <- rbind(rep(10, length(average_coffee_70_vs_90)), average_coffee_70_vs_90)
average_coffee_70_vs_90 <- data.frame(average_coffee_70_vs_90)
average_coffee_70_vs_90 <- average_coffee_70_vs_90[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/average_coffee_quality_vs_rating.png", width = 1000, height = 1000, units = "px")
par(family = "Noto Sans Regular", font=1)
radarchart(average_coffee_70_vs_90,
           plwd=2, plty = 1, pfcol=colors_in, pcol=colors_border,
           centerzero = TRUE,
           title = "Coffee with <70 vs 70<...<90 vs >90 cup points",
           seg = 2, cglcol="black", cglty=2, axislabcol="black",
           vlcex=1, axistype = 4, caxislabels = c(6,8,10), calcex = 1.3)
legend(x=1.3, y=1, legend = c("below 70", "between 70 and 90", "above 90"), bty = "n", pch=20 , col=alpha(coul,0.5), text.col = "black", cex=1.2, pt.cex=3)
dev.off()

# ARABICA VS ROBUSTA
average_robusta <- df %>%
  filter(`Species` == "Robusta") %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_arabica <- df %>%
    filter(`Species` == "Arabica") %>%
    select(characteristics) %>%
    na.omit() %>%
    colMeans()

png(filename ="./plots/spider_charts/arabica_vs_robusta.png", width = 1000, height = 1000, units = "px")
radarchart(prepare_data(average_arabica, average_robusta),
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Arabica vs Robusta",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# PAPUA NEW GUINEA VS HAITI
average_haiti <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(Country.of.Origin == "Haiti") %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_papua_new_guinea <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(Country.of.Origin == "Papua New Guinea") %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

haiti_vs_papua_new_guinea <- rbind(average_haiti, average_papua_new_guinea)
haiti_vs_papua_new_guinea <- rbind(rep(6, length(haiti_vs_papua_new_guinea)), haiti_vs_papua_new_guinea)
haiti_vs_papua_new_guinea <- rbind(rep(10, length(haiti_vs_papua_new_guinea)), haiti_vs_papua_new_guinea)
haiti_vs_papua_new_guinea <- haiti_vs_papua_new_guinea %>% select(-Country.of.Origin)
haiti_vs_papua_new_guinea <- haiti_vs_papua_new_guinea[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/haiti_vs_papua_new_guinea.png", width = 1000, height = 1000, units = "px")
radarchart(haiti_vs_papua_new_guinea,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "avarege coffe from Haiti vs Papua New Guinea",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# HAWAIIAN KONA VS ETHIOPIAN YIRGACHEFFE
average_hawaiian_kona <- df %>%
  select(Variety, characteristics) %>%
  filter(Variety == "Hawaiian Kona") %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_ethiopian_yirgacheffe <- df %>%
  select(Variety, characteristics) %>%
  filter(Variety == "Ethiopian Yirgacheffe") %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

ethiopian_vs_kona <- rbind(average_ethiopian_yirgacheffe, average_hawaiian_kona)
ethiopian_vs_kona <- rbind(rep(6, length(ethiopian_vs_kona)), ethiopian_vs_kona)
ethiopian_vs_kona <- rbind(rep(10, length(ethiopian_vs_kona)), ethiopian_vs_kona)
ethiopian_vs_kona <- ethiopian_vs_kona %>% select(-Variety)
ethiopian_vs_kona <- ethiopian_vs_kona[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/yirgacheffe_vs_kona.png", width = 1000, height = 1000, units = "px")
radarchart(ethiopian_vs_kona,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Ethiopian Yirgacheffe vs Hawaiian Kona",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# Two random countries
random_country_spider <- function () {
    country1 <- sample(unique(df$Country.of.Origin), 1)
    country2 <- sample(unique(df$Country.of.Origin), 1)

    average_country1 <- df %>%
        select(Country.of.Origin, characteristics) %>%
        filter(Country.of.Origin == country1) %>%
        na.omit() %>%
        group_by(Country.of.Origin) %>%
        summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
        na.omit()

    average_country2 <- df %>%
        select(Country.of.Origin, characteristics) %>%
        filter(Country.of.Origin == country2) %>%
        na.omit() %>%
        group_by(Country.of.Origin) %>%
        summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
        na.omit()

    country1_vs_country2 <- rbind(average_country1, average_country2)
    country1_vs_country2 <- rbind(rep(6, length(country1_vs_country2)), country1_vs_country2)
    country1_vs_country2 <- rbind(rep(10, length(country1_vs_country2)), country1_vs_country2)
    country1_vs_country2 <- country1_vs_country2 %>% select(-Country.of.Origin)
    country1_vs_country2 <- country1_vs_country2[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

    radarchart(country1_vs_country2,
                 pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
                 centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
                 title = paste(country1, "vs", country2),
                 seg = 1, cglcol="black", cglty=1, axislabcol="grey",
                 vlcex=1.3)
}

random_country_spider()

# Two random varieties
random_variety_spider <- function () {
  var1 <- sample(unique(df$Variety), 1)
  var2 <- sample(unique(df$Variety), 1)

  average_var1 <- df %>%
    select(Variety, characteristics) %>%
    filter(Variety == var1) %>%
    na.omit() %>%
    group_by(Variety) %>%
    summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
    na.omit()

  average_var2 <- df %>%
      select(Variety, characteristics) %>%
      filter(Variety == var2) %>%
      na.omit() %>%
      group_by(Variety) %>%
      summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
      na.omit()

  var1_vs_var2 <- rbind(average_var1, average_var2)
  var1_vs_var2 <- rbind(rep(6, length(var1_vs_var2)), var1_vs_var2)
  var1_vs_var2 <- rbind(rep(10, length(var1_vs_var2)), var1_vs_var2)
  var1_vs_var2 <- var1_vs_var2 %>% select(-Variety)
  var1_vs_var2 <- var1_vs_var2[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

  radarchart(var1_vs_var2,
             pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
             centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
             title = paste(var1, "vs", var2),
             seg = 1, cglcol="black", cglty=1, axislabcol="grey",
             vlcex=1.3)
}

random_variety_spider()

# BELOW 1KM AND OVER 2KM
waverage_coffee_below_1000m <- df %>%
  filter(Altitude < 400) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_over_2000m <- df %>%
    filter(Altitude >= 1000) %>%
    select(characteristics) %>%
    na.omit() %>%
    colMeans()

png(filename ="./plots/spider_charts/average_coffee_1000m_vs_2000m.png", width = 1000, height = 1000, units = "px")
radarchart(prepare_data(waverage_coffee_below_1000m, average_coffee_over_2000m),
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Coffee below 1000m vs over 2000m",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3, paxislabels = seq(0, 10, 2))
dev.off()



