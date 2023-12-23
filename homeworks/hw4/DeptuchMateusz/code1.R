library(dplyr)
library(plotly)

females <- read.csv("/Users/mateuszdeptuch/Desktop/females.csv", sep = ";")
females <- as.data.frame(apply(females, c(1, 2), function(x) ifelse(x == ":", NA, x)))
males <- read.csv("/Users/mateuszdeptuch/Desktop/males.csv", sep = ";")
males <- as.data.frame(apply(males, c(1, 2), function(x) ifelse(x == ":", NA, x)))

males <- males %>%
  mutate(sex = "male")
females <- females %>%
  mutate(sex = "female")
df <- rbind(males, females)
df[["Overweight"]] <- as.numeric(gsub(",", ".", df[["Overweight"]]))
df1 <- df
df <- df %>%
    arrange(desc(Overweight))%>%
  filter(!is.na(Overweight) & BMI..Labels. != "European Union")

df1 <- df1%>%
  arrange(desc(Overweight))%>%
  filter(!is.na(Overweight) & BMI..Labels. == "European Union")
df <- rbind(df1, df)

df$BMI..Labels. <- factor(df$BMI..Labels., levels = unique(df$BMI..Labels.))

p <- plot_ly(data = df,
             x = ~BMI..Labels.,
             y = ~Overweight,
             type = "bar",
             color = ~sex,
             legendgroup = ~sex,
             colors = c("pink", "lightblue"))
             
updatemenus <- list(
  list(
    buttons = list(
      list(method = "restyle",
           args = list("visible", list(TRUE,TRUE)),
           label = "Both"),
      list(method = "restyle",
           args = list("visible", list(FALSE, TRUE)),
           label = "Males"),
      list(method = "restyle",
           args = list("visible", list(TRUE, FALSE)),
           label = "Females")

    ),
    direction = "down",
    showactive = TRUE,
    x = 0.1,
    xanchor = "left",
    y = 1.15,
    yanchor = "top"
  )
)

p %>% layout(
  updatemenus = updatemenus,
  xaxis = list(title = "Country"),
  yaxis = list(title = "Percent of overweight population"),
  title = "Percentage of overweight population in EU countries in 2019"
)

