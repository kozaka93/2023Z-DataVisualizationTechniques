library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)


# Ramki danych są zescrappowane ze zdjecia wykresu
df <- read.csv("./Data/Df2.csv", header = FALSE, sep = ";")
df <- df %>% mutate(V1 = str_replace(V1, pattern = ",", replacement = "."), V2 = str_replace(V2, pattern = ",", replacement = "."))
df <- df %>% mutate(X = as.numeric(V1), Y = as.numeric(V2))

df_doPKB <- read.csv("./Data/PKB.csv", header = FALSE, sep = ";")
df_doPKB <- df_doPKB %>% mutate(V1 = str_replace(V1, pattern = ",", replacement = "."), V2 = str_replace(V2, pattern = ",", replacement = "."))
df_doPKB <- df_doPKB %>% mutate(X = as.numeric(V1), Y = as.numeric(V2))
# Dodaje puste wiersze, by dlugosci sie zgadzaly
rows <- nrow(df_doPKB)
df_doPKB[(rows+1):(rows+12),] <- NA
df["XPKB"] = df_doPKB["X"]
df["YPKB"] = df_doPKB["Y"]

skala <- 0.1
my_colors <- c("red", "blue")
ggplot(df) +
  geom_line(aes(X, Y, color = "Mld zł"), linewidth = 1) +
  geom_line(aes(XPKB, YPKB/skala, color = "% PKB"), linewidth = 1) +
  scale_x_continuous(breaks = seq(2008, 2023, by = 2)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~.*skala, name = "% PKB"),
    limits = c(0, 50)
    ) +
  labs(
    title = "Wydatki na obsługę długu publicznego w Polsce",
    y = "mld zł",
    x = "Rok",
    colour = "Odmierzane w"
  ) +
  theme_bw() +
  scale_color_manual(values = my_colors) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(color = my_colors[2]),
    axis.text.y.right = element_text(color = my_colors[1]),
    axis.title.y = element_text(color = my_colors[2]),
    axis.title.y.right = element_text(color = my_colors[1])
    )
