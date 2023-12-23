# załadowanie odpowiednich bibliotek
library(plotly)

# załadowanie danych ze strony https://www.kaggle.com/datasets/thedevastator/netflix-imdb-scores/
df <- read.csv('C:/Users/domin/Documents/Studia/TWD/hw4/Netflix TV Shows and Movies.csv')

# tworzenie wykresu
wykres <- plot_ly(data = df, x = ~release_year, y = ~imdb_score, type = "box") %>% 
  animation_opts(10) %>%
  layout(
    title = list(text = "Distribution of IMDb Scores by release year", size = 24),
    margin = list(t = 70, b = 80, r = 15, l =15),
    xaxis = list(title = "Year"),
    yaxis = list(title = "IMDb Score", automargin = TRUE),
    sliders = list(
      list(
        x = -0.02,
        active = 1,
        steps = lapply(seq(1952, 2012, by = 10), function(year) {
          list(
            label = paste0(as.character(year), "-2022"),
            method = "relayout",
            args = list("xaxis.range[0]", year, "xaxis.range[1]", year + 9)
          )
        })
        
      )
    )
  )

#wyświetlenie wykresu  
wykres

