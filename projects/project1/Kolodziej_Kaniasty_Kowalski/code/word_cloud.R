library(wordcloud2)
library(RColorBrewer)
library(tm)
library(dplyr)

df <- read.csv("data/coffee_reviews_dataset/coffee_analysis.csv", stringsAsFactors = FALSE)
reviews <- df$desc_1
docs <- Corpus(VectorSource(reviews))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
words_df <- data.frame(word = names(words),freq=words)

my_palette <- function(n) {
  orig_colors <- brewer.pal(9, "YlOrBr")
  orig_colors <- orig_colors[-c(1, 2)]
  new_colors <- col2rgb(orig_colors) * 0.8
  new_colors <- rgb(new_colors[1, ] / 255, new_colors[2, ] / 255, new_colors[3, ] / 255)
  colorRampPalette(new_colors)(n)
}

palette1 <- my_palette(5)
palette2 <- c("#CD7F32", "#A67B5B", "#836953", "#966919", "#704214")
palette3 <- c("#3e1e04", "#6a3005", "#965015", "#c4923e", "#cbac85")
palette4 <- brewer.pal(n = 5, name = "YlOrBr")

wordcloud2(words_df, size = 1, color = palette1, backgroundColor = "#EDE4D2", minRotation = pi/2, maxRotation = pi/2, rotateRatio = 0.2, fontFamily = "Noto Sans")
wordcloud2(words_df, size = 1, color = palette2, backgroundColor = "#EDE4D2", minRotation = pi/2, maxRotation = pi/2, rotateRatio = 0.2, fontFamily = "Noto Sans")
wordcloud2(words_df, size = 1.5, color = palette3, backgroundColor = "#EDE4D2", minRotation = pi/2, maxRotation = pi/2, rotateRatio = 0.2, fontFamily = "Noto Sans")
wordcloud2(words_df, size = 1.5, color = palette4, backgroundColor = "#EDE4D2", minRotation = pi/2, maxRotation = pi/2, rotateRatio = 0.2, fontFamily = "Noto Sans")
