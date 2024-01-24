generate_wordcloud <- function(words, freq, num_words) {
  
  wordcloud2(
    data.frame(word = words[1:num_words], n = freq[1:num_words]),
    color = brewer.pal(8, "Dark2"),
    minSize = 0.1,
    shape = "star",
    gridSize = 20,
    backgroundColor = "white",
    minRotation = -pi/2,
    maxRotation = pi/2
  )
}