library(ggpubr)
library(ggimage)
library(jpeg)

# Download and read sample image (readJPEG doesn't work with urls)
img <- readJPEG("src/graphics/background.jpeg")
url <- "src/graphics/background.jpeg"

plot <- ggplot(iris, aes(Species, Sepal.Length)) +
  # background_image(img) +
  geom_boxplot(aes(fill = Species))

plot <- ggbackground(plot, url)
print(plot)