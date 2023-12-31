# Your color matrix (replace this with your actual matrix)
color_matrix <- matrix(c("#FF0000", "#00FF00", "#0000FF",
                         "#FFFF00", "#FF00FF", "#00FFFF"), ncol = 3, byrow=TRUE)

# Create a blank image with the same dimensions as the color matrix
image_width <- ncol(color_matrix)
image_height <- nrow(color_matrix)

# Set up the plotting area
plot(1, type = "n", xlim = c(0, image_width+1), ylim = c(0, image_height+1), xlab = "", ylab = "")

# Convert hex colors to RGB format
rgb_colors <- col2rgb(color_matrix)
a=1
# Loop through each pixel and draw a rectangle with the corresponding color
for (i in 1:image_width) {
  for (j in image_height:1) {
    rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, col = rgb(rgb_colors[1, a], rgb_colors[2, a], rgb_colors[3, a], maxColorValue = 255), border = NA)
    a=a+1
  }
}
