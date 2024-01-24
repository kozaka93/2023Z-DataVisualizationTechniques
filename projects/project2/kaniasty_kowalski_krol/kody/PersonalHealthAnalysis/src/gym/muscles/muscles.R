library(png)
library(grid)
library(dplyr)
library(magick)


musclesPlot <- function(data){

total_reps <- data %>%
  group_by(muscle_group) %>%
  summarize(total_reps = sum(reps, na.rm = TRUE))

max_reps <- max(total_reps$total_reps)

colorize <- function(image, reps, max_reps) {
  intensity <- (reps / max_reps) * 100
  image <- image %>% image_modulate(brightness = intensity)
  return(image)
}

images <- list()

for (i in seq_len(nrow(total_reps))) {
  muscle <- total_reps$muscle_group[i]
  reps <- total_reps$total_reps[i]
  image_path <- paste0("./src/gym/muscles/images/", muscle, ".png")
  if (file.exists(image_path)) {
    image <- image_read(image_path)
    colored_image <- colorize(image, reps, max_reps)
    images[[length(images) + 1]] <- colored_image
  }
}

composite_image <- images[[1]]

for (i in 2:length(images)) {
  composite_image <- image_composite(composite_image, images[[i]], operator = "over")
}

other <- image_read(paste0("./src/gym/muscles/images/other.png"))
image_composite <- image_composite(other, composite_image, operator = 'over')
image_write(image_composite, "image_composite.png")
}