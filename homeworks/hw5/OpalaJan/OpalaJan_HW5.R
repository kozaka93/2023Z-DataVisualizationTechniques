install.packages("rgl")
library(rgl)
library(dplyr)
library(tibble)

generate_cone_dataframe <- function(lower, upper, size) {
  cone_df <- tibble::tibble()
  
  while (nrow(cone_df) < size) {
    z <- runif(1, lower, upper)
    x <- runif(1, -sqrt(z), sqrt(z))
    y <- runif(1, -sqrt(z), sqrt(z))
    
    condition <- (z - upper)^2 - x^2 - y^2 >= 0
    
    if (condition) {
      new_row <- tibble::tibble(x = x, y = y, z = z)
      cone_df <- dplyr::bind_rows(cone_df, new_row)
    }
  }
  
  return(cone_df)
}

generate_cylinder_dataframe <- function(lower, upper, size, radius) {
  cylinder_df <- tibble::tibble()
  
  while (nrow(cylinder_df) < size) {
    z <- runif(1, lower, upper)
    x <- runif(1, -sqrt(z), sqrt(z))
    y <- runif(1, -sqrt(z), sqrt(z))
    
    condition <- radius^2 - x^2 - y^2 >= 0
    
    if (condition) {
      new_row <- tibble::tibble(x = x, y = y, z = z)
      cylinder_df <- dplyr::bind_rows(cylinder_df, new_row)
    }
  }
  
  return(cylinder_df)
}

generate_sphere_dataframe <- function(lower, upper, size, radius) {
  sphere_df <- tibble::tibble()
  
  while (nrow(sphere_df) < size) {
    z <- runif(1, lower, upper)
    x <- runif(1, -sqrt(z), sqrt(z))
    y <- runif(1, -sqrt(z), sqrt(z))
    
    condition <- radius^2 - x^2 - y^2 - (z-(upper+lower)/2)^2 >= 0
    
    if (condition) {
      new_row <- tibble::tibble(x = x, y = y, z = z)
      sphere_df <- dplyr::bind_rows(sphere_df, new_row)
    }
  }
  
  return(sphere_df)
}



# funkcja zaczerpnięta ze strony:
# http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
get_colors <- function(groups, group.col){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

stozek1 <- generate_cone_dataframe(lower = 12, upper = 16, size = 200)
stozek2 <- generate_cone_dataframe(lower = 8, upper = 12, size = 200)
stozek3 <- generate_cone_dataframe(lower = 4, upper = 8, size = 200)

choinka <- bind_rows(stozek1, stozek2, stozek3)
choinka <- choinka %>% mutate(typ = "choinka")

pien <- generate_cylinder_dataframe(lower = 2, upper = 4, size = 100, radius = 1)
pien <- pien %>% mutate(typ = "pien")

bombkir1 <- generate_cylinder_dataframe(lower = 12, upper = 13, size = 5, radius = 4)
bombkir2 <- generate_cylinder_dataframe(lower = 8, upper = 9, size = 5, radius = 4)
bombkir3 <- generate_cylinder_dataframe(lower = 4, upper = 5, size = 5, radius = 4)
bombkir <- bind_rows(bombkir1, bombkir2, bombkir3)
bombkir <- bombkir %>% mutate(typ = "bombki czerwone")

bombkib1 <- generate_cylinder_dataframe(lower = 12, upper = 13, size = 5, radius = 4)
bombkib2 <- generate_cylinder_dataframe(lower = 8, upper = 9, size = 5, radius = 4)
bombkib3 <- generate_cylinder_dataframe(lower = 4, upper = 5, size = 5, radius = 4)
bombkib <- bind_rows(bombkib1, bombkib2, bombkib3)
bombkib <- bombkib %>% mutate(typ = "bombki niebieskie")

gwiazdka <- generate_sphere_dataframe(lower = 15, upper = 17, size = 10, radius = 1)
gwiazdka <- gwiazdka %>% mutate(typ = "gwiazdka")

kolory = c("red", "blue", "darkgreen", "yellow", "brown")

choinka <- bind_rows(choinka, pien, bombkir, bombkib, gwiazdka)

open3d()
spheres3d(choinka$x, choinka$y, choinka$z, r = 0.8, color = get_colors(choinka$typ, kolory))
movie3d(spin3d(axis = c(0, 0, 1)), duration = 3,
        dir = getwd())
