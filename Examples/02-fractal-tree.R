library(tidyverse)

deg2rad <- function(deg){
  return(deg * pi / 180)
}


trunk <- data.frame(
  x = c(0, 0),
  y = c(0, 150)
)

x_start <- 0
y_start <- 150
coord_index <- 1
coords <- list(
  x = vector("double", 1000),
  y = vector("double", 1000)
)


branch <- function(x, y, a, len) {
  if (len > 5) {
    new_len <- len * 0.67
    
    new_angle <- a + deg2rad(15)
    new_x <- (len * cos(new_angle)) + x
    new_y <- (len * sin(new_angle)) + y
    coords$x[coord_index] <<- new_x
    coords$y[coord_index] <<- new_y
    coord_index <<- coord_index + 1
    
    branch(new_x, new_y, new_angle, new_len)
    
    new_angle <- a - deg2rad(15)
    new_x <- (len * cos(new_angle)) + x
    new_y <- (len * sin(new_angle)) + y
    coords$x[coord_index] <<- new_x
    coords$y[coord_index] <<- new_y
    coord_index <<- coord_index + 1
    
    branch(new_x, new_y, new_angle, new_len)
  }

  return()
}




branch(0, 150, deg2rad(90), 150)
df <- as_tibble(coords) |>
  filter(x != 0)



df %>% 
  ggplot() +
  geom_point(aes(x, y))
