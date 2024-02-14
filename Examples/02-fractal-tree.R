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
coords <- list(
  x = vector("double", 100),
  y = vector("double", 100)
)


branch <- function(x, y, a, len, coords, index) {
  if (len < 5) {
    return(coords)
  }
  
  rotate_factor <- deg2rad(15)
  new_len <- len - 15
  
  # The left side
  new_angle <- a + rotate_factor
  new_x <- (len * cos(new_angle)) + x
  new_y <- (len * sin(new_angle)) + y
  
  coords$x[index] <- new_x
  coords$y[index] <- new_y
  index <- index + 1
  
  branch(new_x, new_y, new_angle, new_len, coords, index)
  
  # The right side
  new_angle <- a - rotate_factor
  new_x <- (len * cos(new_angle)) + x
  new_y <- (len * sin(new_angle)) + y
  
  coords$x[index] <- new_x
  coords$y[index] <- new_y
  index <- index + 1
  
  branch(new_x, new_y, new_angle, new_len, coords, index)
}




t <- branch(x_start, y_start, deg2rad(90), 100, coords, 1)
df <- as_tibble(t) |>
  filter(x != 0)



df %>% 
  ggplot() +
  geom_point(aes(x, y))
