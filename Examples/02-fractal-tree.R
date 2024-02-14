library(tidyverse)

deg2rad <- function(deg){
  return(deg * pi / 180)
}

x_start <- 5
y_start <- 5
coords <- list(
  x = vector("double", 100),
  y = vector("double", 100)
)
coords$x[1] <- x_start
coords$y[1] <- y_start


tree <- function(coords, current_index, current_angle, current_distance){
  if (current_distance < 5) {
    return(coords)
  }

  current_x <- coords$x[current_index]
  current_y <- coords$y[current_index]
  rotate_factor <- deg2rad(15)
  new_distance <- current_distance - 15
  
  # The left side
  new_angle <- current_angle + rotate_factor
  new_x <- (current_distance * cos(new_angle)) + current_x
  new_y <- (current_distance * sin(new_angle)) + current_y
  
  current_index <- current_index + 1
  coords$x[current_index] <- new_x
  coords$y[current_index] <- new_y
  
  tree(coords, current_index, new_angle, new_distance)
}


t <- tree(coords, 1, deg2rad(90), 100)
df <- data.frame(
  x = t$x,
  y = t$y
)



df %>% 
  ggplot() +
  geom_point(aes(x, y))
