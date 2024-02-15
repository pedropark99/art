library(tidyverse)

# Function to convert degrees into radians
deg2rad <- function(deg){
  return(deg * pi / 180)
}


fractal_tree <- function(x_start, y_start, len_start, rotate_angle, ldf) {
  rotate_factor <- deg2rad(rotate_angle)
  length_decrease_factor <- ldf
  coord_index <- 1
  coords <- list(
    x = rep(NA_real_, 8000),
    y = rep(NA_real_, 8000)
  )
  
  branch <- function(x, y, a, len) {
    if (len > 5) {
      new_len <- len * length_decrease_factor
      
      new_angle <- a + rotate_factor
      new_x <- (len * cos(new_angle)) + x
      new_y <- (len * sin(new_angle)) + y
      
      i <- coord_index:(coord_index + 3)
      coords$x[i] <<- c(0 + x, new_x)
      coords$y[i] <<- c(0 + y, new_y)
      coord_index <<- coord_index + 4
      
      branch(new_x, new_y, new_angle, new_len)
      
      
      
      new_angle <- a - rotate_factor
      new_x <- (len * cos(new_angle)) + x
      new_y <- (len * sin(new_angle)) + y
      
      i <- coord_index:(coord_index + 3)
      coords$x[i] <<- c(0 + x, new_x)
      coords$y[i] <<- c(0 + y, new_y)
      coord_index <<- coord_index + 4
      
      branch(new_x, new_y, new_angle, new_len)
    }
  
    return()
  }
  
  branch(x_start, y_start, deg2rad(90), len_start * 0.67)
  trunk <- data.frame(
    x = c(x_start, x_start),
    y = c(y_start, y_start - len_start)
  )
  
  coords <- as_tibble(coords) %>%
    filter(!is.na(x)) %>% 
    bind_rows(trunk)
  
  coords <- coords %>% 
    mutate(
      line_id = rep(seq_len(nrow(coords) / 2), each = 2)
    )
    
  return(coords)
}

df <- fractal_tree(0, 150, 120, 15, 0.75)

df %>% 
  ggplot() +
  geom_path(aes(x, y, group = line_id)) +
  theme_void()
