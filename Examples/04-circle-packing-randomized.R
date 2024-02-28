# A Random-based Algorithm for Circle Packing
# Inspired by: https://tylerxhobbs.com/essays/2016/a-randomized-approach-to-cicle-packing

library(tidyverse)

distance <- function(x1, y1, x2, y2) {
  s1 <- (x2 - x1)^2
  s2 <- (y2 - y1)^2
  return(sqrt(s1 + s2))
}

circle_packing <- function(circles,
                           rect_width,
                           rect_height,
                           max_attempts,
                           seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  n <- nrow(circles)
  circles <- circles %>% 
    mutate(
      circle_id = seq_len(n),
      x = -50, # Pre-allocating space for the coordinates
      y = -50  # Pre-allocating space for the coordinates
    )
  
  for (current_circle in seq_len(n)) {
    random_x <- runif(max_attempts) * rect_width
    random_y <- runif(max_attempts) * rect_height
    other_circles <- circles %>%
      filter(x > 0, circle_id != current_circle)
    
    if (nrow(other_circles) == 0) {
      # No circles to compare, just place the current
      circles$x[current_circle] <- random_x[1L]
      circles$y[current_circle] <- random_y[1L]
      next
    }
    
    current_circle_r <- circles$radius[current_circle]
    k <- 1L
    while (k < max_attempts) {
      try_x <- random_x[k]
      try_y <- random_y[k]
      test <- other_circles %>% 
        mutate(
          dist = distance(x, y, try_x, try_y),
          not_overlap = dist > (current_circle_r + radius)
        )
      
      if (all(test$not_overlap)) {
        circles$x[current_circle] <- random_x[k]
        circles$y[current_circle] <- random_y[k]
        break
      }
      
      k <- k + 1
    }
  }
  
  circles <- circles %>% filter(x > 0)
  return(circles)
}


n <- 500
set.seed(50)
rs <- c(0.1, 0.15, 0.2, 0.3, 0.6, 0.8)
probs <- c(0.25, 0.25, 0.15, 0.25, 0.05, 0.05)
circles <- tibble(
  radius = sample(rs, size = n, prob = probs, replace = TRUE) * 0.5
)

circles <- circle_packing(
  circles,
  5, 5,
  20,
  42
)

set.seed(50)
pa <- c("#780000","#c1121f","#fdf0d5","#003049","#669bbc")
circles <- circles %>% 
  mutate(
    fill_color = sample(pa, size = nrow(circles), replace = TRUE)
  )


ggplot(circles) +
  ggforce::geom_circle(
    aes(
      x0 = x, y0 = y, r = radius,
      group = circle_id,
      fill = fill_color,
      color = fill_color
    )
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e5e5e5", color = "#e5e5e5")
  )
