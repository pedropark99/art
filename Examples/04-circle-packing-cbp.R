library(tidyverse)

n <- 100
set.seed(50)
rs <- c(0.05, 0.1, 0.15, 0.25, 0.6, 0.8)
probs <- c(0.25, 0.25, 0.15, 0.25, 0.05, 0.05)
circles <- sample(rs, size = n, prob = probs, replace = TRUE) * 0.5

rect_width = 5
rect_height = 5
circle_area <- function(r) {
  (r ^ 2) * pi
}
diameter <- function(r) {
  2 * r
}

circle_packing <- function(circles, strip_width) {
  circles <- sort(circles, decreasing = TRUE)
  
  lc <- list()
  lc[[1L]] <- circles[1L]
  circles <- circles[-1L]

  for (c in circles) {
    c_dia <- diameter(c)
    c_inserted <- FALSE
    for (i in seq_along(lc)) {
      l <- lc[[i]]
      l_total_dia <- sum(diameter(l))
      if ((l_total_dia + c_dia) < strip_width) {
        lc[[i]] <- c(l, c)
        c_inserted <- TRUE
        break
      }
    }
    
    if (!c_inserted) {
      lc[[length(lc) + 1L]] <- c
    }
  }
  
  return(lc)
}


packing <- circle_packing(circles, 2)
circles_df <- tibble(
    shelf_id = seq_along(packing),
    shelf_height = map_dbl(packing, \(x) diameter(max(x)) ),
    radius = packing
  ) %>% 
  mutate(cum_height = cumsum(shelf_height)) %>% 
  unnest(c(radius)) %>% 
  mutate(
    y = cum_height - radius,
    diameter = diameter(radius),
    circle_id = seq_along(radius)
  ) %>% 
  group_by(shelf_id) %>% 
  mutate(
    x = cumsum(diameter) - radius
  )


ggplot(circles_df) +
  ggforce::geom_circle(
    aes(x0 = x, y0 = y, r = radius, group = circle_id)
  )

