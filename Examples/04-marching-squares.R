library(tidyverse)
library(ambient)

n <- 100
grid_height <- n
grid_width <- n
cartesian <- coord_cartesian(
  xlim = c(0, grid_width),
  ylim = c(0, grid_height),
  expand = FALSE
)
set.seed(50)
values <- noise_simplex(c(n, n))
binary_grid <- values > 0

get_square <- function(x, y) {
  if ( (x + 1) > n || (y + 1) > n ) {
    return()
  }
  return(binary_grid[c(x, x + 1), c(y, y + 1)])
}

# a: isoline middle-left
# b: isoline middle-bottom
# c: isoline middle-right
# d: isoline middle-up
a <- c(0, 0.5)
b <- c(0.5, 0)
c <- c(1, 0.5)
d <- c(0.5, 1)

lines <- function(case) {
  cases <- list(
    list(x = 0, y = 0),
    list(x = c(a[1], d[1]), y = c(a[2], d[2])),
    list(x = c(a[1], b[1]), y = c(a[2], b[2])),
    list(x = c(d[1], b[1]), y = c(d[2], b[2])),
    list(x = c(d[1], c[1]), y = c(d[2], c[2])),
    list(x = c(a[1], c[1]), y = c(a[2], c[2])),
    list(x = 0, y = 0),
    list(x = c(b[1], c[1]), y = c(b[2], c[2])),
    list(x = c(b[1], c[1]), y = c(b[2], c[2])),
    list(x = c(b[1], c[1]), y = c(b[2], c[2])),
    list(x = c(a[1], c[1]), y = c(a[2], c[2])),
    list(x = c(d[1], c[1]), y = c(d[2], c[2])),
    list(x = c(b[1], d[1]), y = c(b[2], d[2])),
    list(x = c(c[1], d[1]), y = c(c[2], d[2])),
    list(x = c(c[1], b[1]), y = c(c[2], b[2])),
    list(x = 0, y = 0)
  )
  
  return(cases[[case + 1L]])
}




ls <- vector("list", n)
r_index <- 1L
for (x in seq_len(nrow(binary_grid))) {
  for (y in seq_len(ncol(binary_grid))) {
    s <- get_square(x, y)
    if (is.null(s)) {
      next
    }
    
    # 1st element: up-left
    # 2nd element: down-left
    # 3rd element: down-right
    # 4th element: up-right
    index <- as.vector(s)
    # Use binary counting to get the case:
    case <- c(1, 2, 4, 8)[index] |> sum()
    ls[[r_index]] <- list(
      rect_id = r_index,
      x_rect = x,
      y_rect = y,
      ls = lines(case)
    )
    r_index <- r_index + 1L
  }
}



grid_values <- tibble(ls) %>% 
  unnest_wider(c(ls)) %>% 
  unnest_wider(c(ls)) %>% 
  unnest_longer(c(x, y)) %>% 
  mutate(
    x = x + x_rect,
    y = y + y_rect
  )

grid_values %>% 
  ggplot() +
  geom_line(
    aes(x, y, group = rect_id)
  ) +
  coord_equal()





