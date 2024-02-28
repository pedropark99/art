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

build_grid_df <- function(values, n) {
  tibble(
    x = rep(seq_len(n), each = n),
    y = rep(seq_len(n), times = n),
    value = values |> as.vector()
  )
}

grid_scaled_values <- (values + 1) / max(values + 1)
binary_grid <- grid_scaled_values > 0.5

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

rectangles <- vector("list", (n / 2) ^ 2)
for (x in nrow(binary_grid)) {
  for (y in ncol(binary_grid)) {
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
    # fill <- switch (case,
    #   0 = NULL,
    #   351 = NULL,
    #   20 = list(x = c(a[1], d[1]), y = c(a[2], d[2])),
    #   12 = list(x = c(0, 0.5), y = c(0.5, 0)),
    #   96 = list(x = c(0.5, 1), y = c(0, 0.5)),
    #   223 = list(x = c(0.5, 1), y = c(1, 0.5)),
    #   # 20 + something cases
    #   32 = list(x = c(0.5, 0.5), y = c(0, 1)),
    #   116 = list(x = c(0.5, 1), y = c(1, 0.5)),
    #   128 = list(x = c(0.5, 1), y = c(1, 0.5)),
    # )
  }
}


c(20, 12, 96, 223) |> sum()

c(TRUE, FALSE) |> as.integer()

