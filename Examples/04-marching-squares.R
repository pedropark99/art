library(tidyverse)
library(ambient)
library(S7)

n <- 100
grid_height <- n
grid_width <- n
set.seed(50)
values <- noise_simplex(c(n, n))
binary_grid <- values > 0

build_grid_df <- function(angles, n) {
  tibble(
    x = rep(seq_len(n), times = n),
    y = rep(seq_len(n), each = n),
    value = angles |> as.vector()
  )
}
grid_as_df <- build_grid_df(values, n)

get_cell <- function(x, y) {
  if ( (x + 1) > n || (y + 1) > n ) {
    return()
  }
  return(binary_grid[c(x, x + 1), c(y, y + 1)])
}

S7_polygon <- new_class("S7_polygon",
  properties = list(
    x = class_double,
    y = class_double
  )
)

# a: corner up-left
# b: corner up-right
# c: corner bottom-right
# d: corner bottom-left
a <- c(0, 1)
b <- c(1, 1)
c <- c(1, 0)
d <- c(0, 0)
# ab: midpoint up
# bc: midpoint right
# cd: midpoint down
# da: midpoint left
ab <- c(0.5, 1)
bc <- c(1, 0.5)
cd <- c(0.5, 0)
da <- c(0, 0.5)


cases <- list(
  S7_polygon(x = 0, y = 0),
  S7_polygon(x = c(d[1], cd[1], da[1]), y = c(d[2], cd[2], da[2])),
  S7_polygon(x = c(c[1], cd[1], bc[1]), y = c(c[2], cd[2], bc[2])),
  S7_polygon(x = c(d[1], c[1], bc[1], da[1]), y = c(d[2], c[2], bc[2], da[2])),
  S7_polygon(x = c(b[1], ab[1], bc[1]), y = c(b[2], ab[2], bc[2])),
  S7_polygon(x = c(d[1], da[1], ab[1], b[1], bc[1], cd[1]), y = c(d[2], da[2], ab[2], b[2], bc[2], cd[2])),
  S7_polygon(x = c(ab[1], b[1], c[1], cd[1]), y = c(ab[2], b[2], c[2], cd[2])),
  S7_polygon(x = c(da[1], d[1], c[1], b[1], ab[1]), y = c(da[2], d[2], c[2], b[2], ab[2])),
  S7_polygon(x = c(a[1], ab[1], da[1]), y = c(a[2], ab[2], da[2])),
  S7_polygon(x = c(a[1], ab[1], cd[1], d[1]), y = c(a[2], ab[2], cd[2], d[2])),
  S7_polygon(x = c(a[1], ab[1], bc[1], c[1], cd[1], da[1]), y = c(a[2], ab[2], bc[2], c[2], cd[2], da[2])),
  S7_polygon(x = c(a[1], ab[1], bc[1], c[1], d[1]), y = c(a[2], ab[2], bc[2], c[2], d[2])),
  S7_polygon(x = c(a[1], b[1], bc[1], da[1]), y = c(a[2], b[2], bc[2], da[2])),
  S7_polygon(x = c(a[1], b[1], bc[1], cd[1], d[1]), y = c(a[2], b[2], bc[2], cd[2], d[2])),
  S7_polygon(x = c(a[1], b[1], c[1], cd[1], da[1]), y = c(a[2], b[2], c[2], cd[2], da[2])),
  S7_polygon(x = c(a[1], b[1], c[1], d[1]), y = c(a[2], b[2], c[2], d[2]))
)

get_polygon <- function(case_index) {
  return(cases[[case_index + 1L]])
}



grid_x <- vector("integer", n ^ 2)
grid_y <- vector("integer", n ^ 2)
polygons <- vector("list", n ^ 2)
polygons_indexes <- vector("integer", n ^ 2)
p_index <- 1L
for (x in seq_len(nrow(binary_grid))) {
  for (y in seq_len(ncol(binary_grid))) {
    cell <- get_cell(x, y)
    if (is.null(cell)) {
      next
    }
    
    # 1st element: up-left
    # 2nd element: down-left
    # 3rd element: up-right
    # 4th element: down-right
    index <- as.vector(cell)
    # Use binary counting to get the case:
    case_index <- c(8, 1, 4, 2)[index] |> sum()
    polygons[[p_index]] <- get_polygon(case_index)
    polygons_indexes[p_index] <- p_index
    grid_x[p_index] <- x
    grid_y[p_index] <- y
    p_index <- p_index + 1L
  }
}

result <- tibble(
  polygons = polygons[polygons_indexes != 0L],
  p_index = polygons_indexes[polygons_indexes != 0L],
  grid_x = grid_x[grid_x != 0L],
  grid_y = grid_y[grid_y != 0L]
) %>%
  mutate(
    p_x = map2(polygons, grid_x, \(p, grid_x) p@x + grid_x),
    p_y = map2(polygons, grid_y, \(p, grid_y) p@y + grid_y)
  ) %>% 
  unnest_longer(c(p_x, p_y))

result



result %>% 
  ggplot() +
  geom_point(
    aes(x, y),
    data = grid_as_df %>% filter(value > 0)
  ) +
  geom_polygon(
    aes(x = p_x, y = p_y, group = p_index),
    fill = "blue"
  ) +
  theme_void()





