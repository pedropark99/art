library(tidyverse)
# A implementation of the chaos game that produces
# the Sierpiński triangle
initial_triangle <- tibble(
  x = c(10, 15, 5),
  y = c(10, 5, 5)
)

funs <- list(
  function(point){(point + c(10, 10)) / 2},
  function(point){(point + c(15, 5)) / 2},
  function(point){(point + c(5, 5)) / 2}
)

chaos_game <- function(n){
  choosen_functions <- sample(1:3, size = n, replace = TRUE)
  output_points <- vector("list", n)
  point <- c(8.5, 7.5)
  for(i in seq_len(n)){
    output_points[[i]] <- point
    fun <- funs[[ choosen_functions[i] ]]
    point <- fun(point)
  }
  
  return(output_points)
}

points <- chaos_game(5000)
df <- tibble(
  x = map_dbl(points, \(x) x[1]),
  y = map_dbl(points, \(y) y[2])
)

df %>% 
  ggplot() +
  geom_point(
    aes(x, y)
  ) +
  theme_void()


