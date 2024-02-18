library(tidyverse)
library(ambient)

draw_curve <- function(start_position,
                       angles,
                       n_steps,
                       step_length) {
  
  x_coords <- vector("double", length = n_steps)
  y_coords <- vector("double", length = n_steps)
  
  x <- start_position[1]
  y <- start_position[2]
  for (i in seq_len(n_steps)) {
    column_index <- as.integer(x)
    row_index <- as.integer(y)
    
    if (off_boundaries(column_index, row_index)) {
      break
    }
    
    angle <- angles[row_index, column_index]
    x_step <- step_length * cos(angle)
    y_step <- step_length * sin(angle)
    x <- x + x_step
    y <- y + y_step
    
    x_coords[i] <- x
    y_coords[i] <- y
  }
  
  # This eliminates potential empty values
  # if the for loop ends before the number
  # of steps
  x_coords <- x_coords[seq_len(i - 1)]
  y_coords <- y_coords[seq_len(i - 1)]
  return(tibble(x = x_coords, y = y_coords))
}




n <- 50
set.seed(50)
angles1 <- noise_perlin(c(n, n)) * 2 * pi
set.seed(2314)
angles2 <- noise_perlin(c(n, n)) * 2 * pi

m <- 10000
set.seed(421)
xs <- as.integer(runif(m) * grid_width)
ys <- as.integer(runif(m) * grid_height)
starts <- map2(xs, ys, \(x, y) c(x, y))

curves1 <- map_dfr(
  .x = starts,
  .f= draw_curve,
  angles = angles1,
  n_steps = 10,
  step_length = step_length,
  .id = "line_id"
)


m <- 10000
set.seed(468)
xs <- as.integer(runif(m) * grid_width)
ys <- as.integer(runif(m) * grid_height)
starts <- map2(xs, ys, \(x, y) c(x, y))

curves2 <- map_dfr(
  .x = starts,
  .f= draw_curve,
  angles = angles2,
  n_steps = 10,
  step_length = step_length,
  .id = "line_id"
)
curves2$line_id <- as.character(as.integer(curves2$line_id) + m)



curves <- bind_rows(curves1, curves2)
line_ids <- unique(curves$line_id)
colors <- tibble(
  line_id = line_ids,
  r_offset = runif(length(line_ids)),
  color = sample(
    c("black", "white"),
    size = length(line_ids),
    replace = TRUE
  )
)


curves <- curves %>%
  left_join(colors) %>% 
  mutate(
    x = x + r_offset,
    y = y + r_offset
  )



ggplot(curves) +
  geom_path(
    aes(x, y, group = line_id, color = color)
  ) +
  theme_void() +
  scale_color_identity()


