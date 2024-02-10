library(tidyverse)
library(ambient)
library(glue)

pallete1 <- c(
  "#05450b",
  "#f6d5e5",
  "#450b20",
  "#361d06",
  "#f0eab9"
)

palette2 <- c(
  "#263F30",
  "#EAD3D2",
  "#361d06",
  "#AAC0AA",
  "white",
#  "#83B692",
  "#450b20",
  "#67597A"
)

background <- theme_void() +
  theme(plot.background = element_rect(fill = palette2[1]))


display_field <- function(noises, rect_width = 10, rect_height = 0.8){
  nrows <- nrow(noises)
  ncols <- ncol(noises)
  width <- rect_width
  height <- rect_height
  rect_x_coords <- c(0, 0 + width, 0 + width, 0)
  rect_y_coords <- c(0 + height, 0 + height, 0, 0)
  
  field_coords <- tibble(
      col_id = rep(seq_len(nrows), each = nrows),
      row_id = rep.int(seq_len(ncols), times = ncols),
      noise = noise %>% as.vector(),
      x = map(seq_len(ncols * nrows), function(x) rect_x_coords),
      y = map(seq_len(ncols * nrows), function(y) rect_y_coords)
    ) %>% 
    unnest(c(x, y))
  
  rects <- field_coords %>% 
    mutate(
      noise = noise * pi,
      x = (x * cos(noise)) + (y * -1 * sin(noise)),
      y = (x * sin(noise)) + (y * cos(noise))
    ) %>% 
    mutate(
      x = (col_id * 5) + x,
      y = (row_id * 5) + y,
      rect_id = rep(seq_len(ncols * nrows), each = 4)
    )
  
  pl <- ggplot(rects) +
    geom_polygon(
      aes(x = x, y = y, group = rect_id)
    ) +
    theme_void()
  
  print(pl)
}








#### VARIÁVEIS DO GRID ------------------

n_steps <- 400
grid_height <- 500
grid_width <- 500
step_length <- 1
resolution <- as.integer(grid_width * 0.01)

set.seed(10)
noise <- noise_perlin(
  c(grid_width, grid_height)
)
noise_as_angle <- noise * pi

cartesian_grid <- coord_cartesian(
  xlim = c(0, grid_width),
  ylim = c(0, grid_height)
)


#### DESENHANDO A LINHA ----------------

draw_line <- function(start_position){
  # The x and y coordinates of the line.
  line_xs <- vector("double", length = n_steps)
  line_ys <- vector("double", length = n_steps)
  # Start position
  x_pos <- start_position[1]
  y_pos <- start_position[2]
  for (i in seq_len(n_steps)) {
    column_index <- as.integer(x_pos)
    row_index <- as.integer(y_pos)
    
    if (column_index <= 0
        || row_index <= 0
        || column_index >= grid_width
        || row_index >= grid_height) {
      break
    }
    grid_angle <- noise_as_angle[row_index, column_index]
    x_step <- step_length * cos(grid_angle)
    y_step <- step_length * sin(grid_angle)
    x_pos <- x_pos + x_step
    y_pos <- y_pos + y_step
    
    line_xs[i] <- x_pos
    line_ys[i] <- y_pos
  }
  
  line_xs <- line_xs[seq_len(i - 1)]
  line_ys <- line_ys[seq_len(i - 1)]
  
  return(tibble(
    step_id = seq_len(i - 1),
    x = line_xs,
    y = line_ys
  ))
}

set.seed(10)
n_lines <- 300
xs <- runif(n_lines, 50, 450)
ys <- runif(n_lines, 50, 450)
start_positions <- vector("list", length = n_lines)
for(i in seq_along(start_positions)){
  start_positions[[i]] <- c(xs[i], ys[i])
}

lines <- map_dfr(
  start_positions,
  function(x) draw_line(x),
  .id = "line_id"
) %>% 
  mutate(line_id = as.integer(line_id))

set.seed(10)
random_colors <- tibble(
  line_id = seq_len(n_lines),
  color = sample(palette2, n_lines, replace = TRUE)
)
lines <- lines %>% left_join(random_colors)


### To end the lines in random places: 
# set.seed(10)
# random_ends <- as.integer(runif(n_lines, 5, 50))
# random_colors <- sample(palette2, n_lines, replace = TRUE)
# lines <- lines %>% 
#   group_by(line_id) %>% 
#   summarise(max_step_id = max(step_id)) %>% 
#   mutate(
#     max_step_id = if_else(max_step_id > 50, max_step_id - random_ends, max_step_id),
#     color = random_colors,
#     step_id = map(max_step_id, function(x) seq_len(x))
#   ) %>%
#   unnest(c(step_id)) %>% 
#   left_join(lines, by = join_by(line_id, step_id))





pl <- lines %>% 
  ggplot() +
  geom_path(
    aes(x, y, group = line_id, color = color),
  ) +
  cartesian_grid +
  theme_void() +
  scale_color_identity()

print(pl)



ragg::agg_png("Cover/Figures/flow_field1.png",
        res = 500,
        width = 5000,
        height = 4200, background = NULL)
print(pl)
dev.off()














#### GRID PARA PADRÕES ABAIXO --------------------

set.seed(10)
noise <- noise_perlin(c(100, 100))
ncols <- ncol(noise)
nrows <- nrow(noise)

width <- 10
height <- 0.8
rect_x_coords <- c(0, 0 + width, 0 + width, 0)
rect_y_coords <- c(0 + height, 0 + height, 0, 0)

field_coords <- tibble(
  col_id = rep(1:100, each = 100),
  row_id = rep.int(1:100, times = 100),
  noise = noise %>% as.vector(),
  x = map(seq_len(100 * 100), function(x) rect_x_coords),
  y = map(seq_len(100 * 100), function(y) rect_y_coords)
) %>% 
  unnest(c(x, y))


#### PADRÃO 1 O MAIS MALUCO DE TODOS -------------------
### A ideia é aplicar uma operação de rotação 2 vezes,
### usando dois fatores distintos sobre o noise

dados <- field_coords %>% 
  mutate(
    noise = 2 * noise, ## Changing the factor affects the end plot
    x = (x * cos(noise)) + (y * -1 * sin(noise)),
    y = (x * sin(noise)) + (y * cos(noise))
  ) %>% 
  mutate(
    x = (col_id * 5) + x,
    y = (row_id * 5) + y,
    rect_id = rep(seq_len(100*100), each = 4)
  )


dados <- dados %>% 
  mutate(
    noise = 15 * noise, ## Changing the factor affects the end plot
    x = (x * cos(noise)) + (y * -1 * sin(noise)),
    y = (x * sin(noise)) + (y * cos(noise))
  ) %>% 
  mutate(
    x = (col_id * 5) + x,
    y = (row_id * 5) + y,
    rect_id = rep(seq_len(100*100), each = 4)
  )

dados %>% 
  ggplot() +
  geom_polygon(
    aes(x = x, y = y, group = rect_id),
    color = palette2[2]
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = palette2[1]))


#### PADRÃO 2 FLOW FIELD BONITO ------------------------


dados <- field_coords %>% 
  mutate(
    noise = 15 * noise, ## Changing the factor affects the end plot
    x = (x * cos(noise)) + (y * -1 * sin(noise)),
    y = (x * sin(noise)) + (y * cos(noise))
  ) %>% 
  mutate(
    x = (col_id * 5) + x,
    y = (row_id * 5) + y,
    rect_id = rep(seq_len(100*100), each = 4)
  )

pl <- dados %>% 
  ggplot() +
  geom_polygon(
    aes(x = x, y = y, group = rect_id),
    fill = "#30543f"
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = palette2[1]))

print(pl)



ragg::agg_png("Cover/Figures/flow_field_pattern1.png",
              res = 500,
              width = 4500,
              height = 4500)
print(pl)
dev.off()
