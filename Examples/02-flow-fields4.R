library(tidyverse)
library(ambient)

n <- 100
grid_height <- n
grid_width <- n
cartesian <- coord_cartesian(
  xlim = c(0, grid_width),
  ylim = c(0, grid_height)
)
set.seed(50)
angles <- noise_perlin(c(n, n)) * 4 * pi

pallete <- c(
  "#8ecae6",
  "#219ebc",
  "#023047",
  "#ffb703",
  "#fb8500"
)


# Drawing curves =========================
d_sep <- 0.5
get_density_col <- function(x){
  as.integer(x / d_sep) + 1L
}
get_density_row <- function(y){
  as.integer(y / d_sep) + 1L
}

distance <- function(x1, y1, x2, y2){
  s1 <- (x2 - x1)^2
  s2 <- (y2 - y1)^2
  return(sqrt(s1 + s2))
}

off_boundaries <- function(x, y, limit = grid_width){
  x <= 0 ||
    y <= 0 ||
    x >= limit ||
    y >= limit
}


n_curves <- 1500
set.seed(656)
xs <- runif(n_curves) * grid_width
set.seed(126)
ys <- runif(n_curves) * grid_height
starts <- map2(xs, ys, \(x,y) c(x, y))


n_steps <- 30
step_length <- 0.01 * grid_width
curves <- list(
  curve_id = vector("integer", length = n_steps * n_curves),
  dx = rep(-50, times = n_steps * n_curves),
  dy = rep(-50, times = n_steps * n_curves),
  x = rep(-50, times = n_steps * n_curves),
  y = rep(-50, times = n_steps * n_curves)
)

valid_seedpoint <- function(x, y, curve_id) {
  density_col <- get_density_col(x)
  density_row <- get_density_row(y)
  
  cols <- (density_col - 1L):(density_col + 1L)
  cols <- cols[cols > 0]
  rows <- (density_row - 1L):(density_row + 1L)
  rows <- rows[rows > 0]
  
  pos <- (
    curves$curve_id != curve_id
    & curves$dx %in% cols
    & curves$dy %in% rows
  )
  xs <- curves$x[pos]
  ys <- curves$y[pos]
  if (length(xs) == 0 || length(ys) == 0) {
    return(TRUE)
  }
  
  distances <- distance(x, y, xs, ys)
  if (any(distances <= d_sep)) {
    return(FALSE)
  }
  
  return(TRUE)
}


for (curve_id in seq_len(n_curves)) {
  start <- starts[[curve_id]]
  x <- start[1]
  y <- start[2]
  
  for (i in seq_len(n_steps)) {
    ff_column_index <- as.integer(x)
    ff_row_index <- as.integer(y)
    
    if (off_boundaries(ff_column_index, ff_row_index)) {
      break
    }
    
    angle <- angles[ff_row_index, ff_column_index]
    x_step <- step_length * cos(angle)
    y_step <- step_length * sin(angle)
    x <- x + x_step
    y <- y + y_step
    
    if (!valid_seedpoint(x, y, curve_id)) {
      break
    }
    
    curve_index <- (n_steps * (curve_id - 1)) + i
    curves$curve_id[curve_index] <- curve_id
    curves$x[curve_index] <- x
    curves$y[curve_index] <- y
    curves$dx[curve_index] <- get_density_col(x)
    curves$dy[curve_index] <- get_density_row(y)
  }
}



curves_df <- curves %>% 
  as_tibble() %>% 
  filter(x != -50)

curve_ids <- unique(curves_df$curve_id)
colors <- tibble(
  curve_id = curve_ids,
  color = sample(
    pallete, size = length(curve_ids), replace = TRUE
  )
)

curves_df <- curves_df %>% 
  left_join(colors)


pl <- curves_df %>%
  ggplot() +
  geom_path(
    aes(x, y, group = curve_id, color = color),
    linewidth = 1
  ) +
  cartesian +
  theme_void() +
  scale_color_identity() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fae9be")
  )


ragg::agg_png("flow_field.png", width = 2000, height = 1200, res = 200)
print(pl)
dev.off()
