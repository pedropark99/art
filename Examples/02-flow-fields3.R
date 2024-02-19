library(tidyverse)
library(ambient)

n <- 50
grid_height <- n
grid_width <- n
cartesian <- coord_cartesian(
  xlim = c(0, grid_width),
  ylim = c(0, grid_height)
)
set.seed(50)
angles <- noise_perlin(c(n, n)) * 4 * pi



# Drawing curves =========================
d_sep <- 0.4
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

curves_df %>%
ggplot() +
  geom_path(aes(x, y, group = curve_id)) +
  cartesian +
  theme_void()











# =================================================
# Visualizing the flow field ======================
# =================================================


build_grid_df <- function(angles, n) {
  tibble(
    x = rep(seq_len(n), each = n),
    y = rep(seq_len(n), times = n),
    value = angles |> as.vector()
  )
}

visualize_field <- function(grid, n){
  # Calculate the n^2 lines
  grid <- grid %>% 
    mutate(
      line_id = seq_len(nrow(grid)),
      xend = cos(value),
      yend = sin(value),
    )
  
  # Spread the lines across the grid
  grid <- grid %>% 
    mutate(
      xend = xend + x,
      yend = yend + y
    )
  
  # Plot these lines
  u <- "inches"
  a <- arrow(length = unit(0.025, u))
  ggplot(grid) +
    geom_segment(
      aes(
        x = x, y = y,
        xend = xend,yend = yend,
        group = line_id
      ),
      arrow = a
    ) +
    coord_cartesian(
      xlim = c(0,n), ylim = c(0,n)
    ) +
    theme_void()
}


visualize_field(build_grid_df(angles, n), n)



# ===================================================
# Visualizing the density grid ======================
# ===================================================

d_sep <- 0.5
d_xs <- seq(0, grid_width, by = d_sep)
d_ys <- seq(0, grid_height, by = d_sep)
density_grid <- tibble(
  x = d_xs, y = d_ys
)

ggplot(density_grid) +
  geom_vline(aes(xintercept = x)) +
  geom_hline(aes(yintercept = y)) +
  ggtitle(
    label = "The density grid",
    subtitle = "Each cell in this grid represents a local cell with width and height\nequal to d_sep"
  ) +
  coord_cartesian(
    xlim = c(0, grid_width),
    ylim = c(0, grid_height),
    expand = FALSE
  )
  



