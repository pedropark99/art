library(tidyverse)
library(ambient)
library(ragg)


### APPROACH 1 ----------------
set.seed(10)
noise <- noise_perlin(c(100, 100))
ncols <- ncol(noise)
nrows <- nrow(noise)


dados <- tibble(
  col_id = rep(1:100, each = 100),
  row_id = rep.int(1:100, times = 100),
  noise = noise %>% as.vector()
)

width <- 4
height <- 0.8
dados <- dados %>% 
  mutate(
    rect_id = seq_len(nrow(dados)),
    x = col_id * 5,
    y = row_id * 5
  ) %>% 
  mutate(
    x = map(x, function(x) as.double(c(x, x + width, x + width, x)) ),
    y = map(y, function(y) as.double(c(y, y, y + height, y + height)) ),
  ) %>% 
  select(-col_id, -row_id) %>% 
  unnest(c(x, y)) %>%
  mutate(
    x = (x * cos(noise)) + (y * -1 * sin(noise)),
    y = (x * sin(noise)) + (y * cos(noise))
  ) 



pl <- dados %>% 
  ggplot() +
  geom_polygon(
    aes(x = x, y = y, group = rect_id)
  ) +
  theme_void()

print(pl)

agg_png("teste.png", width = 3000, height = 3000, res = 300)
print(pl)
dev.off()






### APPROACH 2 ----------------

width <- 10
height <- 0.8
rect_x_coords <- c(0, 0 + width, 0 + width, 0)
rect_y_coords <- c(0 + height, 0 + height, 0, 0)

dados <- tibble(
    col_id = rep(1:100, each = 100),
    row_id = rep.int(1:100, times = 100),
    noise = noise %>% as.vector(),
    x = map(seq_len(100*100), function(x) rect_x_coords),
    y = map(seq_len(100*100), function(y) rect_y_coords)
  ) %>% 
  unnest(c(x, y))


dados <- dados %>% 
  mutate(
    noise = 4 * noise, ## Changing the factor affects the end plot
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
    aes(x = x, y = y, group = rect_id)
  ) +
  theme_void()

print(pl)
