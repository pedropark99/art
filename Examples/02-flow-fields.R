# Flow fields using classic Perlin noise and Simplex noise
##############################
library(ambient)
library(tidyverse)


set.seed(50)
r <- noise_perlin(c(100, 100)) * 2 * pi

df <- tibble(
  col_id = rep(seq_len(100), each = 100),
  row_id = rep(seq_len(100), times = 100),
  a = r %>% as.vector()
)

df <- df %>% 
  mutate(
    line_id = seq_len(nrow(df)),
    x = map(a, \(x) c(0, cos(x))),
    y = map(a, \(y) c(0, sin(y))),
  ) %>% 
  unnest(c(x, y)) %>% 
  mutate(
    x = x + col_id,
    y = y + row_id
  )


df %>% 
  ggplot() +
  geom_path(
    aes(x, y, group = line_id)
  )







set.seed(50)
r <- noise_simplex(c(100, 100)) * 2 * pi

df <- tibble(
  col_id = rep(seq_len(100), each = 100),
  row_id = rep(seq_len(100), times = 100),
  a = r %>% as.vector()
)

df <- df %>% 
  mutate(
    line_id = seq_len(nrow(df)),
    x = map(a, \(x) c(0, cos(x))),
    y = map(a, \(y) c(0, sin(y))),
  ) %>% 
  unnest(c(x, y)) %>% 
  mutate(
    x = x + col_id,
    y = y + row_id
  )


df %>% 
  ggplot() +
  geom_path(
    aes(x, y, group = line_id)
  )
