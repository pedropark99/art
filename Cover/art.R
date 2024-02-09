library(tidyverse)
pallete1 <- c(
  "#05450b",
  "#f6d5e5",
  "#450b20",
  "#361d06",
  "#f0eab9"
)


dados <- tibble(
  x = runif(100),
  y = runif(100),
  color = sample(pallete1, 100, replace = TRUE)
)


dados %>% 
  ggplot() +
  geom_point(
    aes(x, y, color = color)
  ) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = pallete1[1]))

