library(tidyverse)
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


dados <- tibble(
  x = runif(100),
  y = runif(100),
  color = sample(palette2, 100, replace = TRUE)
)


dados %>% 
  ggplot() +
  geom_point(
    aes(x, y, color = color)
  ) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = palette2[1]))

