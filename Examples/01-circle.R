library(tidyverse)
palette2 <- c(
  "#263F30",
  "#EAD3D2"
)

dados <- tibble(
  x = 1:100 |> cos(),
  y = 1:100 |> sin(),
  color = palette2[2]
)


dados %>% 
  ggplot() +
  geom_path(
    aes(x, y, color = color)
  ) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = palette2[1]))

