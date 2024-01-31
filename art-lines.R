setwd("C:/Users/pedro/Documents/Projetos/Livros/art")

library(tidyverse)
library(ambient)

rad2deg <- function(rad) {
  (rad * 180) / pi
}

deg2rad <- function(deg) {
  (deg * pi) / 180
}

width <- 2000
height <- 2000

step <- 10
x <- 20
y <- 50
lastx <- x
lasty <- y

n_steps <- ((width - 200) - x) / step
df <- tibble(
  n_step = seq_len(n_steps),
  x = ((n_step - 1) * step) + x,
  angle = deg2rad(n_step),
  y = 50 + sin(angle * 2)
)


ggplot(df) +
  geom_line(
    aes(group = 1, x = x, y = y)
  )




noises <- gen_perlin(1, y = 1 + (seq_len(n_steps) * 0.1))
df <- tibble(
  n_step = seq_len(n_steps),
  x = ((n_step - 1) * step) + x,
  angle = deg2rad(n_step),
  y = seq_len(n_steps) * 2 + 50 + (noises * 50)
)

ggplot(df) +
  geom_line(
    aes(group = 1, x = x, y = y)
  ) +
  xlim(c(0, width)) +
  ylim(c(0, height))
