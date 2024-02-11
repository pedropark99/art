library(Rcpp)
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)
library(ggthemes)
library(tictoc)


fern_transform <- function(coord, ind) {
  
  # coefficients for the stem function f_1
  if(ind == 1) {
    mat <- matrix(c(0, 0, 0, .16), 2, 2) # matrix to multiply
    off <- c(0, 0)                       # offset vector to add
  }
  
  # coefficients for the small leaflet function f_2
  if(ind == 2) {
    mat <- matrix(c(.85, -.04, .04, .85), 2, 2)
    off <- c(0, 1.6)                      
  }
  # coefficients for the right-side function f_3
  if(ind == 3) {
    mat <- matrix(c(.2, .23, -.26, .22), 2, 2)
    off <- c(0, 1.6)                      
  }
  
  # coefficients for the left-side function f_4
  if(ind == 4) {
    mat <- matrix(c(-.15, .26, .28, .24), 2, 2)
    off <- c(0, .44)                     
  }
  
  # return the affine transformed coords
  coord <- mat %*% coord + off
  return(coord)
}



fern_chaos <- function(iterations = 10000, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  # which transformation to apply at each iteration
  transform_index <- sample(
    x = 1:4, 
    size = iterations, 
    replace= TRUE, 
    prob = c(.01, .85, .07, .07)
  )
  
  # initialise chaos game at the origin
  start <- matrix(c(0, 0))
  
  # helper function to collapse accumulated output
  bind_to_column_matrix <- function(lst) {
    do.call(cbind, lst)
  }
  
  # iterate until done!
  coord_matrix <- transform_index |>
    accumulate(fern_transform, .init = start) |>
    bind_to_column_matrix() 
  
  # tidy the output, add extra columns, and return
  coord_df <- t(coord_matrix) |> 
    as.data.frame() 
  names(coord_df) <- c("x", "y")
  coord_df <- coord_df |>
    as_tibble() |>
    mutate(
      transform = c(0, transform_index),
      iteration = row_number() - 1
    )
  return(coord_df)
}


fern_dat <- fern_chaos(seed = 1)
fern_dat


ggplot(fern_dat, aes(x, y)) +
  geom_point(colour = "black", size = 1, stroke = 0) +
  coord_equal() +
  theme_void()





unboxer_base <- function(iterations, layers, seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  # coefficients defining affine layer transforms, A_i
  coeffs <- array(
    data = runif(9 * layers, min = -1, max = 1), 
    dim = c(3, 3, layers)
  )
  
  # list of variant functions, g_j
  funs <- list(
    function(point) point + (sum(point ^ 2)) ^ (1/3),
    function(point) sin(point),
    function(point) 2 * sin(point)
  )
  
  # updater function: apply the layer, then the function
  # (the weirdness with point[3] is me treating colour as special)
  update <- function(point, layer, transform) {
    f <- funs[[transform]]
    z <- point[3]
    point[3] <- 1
    point <- f(point %*% coeffs[,,layer])
    point[3] <- (point[3] + z)/2
    return(point)
  }
  
  # initial point
  point0 <- matrix(
    data = runif(3, min = -1, max = 1), 
    nrow = 1,
    ncol = 3
  )
  
  # sample points
  layer_ind <- sample(layers, iterations, replace = TRUE)  
  trans_ind <- sample(length(funs), iterations, replace = TRUE)  
  points <- accumulate2(layer_ind, trans_ind, update, .init = point0)
  
  # tidy up, add columns, and return
  points <- matrix(unlist(points), ncol = 3, byrow = TRUE)
  points <- cbind(
    points,
    c(0, layer_ind),
    c(0, trans_ind)
  )
  return(points)
}


unboxer_base(10, layers = 5, seed = 333)
sample_canva2 <- function(seed = NULL, n = 4) {
  
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}


unbox_art <- function(data, seed = NULL, size = 1) {
  
  # convert to data frame and sample a palette
  data <- data |> as.data.frame() |> as_tibble()
  names(data) <- c("x", "y", "c", "l", "t")[1:ncol(data)]
  shades <- sample_canva2(seed)
  
  # render image as a scatter plot
  ggplot(data, aes(x, y, colour = c)) +
    geom_point(
      size = size,
      stroke = 0,
      show.legend = FALSE
    ) + 
    theme_void() + 
    coord_equal(xlim = c(-4, 4), ylim = c(-4, 4)) + 
    scale_colour_gradientn(colours = shades) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.background = element_rect(
      fill = shades[1], colour = shades[1]
    ))
}


mil <- 1000000
tic()
unboxer_base(3 * mil, layers = 3, seed = 66) |> 
  unbox_art(seed = 66, size = .1)
