
library(jasmines)
library(ambient)
library(ggplot2)
library(scico)

# euclidian distance
euclid <- function(x,y) {
  sqrt(x^2 + y^2)
}

# gaussian decay
gauss <- function(distance, sd) {
  exp(-(distance/sd)^2)
}

# discretise to integer from 1 to n, preserving the midpoint
discretise <- function(x, n) {
  ext <- max(abs(x))
  x <- normalise(x, from = c(-1,1)*ext, to = c(0,1))
  x <- round(x * (n - 1) + 1)
  return(x)
}

# construct the grid
grid <- long_grid(
  x = seq(-10, 10, length.out = 1000),
  y = seq(-10, 10, length.out = 1000)
)

# distance from the origin
grid$dist <- euclid(grid$x, grid$y)

# # circular ripple pattern
# grid$ripple <- gen_waves(
#   x = grid$x,
#   y = grid$y,
#   frequency = .5
# )


# circular ripple pattern
grid$ripple <- gen_waves(
  x = grid$x,
  y = grid$y,
  frequency = .5
)

# add curl to the ripple
curl <- curl_noise(
  generator = gen_perlin,
  x = grid$dist,
  y = grid$ripple,
)
grid$curl <- curl$y

# apply gaussian decay
grid$out <- (grid$ripple + grid$curl) * gauss(grid$dist, sd = 3)

# define the colour palette
ncol <- 1000
cols <- scico::scico(n = ncol, palette = "berlin")

# colourise
grid$col <- cols[discretise(grid$out, ncol)]

rst <- as.raster(grid, col)
plot(rst)

# # plot
# scf <- .1
# pic <- ggplot(
#   data = grid,
#   mapping = aes(
#     x = x,# + scf * x_curl,
#     y = y,# + scf * y_curl,
#     colour = out
#     )
#   ) +
#   geom_point(show.legend = FALSE, alpha = .5) +
#   coord_equal() +
#   scale_fill_scico(palette = "berlin") +
#   theme_void()
#
# plot(pic)

#
# b <- make_rows(1,grain = 10000)
# b$x <- b$x - .5
# b$x <- b$x * 30
# b$y <- 0
#
# d <- curl_noise(
#   generator = fracture,
#   noise = gen_perlin,
#   octaves = 2,
#   fractal = fbm,
#   x = b$x,
#   y = b$y,
# )
#
# scl <- exp(-abs(b$x))
# d$x <- b$x #+ #d$x * .0 * scl
# d$y <- b$y + d$y * .01 * scl
#
# # god this is bad
# turn <- function(x, n) {
#   if(n == 0) {
#     return(x)
#   }
#   if(n > 0) {
#     up <- x[1:n]
#     down <- x[(n+1):length(x)]
#   } else {
#     n <- length(x) - abs(n)
#     up <- x[1:n]
#     down <- x[(n+1):length(x)]
#   }
#   return(c(down,up))
# }
#
# for(i in 1:5) {
#   d$y <- (d$y + turn(d$y, 1) + turn(d$y, -1))/3
# }
#
# x <- gen_waves(
#   x = d$x,
#   y = d$y
# )
#
#
# col_top <- "black"
# col_bottom <- "grey30"
# col_border <- "grey60"
# ext <- max(abs(d$y))
# p <- ggplot(d, aes(x,ymax = y)) +
#   geom_ribbon(colour = NA, ymin = -ext*1.3, fill = col_bottom) +
#   geom_ribbon(aes(x,ymin=y), colour = NA, ymax = ext*1.3, fill = col_top) +
#   geom_line(aes(x,y), colour = col_border) +
#   ylim(-ext,ext) +
#   theme_void()
# plot(p)
