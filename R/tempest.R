

#' Create tempest images
#'
#' @export
#'
tempest <- function(path = "tempest.png") {

  # create a point set from perlin noise
  n <- 300
  x <- ambient::noise_perlin(
    dim = c(n, n),
    fractal = "rigid-multi",
    pertubation = "normal",
    frequency = .005
  )

  # seed locations are the ridges
  v <- which(x > quantile(x, .9), arr.ind = TRUE)
  v <- v/(n+1)
  v <- as.data.frame(v)
  names(v) <- c("x", "y")

  # create a colour palette
  n <- dim(v)[1]
  cols <- viridis::viridis(n, alpha = .5, option = "magma")

  # parameters
  its <- 2 # number of iterations to peturb each point
  stp <- .1 # size of the curl step

  # iterate each point through curl noise
  ribbon <- list()
  ribbon[[1]] <- v
  ribbon[[1]]$z <- 0
  for(i in 1:its) {
    ribbon[[i+1]] <- ambient::curl_noise(
      ambient::gen_simplex,
      x = ribbon[[i]]$x,
      y = ribbon[[i]]$y
    )
    ribbon[[i+1]]$z <- sqrt(ribbon[[i+1]]$x^2 + ribbon[[i+1]]$y^2)
    ribbon[[i+1]]$z <- order(ribbon[[i+1]]$z)
    ribbon[[i+1]]$x <- ribbon[[i]]$x + ribbon[[i+1]]$x*stp
    ribbon[[i+1]]$y <- ribbon[[i]]$y + ribbon[[i+1]]$y*stp
  }


  # draw the picture
  gap <- .5
  op <- par(bg = "black", pty = "s")
  plot.new()
  plot.window(c(-gap,1+gap), c(-gap,1+gap))
  for(i in 1:its) {
    segments(
      x0 = ribbon[[i]]$x,
      y0 = ribbon[[i]]$y,
      x1 = ribbon[[i+1]]$x,
      y1 = ribbon[[i+1]]$y,
      col = cols[ribbon[[i+1]]$z],
      lwd = 4,
    )
  }

  dev.print(
    device = png,
    filename = path,
    width = 3000,
    height = 3000
  )

  # reset device parameters
  par(op)

}
