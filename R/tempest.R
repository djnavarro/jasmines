


#' Generate a tempest image
#'
#' @param file where to save the file
#' @param seeds how many "sticks" to use as the seed?
#' @param iterations how many times should we iterate the curl noise?
#' @param scale how large is each curl step?
#' @param alpha how transparent is each line?
#' @param grain how many points comprise a stick?
#' @param width how wide are the lines?
#' @param palette function generating a palette (default: viridis)
#' @param ... arguments to be passed to palette
#'
#' @export
tempest <- function(
  file,
  seeds = 6,      # how many seed lines?
  iterations = 6, # how many iterations to curl?
  scale = .02,    # size of the curl step
  alpha = .3,     # transparency of each line
  grain = 10000,  # number of points in each stick
  width = 6,      # width of each line
  palette = NULL, # function to generate palette (args: n, alpha, ...)
  ...             # options to pass to palette function
) {

  # default palette
  if(is.null(palette)) {
    palette <- function(n, alpha, ...) {
      viridis::viridis(n, alpha, ...)
    }
  }

  # a "stick" is just a random line
  make_stick <- function(id, n = grain) {
    tibble::tibble(
      x = seq(stats::runif(1), stats::runif(1), length.out = n),
      y = seq(stats::runif(1), stats::runif(1), length.out = n),
      id = id
    )
  }

  # create a set of sticks to use as the seed
  sticks <- purrr::map_dfr(1:seeds, make_stick)

  # create a colour palette
  cols <- palette(nrow(sticks), alpha)

  # iterate each point through curl noise
  ribbon <- list()
  ribbon[[1]] <- sticks
  ribbon[[1]]$z <- 0

  for(i in 1:iterations) {

    # apply curl noise
    ribbon[[i+1]] <- ambient::curl_noise(
      ambient::gen_simplex,
      x = ribbon[[i]]$x,
      y = ribbon[[i]]$y
    )

    # order the steps by their length
    ribbon[[i+1]]$z <- sqrt(ribbon[[i+1]]$x^2 + ribbon[[i+1]]$y^2)
    ribbon[[i+1]]$z <- order(ribbon[[i+1]]$z)

    # rescale the curl noise
    ribbon[[i+1]]$x <- ribbon[[i]]$x + ribbon[[i+1]]$x * scale
    ribbon[[i+1]]$y <- ribbon[[i]]$y + ribbon[[i+1]]$y * scale
  }

  # min, max
  xmin <- min(sapply(ribbon, function(r) {min(r$x)}))
  xmax <- max(sapply(ribbon, function(r) {max(r$x)}))
  ymin <- min(sapply(ribbon, function(r) {min(r$y)}))
  ymax <- max(sapply(ribbon, function(r) {max(r$y)}))

  # setup the plot
  gap <- .05
  op <- graphics::par(bg = "black", pty = "s")
  graphics::plot.new()
  graphics::plot.window(c(-gap, 1 + gap), c(-gap, 1 + gap))

  # plot a series of curl iterations
  for(i in 1:iterations) {
    graphics::segments(
      x0 = (ribbon[[i]]$x -xmin) / (xmax - xmin),
      y0 = (ribbon[[i]]$y - ymin) / (ymax - ymin),
      x1 = (ribbon[[i+1]]$x - xmin) / (xmax - xmin),
      y1 = (ribbon[[i+1]]$y - ymin) / (ymax - ymin),
      col = cols[ribbon[[i+1]]$z],
      lwd = width,
    )
  }

  # generate the file
  grDevices::dev.print(
    device = grDevices::png,
    filename = file,
    width = 3000,
    height = 3000
  )

  # reset device parameters
  graphics::par(op)

}
