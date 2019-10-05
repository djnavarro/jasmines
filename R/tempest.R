
#' Generate a random set of sticks
#'
#' @param n how many sticks
#' @param grain how many points along each stick
#'
#' @return a tibble with columns x, y and id
#' @export
make_sticks <- function(n = 10, grain = 1000) {
  make_stick <- function(id, grain) {
    return(tibble::tibble(
      x = seq(stats::runif(1), stats::runif(1), length.out = grain),
      y = seq(stats::runif(1), stats::runif(1), length.out = grain),
      id = id
    ))
  }
  points <- purrr::map_dfr(1:n, make_stick, grain = grain)
  return(points)
}

#' Generate a random set of bubbles
#'
#' @param n how many bubbles
#' @param grain how many points along each stick
#'
#' @return a tibble with columns x, y and id
#' @export
make_bubbles <- function(n = 2, grain = 1000) {

  make_bubble <- function(id, grain) {

    radius <- stats::runif(1)
    origin_x <- stats::runif(1)
    origin_y <- stats::runif(1)
    th <- seq(0, 2*pi, length.out = grain)

    return(tibble::tibble(
      x = radius * cos(th) + origin_x,
      y = radius * sin(th) + origin_y,
      id = id
    ))
  }

  points <- purrr::map_dfr(1:n, make_bubble, grain = grain)
  return(points)
}





#' Generate a tempest image
#'
#' @param file where to save the file
#' @param seed data frame with x, y, id
#' @param iterations how many times should we iterate the curl noise?
#' @param scale how large is each curl step?
#' @param alpha how transparent is each line?
#' @param width how wide are the lines?
#' @param box vector specifying the edges
#' @param zoom rescale image to fit in the unit square?
#' @param palette function generating a palette (default: viridis)
#' @param ... arguments to be passed to palette
#'
#' @export
tempest <- function(
  file,
  seed = make_sticks(), # seed points
  iterations = 6, # how many iterations to curl?
  scale = .02, # size of the curl step
  alpha = .3, # transparency of each line
  width = 6, # width of each line
  box = NULL, # size of the box
  zoom = TRUE, # zoom in/out so that the final image fits in unit square
  palette = NULL, # function to generate palette (args: n, alpha, ...)
  ... # options to pass to palette function
) {

  # default palette
  if(is.null(palette)) {
    palette <- function(n, alpha, ...) {
      viridis::viridis(n, alpha, ...)
    }
  }

  # create a colour palette
  cols <- palette(nrow(seed), alpha, ...)

  # iterate each point through curl noise
  ribbon <- list()
  ribbon[[1]] <- seed
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
    if(is.null(box)) {
      ribbon[[i+1]]$x <- ribbon[[i]]$x + ribbon[[i+1]]$x * scale
      ribbon[[i+1]]$y <- ribbon[[i]]$y + ribbon[[i+1]]$y * scale
    }
  }

  if(is.null(box)) {
    box <- c(xmin = -.05, xmax = 1.05, ymin = -.05, ymax = 1.05)
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
  graphics::plot.window(xlim = box[1:2], ylim = box[3:4])

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
