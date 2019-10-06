
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

#' Generate an even set of rows
#'
#' @param n how many rows
#' @param grain how many points along each row
#'
#' @return a tibble with columns x, y and id
#' @export
make_rows <- function(n = 10, grain = 1000) {
  make_row <- function(id, grain) {
    return(tibble::tibble(
      x = seq(0, 1, length.out = grain),
      y = id/(n+1),
      id = id
    ))
  }
  points <- purrr::map_dfr(1:n, make_row, grain = grain)
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
#' @param file where to save the file (default: NULL)
#' @param seed data frame with x, y, id
#' @param iterations how many times should we iterate the curl noise?
#' @param burnin how many iterations should we discard as burnin?
#' @param scale how large is each curl step?
#' @param alpha_init how transparent is each line?
#' @param alpha_decay how does transparency decay over iterations?
#' @param width how wide are the lines?
#' @param box vector specifying the edges
#' @param zoom rescale image to fit in the unit square?
#' @param seed_col what colour to draw the seed? (default: NULL)
#' @param seed_fill what colour to fill the seed? (default: NULL)
#' @param palette function generating a palette (default: viridis)
#' @param ... arguments to be passed to palette
#'
#' @export
tempest <- function(
  file = NULL,
  seed = make_sticks(), # seed points
  iterations = 6, # how many iterations to curl?
  burnin = 0, # how many of the iterations do we not draw?
  scale = .02, # size of the curl step
  alpha_init = .3, # transparency of each line
  alpha_decay = 0, # rate of decay
  width = 6, # width of each line
  box = NULL, # size of the box
  zoom = TRUE, # zoom in/out so that the final image fits in unit square
  seed_col = NULL,
  seed_fill = NULL,
  palette = NULL, # function to generate palette (args: n, alpha, ...)
  ... # options to pass to palette function
) {

  # default palette
  if(is.null(palette)) {
    palette <- function(n, alpha, ...) {
      viridis::viridis(n, alpha, ...)
    }
  }

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

  # set the bounding box if there isn't one given
  if(is.null(box)) {
    box <- c(xmin = -.05, xmax = 1.05, ymin = -.05, ymax = 1.05)
  }

  # min, max
  xmin <- min(sapply(ribbon, function(r) {min(r$x)}))
  xmax <- max(sapply(ribbon, function(r) {max(r$x)}))
  ymin <- min(sapply(ribbon, function(r) {min(r$y)}))
  ymax <- max(sapply(ribbon, function(r) {max(r$y)}))

  # force to the same scale
  xmin <- min(xmin, ymin)
  xmax <- max(xmax, ymax)
  ymin <- xmin
  ymax <- xmax

  # draw to png file
  if(!is.null(file)) {
    grDevices::png(
      filename = file,
      width = 4000,
      height = 4000,
      bg = "black"
    )
  }

  # setup the plot
  op <- graphics::par(bg = "black", pty = "s", mar = c(0,0,0,0))
  graphics::plot.new()
  graphics::plot.window(xlim = box[1:2], ylim = box[3:4])

  # plot a series of curl iterations
  for(i in 1:iterations) {
    if(i > burnin) {
      # create a colour palette for this iteration
      cols <- palette(nrow(seed), (alpha_init) * (1 - alpha_decay)^(i-1), ...)

      # draw the segments
      graphics::segments(
        x0 = (ribbon[[i]]$x -xmin) / (xmax - xmin),
        y0 = (ribbon[[i]]$y - ymin) / (ymax - ymin),
        x1 = (ribbon[[i+1]]$x - xmin) / (xmax - xmin),
        y1 = (ribbon[[i+1]]$y - ymin) / (ymax - ymin),
        col = cols[ribbon[[i+1]]$z],
        lwd = width,
      )
    }
  }

  # fill in the seed shape if requested
  if(!is.null(seed_fill)) {
    for(i in 1:max(seed$id)) {
      s <- seed[seed$id == i,]
      graphics::polygon(
        x = (s$x - xmin)/(xmax - xmin),
        y = (s$y - ymin)/(ymax - ymin),
        col = seed_fill,
        lwd= 1
      )
    }
  }

  # draw outline of seed shape if requested
  if(!is.null(seed_col)) {
    for(i in 1:max(seed$id)) {
      s <- seed[seed$id == i,]
      graphics::lines(
        x = (s$x - xmin)/(xmax - xmin),
        y = (s$y - ymin)/(ymax - ymin),
        col = seed_col,
        lwd= 10
      )
    }
  }

  # generate the file
  if(!is.null(file)) {
    grDevices::dev.off()
  }

  # reset device parameters
  graphics::par(op)

}
