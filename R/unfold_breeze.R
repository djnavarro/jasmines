#' Unfold a scene with a swirl operation
#'
#' @param data data frame with x, y, id, and more
#' @param iterations how many times should we iterate?
#' @param scale how large is each step?
#' @param drift gaussian noise to inject at each step
#' @param noise noise function (default is ambient::gen_simplex)
#' @param fractal fractal function (default is ambient::billow)
#' @param octaves default = 8
#' @param output name of the unfolding variable to add (e.g., time)
#' @param ... arguments to pass to ambient::fracture
#'
#' @return a "tempest" ribbon, data frame with x, y, order, time and id
#' @export
unfold_breeze <- function(
  data = scene_sticks(), # seed points
  iterations = 6,       # how many iterations to curl?
  scale = .02,          # size of the curl step
  drift = .01,
  noise = NULL,
  fractal = NULL,
  octaves = 8,
  output = "time", ...
) {

  # default noise and fractal
  if(is.null(noise)) noise <- ambient::gen_simplex
  if(is.null(fractal)) fractal<- ambient::billow

  # get seed value
  seed <- data$seed[1]

  n <- nrow(data)
  data[[output]] <- 1   # initial value

  do_step <- function(data, time) {

    n <- nrow(data)

    # generate the curl noise
    noise <- ambient::curl_noise(
      x = data$x + stats::rnorm(n) * drift,
      y = data$y + stats::rnorm(n) * drift,
      seed = seed,
      generator = ambient::fracture,
      noise = noise,
      fractal = fractal,
      octaves = octaves,
      ...
    )

    # update the co-ordinates
    data$x <- data$x + noise$x * scale
    data$y <- data$y + noise$y * scale

    # increment the "time" dimension
    data[[output]] <- time

    return(data)
  }

  # accumulate
  ribbon <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = do_step,
    .init = data
  )
  ribbon <- dplyr::bind_rows(ribbon)

  return(ribbon)
}
