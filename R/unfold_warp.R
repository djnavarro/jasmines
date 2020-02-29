#' Unfold a scene with a warp operation
#'
#' @param data data frame with x, y, id, and more
#' @param iterations how many times should we iterate the noise?
#' @param scale how large is each step?
#' @param scatter should the noise seed be "scattered"?
#' @param output name of the output variable (default = time)
#'
#' @return thing
#' @export
unfold_warp <- function(
  data = scene_sticks(), # seed points
  iterations = 6,        # how many iterations?
  scale = .02,           # size of the noise step
  scatter = FALSE,
  output = "time"
) {

  # check for seed value
  if(scatter == FALSE) seed <- data$seed[1]
  if(scatter == TRUE)  seed <- NULL

  n <- nrow(data)
  data[[output]] <- 1    # initial value for the "time" output

  do_step <- function(data, time) {

    # generate the noise
    noise_x <- ambient::gen_simplex(
      x = data$x,
      seed = seed
    )

    noise_y <- ambient::gen_simplex(
      x = data$y,
      seed = seed
    )

    # update the co-ordinates
    data$x <- data$x + noise_x * scale
    data$y <- data$y + noise_y * scale

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
