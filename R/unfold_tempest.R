#' Generate a tempest object
#'
#' @param data data frame with x, y, id, and more
#' @param iterations how many times should we iterate the curl noise?
#' @param scale how large is each curl step?
#' @param seed seed to pass to curl_noise()
#' @param output1 name of the primary unfolding variable to add (e.g., time)
#' @param output2 name of the secondary unfolding variable to add (e.g., order)
#'
#' @return a "tempest" ribbon, data frame with x, y, order, time and id
#' @export
unfold_tempest <- function(
  data = scene_sticks(), # seed points
  iterations = 6,       # how many iterations to curl?
  scale = .02,          # size of the curl step
  seed = NULL,
  output1 = "time",
  output2 = "order"
) {

  n <- nrow(data)
  data[[output1]] <- 1    # initial value for the "time" output
  data[[output2]] <- 1:n  # initial values for the "order" output

  curl_step <- function(data, time) {

    # generate the curl noise
    noise <- ambient::curl_noise(
      generator = ambient::gen_simplex,
      x = data$x,
      y = data$y,
      seed = seed
    )

    # update the co-ordinates
    data$x <- data$x + noise$x * scale
    data$y <- data$y + noise$y * scale

    # increment the "time" dimension
    data[[output1]] <- time

    # calculate the "ordering"
    data[[output2]] <- order(sqrt(noise$x^2 + noise$y^2))

    return(data)
  }

  # accumulate
  ribbon <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = curl_step,
    .init = data
  )
  ribbon <- dplyr::bind_rows(ribbon)

  return(ribbon)
}
