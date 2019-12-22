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
unfold_slice <- function(
  data = scene_sticks(), # seed points
  iterations = 6,        # how many iterations
  scale = .2,           # size of the worley step
  seed = NULL,
  output1 = "time",
  output2 = "order"
) {

  # parse the seed information
  if(is.null(seed)) {
    seed_x <- NULL
    seed_y <- NULL
  }
  if(length(seed) == 1) {
    seed_x <- seed
    seed_y <- seed
  }
  if(length(seed) > 1) {
    seed_x <- seed[1]
    seed_y <- seed[2]
  }

  # initialise
  state <- data
  state[[output1]] <- 1
  state[[output2]] <- 1

  # set up the list
  slice <- list()
  slice[[1]] <- state

  for(i in 2:iterations) {

    x <- state$x + ambient::gen_worley(state$x, state$y, seed = seed_x, value = "cell") * scale
    y <- state$y + ambient::gen_worley(state$y, state$x, seed = seed_y, value = "cell") * scale

    state$x <- x
    state$y <- y
    state[[output1]] <- i
    state[[output2]] <- i

    slice[[i]] <- state

  }

  slice <- dplyr::bind_rows(slice)
  return(slice)
}
