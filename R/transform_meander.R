
#' Make smoothed brownian bridge time series
#'
#' @param seed number of time series or complete seed as tibble
#' @param length number of time points in the time series
#' @param smoothing number of smoothing iterations
#' @param endpause length of pause at the end
#' @param na.rm logical, remove NAs from seed
#'
#' @return tibble with columns series, time, x, y
#' @export
time_meander <- function(
  seed = 20,
  length = 100,
  smoothing = 6,
  endpause = 0,
  na.rm = TRUE          # Remove NA values from the data.frame
){

  # if seed is an number, interpret it as the number of
  # time series to generate, and do not shift the data
  if(base::length(seed) == 1 & is.numeric(seed)) {
    nseries <- seed
    shift <- FALSE

    # alternatively, seed should be a data frame with two
    # columns, xpos and ypos, indicating the offset for
    # each of the series
  } else {
      if(apply(seed, 2, function(x) any(is.na(x))) && !na.rm) stop("Data contains NA's you should remove them if you want this to work. (don't be Dale)")

      if(apply(seed, 2, function(x) any(is.na(x))) && na.rm){
        seed<- dplyr::filter(.data = seed, !is.na(seed$x),!is.na(seed$y))
      }
    nseries <- nrow(seed)
    seed$id <- 1:nseries
    shift <- TRUE
  }

  # for convenience
  lag <- function(x) dplyr::lag(x, default = 0)
  lead <- function(x) dplyr::lead(x, default = 0)

  # locally smoothed brownian bridge
  smooth_walk <- function(n = 0) {
    w <- c(0, e1071::rbridge(1, length-3))
    while(n > 0) {
      w <- (lag(w) + w + lead(w))/3
      n <- n - 1
    }
    w <- c(0, w, 0)
    return(w)
  }

  # generate multiple smoothed walks
  smooth_walks <- function(nseries, nsmooth) {
    replicate(nseries, smooth_walk(nsmooth)) %>% as.vector()
  }

  # generate the base walks...
  walks <- tibble::tibble(
    id = unclass(gl(nseries, length)),
    time = rep(1:length, nseries),
    x = smooth_walks(nseries, smoothing),
    y = smooth_walks(nseries, smoothing)
  )

  # offset all the series using the seed if requested...
  if(shift) {
    seed <- dplyr::rename(seed, seed_x = x, seed_y = y)
    walks <- walks %>%
      dplyr::full_join(seed) %>%
      dplyr::mutate(x = x + seed_x, y = y + seed_y)
  }

  # add copies of the final frame if requested...
  if(endpause > 0) {
    endpoint <- walks %>% dplyr::filter(time == length)
    pausemap <- purrr::map_dfr(
      .x = (length + 1):(length + endpause),
      .f = ~ dplyr::mutate(endpoint, time = .x)
    )
    walks <- dplyr::bind_rows(walks, pausemap)
  }

  # return
  return(walks)
}

