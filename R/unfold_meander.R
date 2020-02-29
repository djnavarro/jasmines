
#' Unfold a scene with a meander operation
#'
#' @param data data frame with x, y, etc
#' @param iterations number of time points in the time series
#' @param smoothing number of smoothing iterations
#' @param endpause length of pause at the end
#' @param output1 name of the primary unfolding variable to add (e.g., time)
#' @param output2 name of the secondary unfolding variable to add (e.g., series)
#'
#' @return tibble with columns series, time, x, y
#' @export
unfold_meander <- function(
  data = entity_circle(),
  iterations = 100,
  smoothing = 6,
  endpause = 0,
  output1 = "time",
  output2 = "series"
) {


  nseries <- nrow(data)
  data[[output2]] <- 1:nseries

  # for convenience
  lag <- function(x) dplyr::lag(x, default = 0)
  lead <- function(x) dplyr::lead(x, default = 0)

  # locally smoothed brownian bridge
  smooth_walk <- function(n = 0, iterations) {
    w <- c(0, e1071::rbridge(1, iterations-3))
    while(n > 0) {
      w <- (lag(w) + w + lead(w))/3
      n <- n - 1
    }
    w <- c(0, w, 0)
    return(w)
  }

  # generate multiple smoothed walks
  smooth_walks <- function(nseries, nsmooth, iterations) {
    replicate(nseries, smooth_walk(nsmooth, iterations)) %>% as.vector()
  }

  # # generate the base walks...
  # walks <- tibble::tibble(
  #   id = unclass(gl(nseries, length)),
  #   time = rep(1:length, nseries),
  #   x = smooth_walks(nseries, smoothing),
  #   y = smooth_walks(nseries, smoothing)
  # )

  # generate the base walks...
  walks <- tibble::tibble(
    x = smooth_walks(nseries, smoothing, iterations),
    y = smooth_walks(nseries, smoothing, iterations),
    id = sapply(data$id, rep, iterations) %>% as.vector() #rep.int(data$id, iterations)
  )
  walks[[output1]] <-rep(1:iterations, nseries) # time
  walks[[output2]] <- unclass(gl(nseries, iterations)) # series

  # merge it with data so that any extra columns from data are retained
  df <- data
  df$x <- NULL
  df$y <- NULL
  walks <- dplyr::full_join(walks, df, by = c("id",output2))

  # offset all the series using the seed
  data <- dplyr::rename(data, seed_x = x, seed_y = y)
  tmp <- walks %>%
    dplyr::full_join(data) %>%
    dplyr::mutate(x = x + seed_x, y = y + seed_y)
  walks$x <- tmp$x
  walks$y <- tmp$y

  # add copies of the final frame if requested...
  if(endpause > 0) {
    endpoint <- walks[walks[[output1]] == iterations,,drop = FALSE]

    add_end <- function(x) {
      endpoint[[output1]] <- x
      return(endpoint)
    }

    pausemap <- purrr::map_dfr(
      .x = (iterations + 1):(iterations + endpause),
      .f = add_end
    )
    walks <- dplyr::bind_rows(walks, pausemap)
  }

  # return
  return(walks)
}

