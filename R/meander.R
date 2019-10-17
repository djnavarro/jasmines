
#' Create meandering walk animation
#'
#' @param file where to save
#' @param bridge tibble specifying the time series
#' @param wake_length length of the tail
#' @param palette function generating palette values
#' @param background colour of the background
#' @param ... other arguments to pass to shadow_wake
#'
#' @export
meander <- function(
  file = NULL,
  bridge = make_bridges(),
  wake_length = .1,
  palette = palette_scico(palette="berlin"),
  background = "black",
  ...
) {

  # scale the series so that x and y are between -1 and 1
  bridge <- bridge %>%
    dplyr::mutate(
      x = x / max(abs(x), abs(y)),
      y = y / max(abs(x), abs(y))
    )

  # read off the parameters
  ntimes <- max(bridge$time)
  nseries <- max(bridge$series)

  # gganimate
  pic <- ggplot2::ggplot(
      data = bridge,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        colour = factor(series)
      )
    ) +
    ggplot2::geom_point(
      show.legend = FALSE,
      size = 3,
      alpha = .6
    ) +
    theme_mono(background) +
    ggplot2::scale_color_manual(values = palette(nseries, alpha = .6)) +
    gganimate::transition_time(time = time)  +
    gganimate::ease_aes('linear') +
    gganimate::shadow_wake(wake_length = wake_length, ...)

  # just in case
  op <- par(bg = background)

  # create
  if(!is.null(file)) {
    pic %>% gganimate::animate(
      nframes = 200,
      detail = 5,
      type = "cairo"
    )
    gganimate::anim_save(file)
  } else {
    par(op)
    return(pic)
  }

  par(op)


  # invisibly return the tibble
  return(invisible(bridge))
}


#' Make smoothed brownian bridge time series
#'
#' @param ntimes number of time points
#' @param nseries number of time series
#' @param smoothing number of smoothing iterations
#'
#' @return tibble with columns series, time, x, y
#' @export
make_bridges <- function(
  ntimes = 100,
  nseries = 20,
  smoothing = 6
) {

  # for convenience
  lag <- function(x) dplyr::lag(x, default = 0)
  lead <- function(x) dplyr::lead(x, default = 0)

  # locally smoothed brownian bridge
  smooth_walk <- function(n = 0) {
    w <- c(0, e1071::rbridge(1, ntimes-3))
    while(n > 0) {
      w <- (lag(w) + w + lead(w))/3
      n <- n - 1
    }
    w <- c(0, w, 0)
    #w <- w + rnorm(1)/100
    return(w)
  }

  # generate multiple smoothed walks
  smooth_walks <- function(nseries, nsmooth) {
    replicate(nseries, smooth_walk(nsmooth)) %>% as.vector()
  }

  # return
  tibble::tibble(
    series = unclass(gl(nseries, ntimes)),
    time = rep(1:ntimes, nseries),
    x = smooth_walks(nseries, smoothing),
    y = smooth_walks(nseries, smoothing)
  )
}





#' Rotate around the origin
#'
#' @param theta size of rotation
#'
#' @return function that returns a modified data frame
#' @export
flow_circle <- function(theta = pi/128) {
  function(data) {
    x <- data$x * cos(theta) - data$y * sin(theta)
    y <- data$x * sin(theta) + data$y * cos(theta)
    data$x <- x
    data$y <- y
    return(data)
  }
}


#' Smoothed brownian bridges with seed points
#'
#' @param dots the seed points
#' @param ntimes times
#' @param npause length of pause at the end
#'
#' @return bridges tibble
#' @export
make_bridges_from_seed <- function(dots, ntimes = 100, npause = 10) {

  # the bridges
  bridges <- make_bridges(
    ntimes = ntimes,
    nseries = nrow(dots)
  ) %>%
    dplyr::full_join(dots) %>%
    dplyr::mutate(
      x = x + xpos/10,
      y = y + ypos/10
    )

  # grab the start point, replicate several times to
  # create a pause
  endpoint <- bridges %>% dplyr::filter(time == ntimes)
  pausemap <- purrr::map_dfr(
    .x = (ntimes + 1):(ntimes + npause),
    .f = ~ dplyr::mutate(endpoint, time = .x)
  )
  bridges <- dplyr::bind_rows(bridges, pausemap)

  return(bridges)
}

# # curl current
# add_eddy <- function(tbl, seed) {
#
#   # generate the curl field at each point
#   curl <- ambient::curl_noise(
#     generator = ambient::gen_simplex,
#     seed = seed,
#     x = tbl$x,
#     y = tbl$y
#   )
#
#   curl$x <- curl$x * vlength(tbl) * .01
#   curl$y <- curl$x * vlength(tbl) * .01
#
#   tbl <- tbl %>% dplyr::mutate(
#     x = x + curl$x,
#     y = y + curl$y
#   )
#
#   return(tbl)
# }
