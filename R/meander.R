
#' Create meandering walk animation
#'
#' @param bridge tibble specifying the time series
#' @param file where to save
#' @param wake_length length of the tail
#' @param palette function generating palette values
#' @param background colour of the background
#' @param ... other arguments to pass to shadow_wake
#'
#' @export
meander <- function(
  bridge = make_bridges(),
  file = NULL,
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
#' @param seed number of time series or complete seed as tibble
#' @param length number of time points in the time series
#' @param smoothing number of smoothing iterations
#' @param endpause length of pause at the end
#'
#' @return tibble with columns series, time, x, y
#' @export
make_bridges <- function(
  seed = 20,
  length = 100,
  smoothing = 6,
  endpause = 0
) {

  # if seed is an number, interpret it as the number of
  # time series to generate, and do not shift the data
  if(base::length(seed) == 1 & is.numeric(seed)) {
    nseries <- seed
    shift <- FALSE

    # alternatively, seed should be a data frame with two
    # columns, xpos and ypos, indicating the offset for
    # each of the series
  } else {
    nseries <- nrow(seed)
    seed$series <- 1:nseries
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
    series = unclass(gl(nseries, length)),
    time = rep(1:length, nseries),
    x = smooth_walks(nseries, smoothing),
    y = smooth_walks(nseries, smoothing)
  )

  # offset all the series using the seed if requested...
  if(shift) {
    walks <- walks %>%
      dplyr::full_join(seed) %>%
      dplyr::mutate(x = x + xpos, y = y + ypos)
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


# # rotate around origin
# flow_circle <- function(theta = pi/128) {
#   function(data) {
#     x <- data$x * cos(theta) - data$y * sin(theta)
#     y <- data$x * sin(theta) + data$y * cos(theta)
#     data$x <- x
#     data$y <- y
#     return(data)
#   }
# }

# make_bridges_from_seed <- function(dots, ntimes = 100, npause = 10) {
#
#   # the bridges
#   bridges <- make_bridges(
#     ntimes = ntimes,
#     nseries = nrow(dots)
#   ) %>%
#     dplyr::full_join(dots) %>%
#     dplyr::mutate(
#       x = x + xpos/10,
#       y = y + ypos/10
#     )
#
#   # grab the start point, replicate several times to
#   # create a pause
#   endpoint <- bridges %>% dplyr::filter(time == ntimes)
#   pausemap <- purrr::map_dfr(
#     .x = (ntimes + 1):(ntimes + npause),
#     .f = ~ dplyr::mutate(endpoint, time = .x)
#   )
#   bridges <- dplyr::bind_rows(bridges, pausemap)
#
#   return(bridges)
# }

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
