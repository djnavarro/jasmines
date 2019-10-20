
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
style_walk <- function(
  bridge = meander(),
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
  nseries <- max(bridge$id)

  # gganimate
  pic <- ggplot2::ggplot(
    data = bridge,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      colour = factor(id)
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
  op <- graphics::par(bg = background)

  # create
  if(!is.null(file)) {
    pic %>% gganimate::animate(
      nframes = 200,
      detail = 5,
      type = "cairo"
    )
    gganimate::anim_save(file)
  } else {
    graphics::par(op)
    return(pic)
  }

  graphics::par(op)


  # invisibly return the tibble
  return(invisible(bridge))
}

