
#' Style as animated points
#'
#' @param data tibble specifying the time series
#' @param wake_length length of the tail
#' @param palette function generating palette values
#' @param background colour of the background
#' @param ... other arguments to pass to shadow_wake
#'
#' The \code{style_walk()} function generates an animation as an output. The
#' input data takes the form of a tibble with variables \code{x} and \code{y}
#' specifying co-ordinate values, a \code{id} variable identifying each point
#' and a \code{time} variable specifying the time
#'
#' @return The output is a gganim object
#'
#' @export
style_walk <- function(
  data = unfold_meander(),
  wake_length = .1,
  palette = palette_scico(palette="berlin"),
  background = "black",
  ...
) {

  # scale the series so that x and y are between -1 and 1
  bridge <- data %>%
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
    mapping = ggplot2::aes(x = x, y = y, colour = factor(id))
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

  return(pic)
}

