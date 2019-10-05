
#' Create animated brownian bridge
#'
#' @param file where to save
#' @param ntimes how many time points
#' @param nseries how many time series
#'
#' @export
bridge <- function(file, ntimes = 100, nseries = 20) {

  # construct tibble storing simulation
  tbl <- tibble::tibble(
    Time = rep(1:ntimes, nseries),
    Horizontal = replicate(nseries, c(0, e1071::rbridge(1,ntimes-1))) %>% as.vector(),
    Vertical = replicate(nseries, c(0, e1071::rbridge(1,ntimes-1))) %>% as.vector(),
    Series = gl(nseries, ntimes)
  )

  # gganimate
  pic <- tbl %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Horizontal,
      y = Vertical,
      colour = Series)) +
    ggplot2::geom_point(show.legend = FALSE,
                        size = 5, alpha = .6) +
    ggplot2::theme_void() +
    gganimate::transition_time(time = Time)  +
    gganimate::ease_aes('linear') +
    gganimate::shadow_wake(.1)

  # create
  pic %>% gganimate::animate(nframes = 200, detail = 5, type = "cairo")
  gganimate::anim_save(file)

}
