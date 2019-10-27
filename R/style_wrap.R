
#' Style wrap
#'
#' @param coil a coiled bridge
#' @param background colour
#' @param type line, barbelle or points
#' @param palette palette generator
#'
#' @return ggplot
#' @export
style_wrap <- function(
  coil,
  file = NULL,
  background = "black",
  type = "line",
  width = 3000, # width in pixels ()
  height = 3000,
  palette = palette_viridis()
) {

  nseries <- max(coil$id)

  pic <- ggplot2::ggplot(
    data = coil,
    mapping = ggplot2::aes(
      x = x, xend = x2,
      y = y, yend = y2,
      color = factor(id)
    )
  ) +
    ggplot2::scale_color_manual(values = palette(nseries, alpha = .6)) +
    theme_mono("black")

  if(type == "line" | type == "barbell") {
    pic <- pic +
      ggplot2::geom_segment(size = 2, show.legend = FALSE)
  }

  if(type == "points" | type == "barbell") {
    pic <- pic +
      ggplot2::geom_point(size = 2, show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(x = x2, y = y2), show.legend = FALSE)
  }

  return(pic)
}

