
#' Swirl
#'
#' @param bridge bridge tibble
#' @param scale length of swirl
#' @param type plot type (line, point, barbell)
#' @export
swirl <- function(
  bridge = make_bridges(50, 50),
  scale = .02,
  type = "line"
) {

  bridge <- bridge %>%
    dplyr::mutate(len = sqrt(x^2 + y^2))

  curl <- ambient::curl_noise(
    generator = ambient::gen_simplex,
    x = bridge$x,
    y = bridge$y
  )

  bridge$x2 <- bridge$x + (curl$x * scale * bridge$len)
  bridge$y2 <- bridge$y + (curl$y * scale * bridge$len)

  background <- "black"

  pic <- ggplot2::ggplot(
    data = bridge,
    mapping = ggplot2::aes(
      x = x, xend = x2,
      y = y, yend = y2,
      color = series
    )
  ) +
#    scico::scale_color_scico() +
    theme_mono("black")

  if(type == "line" | type == "barbell") {
    pic <- pic +
      ggplot2::geom_segment(size = 2, show.legend = FALSE)
  }

  if(type == "points" | type == "barbell") {
    pic <- pic +
      ggplot2::geom_point(size = 2, show.legend = FALSE) +
      ggplot2::geom_point(aes(x = x2, y = y2), show.legend = FALSE)
  }

  return(pic)
}

