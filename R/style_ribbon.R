
#' Style as a ribbon image
#'
#' @param data data frame with x, y, order, id, time
#' @param discard how many iterations should we discard before drawing?
#' @param alpha length two numeric, first element is the initial alpha, (optional) second is the decay rate for alpha
#' @param palette function generating a palette (or string naming the palette)
#' @param colour name of variable to use to specify the colour aesthetic
#' @param background colour of the background in the plot
#' @param type type of geom to use ("segment", "curve" or "point")
#' @param ... arguments to pass to geom
#'
#' @return Returns a ggplot2 object
#' @export
style_ribbon <- function(
  data,
  palette = "viridis",   # function to generate palette (args: n, alpha)
  colour = "order",      # name of column to use as the colouring
  alpha = c(.3, 0),      # initial transparency and decay
  background = "black",
  discard = 0,           # how many of the iterations do we not draw?
  type = "segment",
  ...
) {

  ribbon <- data
  ribbon$order <- ribbon[[colour]]

  # use named paletee if the input is character
  if(is.character(palette)) {
    palette <- palette_named(palette)
  }

  # rewrite parameters as their old names
  alpha_init <- alpha[1]
  if(length(alpha) > 1) {
    alpha_decay <- alpha[2]
  } else {
    alpha_decay <- 0
  }
  burnin <- discard

  #supply a default order if the input lacks one
  if(!("order" %in% names(ribbon))) {
    ribbon$order <- 1:nrow(ribbon)
  }

  # min, max
  xmin <- min(ribbon$x)
  xmax <- max(ribbon$x)
  ymin <- min(ribbon$y)
  ymax <- max(ribbon$y)

  # force to the same scale
  xmin <- min(xmin, ymin)
  xmax <- max(xmax, ymax)
  ymin <- xmin
  ymax <- xmax

  # normalise
  ribbon <- ribbon %>%
    dplyr::mutate(
      x = (x - xmin) / (xmax - xmin),
      y = (y - ymin) / (ymax - ymin),
      al = alpha_init * (1 - alpha_decay)^(time - 1)
    )

  ribbon2 <- ribbon %>%
    dplyr::rename(xend = x, yend = y) %>%
    dplyr::mutate(time = time - 1) %>%
    dplyr::filter(time > 0)

  ribbon <- ribbon %>%
    dplyr::filter(time < max(time))

  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend
  ribbon$order <- ribbon2$order

  # generate colour set
  col_set <- colours_from(palette, ribbon$order)

  # create basic object
  pic <- ggplot2::ggplot(
    data = ribbon %>% dplyr::filter(time > burnin),
    mapping = ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      alpha = al,
      colour = factor(order)
    )
  ) +
    ggplot2::scale_color_manual(values = col_set) +
    ggplot2::scale_alpha_identity() +
    theme_mono(background) +
    ggplot2::coord_equal(
      xlim = c(-.05, 1.05),
      ylim = c(-.05, 1.05)
    )

  # add the plot information
  if(type == "segment") {
    pic <- pic + ggplot2::geom_segment(show.legend = FALSE, ...)
  }
  if(type == "curve") {
    pic <- pic + ggplot2::geom_curve(show.legend = FALSE, ...)
  }
  if(type == "point") {
    pic <- pic + ggplot2::geom_point(show.legend = FALSE, ...)
  }

  return(pic)
}


colours_from <- function(palette, order, ...) {
  palette(n = length(unique(order)))
}

