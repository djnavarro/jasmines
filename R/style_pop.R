
#' Style as a pop art image
#'
#' @param data data frame with x, y, order, id, time
#' @param alpha length two numeric, first element is the initial alpha, (optional) second is the decay rate for alpha
#' @param palette function generating a palette (or string naming the palette)
#' @param colour name of variable to use to specify the colour aesthetic
#' @param background colour of the background in the plot
#' @param ... arguments to pass to geom
#'
#' @return Returns a ggplot2 object
#' @export
style_pop <- function(
  data,
  palette = "base",
  colour = "ind",
  alpha = .3,
  fade = 0,
  background = "warhol",
  adjust = .7,
  panels = 4,
  ...
) {

  # construct parameter set
  params <- tibble::tibble(
    palette = palette,
    alpha = alpha,
    colour = colour,
    fade = fade,
    background = background,
    adjust = adjust
  )

  # replicate if necessary
  if(nrow(params) < panels) {
    copies <- ceiling(panels/nrow(params))
    params <- purrr::map_dfr(1:copies, ~params)
  }

  # truncate if necessary
  if(nrow(params) > panels) {
    params <- params[1:panels,]
  }

  params$ind <- 1:panels

  one_plot <- function(par) {
    style_ribbon(
      data = data,
      palette = palette_adjust(
        name = par$palette,
        prefix = NULL,
        red.f = par$adjust,
        green.f = par$adjust,
        blue.f = par$adjust
      ),
      colour = par$colour,
      alpha = c(par$alpha, par$fade),
      background = (palette_named(par$background))(panels)[par$ind],
      ...
    )
  }

  warhol <- params %>%
    purrr::transpose() %>%
    purrr::map(one_plot) %>%
    patchwork::wrap_plots()

  return(warhol)
}
