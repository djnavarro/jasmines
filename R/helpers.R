#' Function factory for scico palettes
#'
#' @param ... arguments (besides n and alpha) to be passed to scico::scico
#'
#' @return a function that takes arguments n and alpha
#' @export
palette_scico <- function(...) {
  function(n, alpha = NULL) scico::scico(n = n, alpha = alpha, ...)
}

#' Function factory viridis palettes
#'
#' @param ... arguments to be passed to viridis::viridis
#'
#' @return a function that takes arguments n and alpha
#' @export
palette_viridis <- function(...) {
  function(n, alpha) viridis::viridis(n, alpha, ...)
}


#' Theme that supplies a background colour only
#'
#' @param background the background colour
#'
#' @return ggplot theme
#' @export
theme_mono <- function(background) {
  ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      ),
      axis.line = ggplot2::element_line(
        colour = background
      ),
      plot.background = ggplot2::element_rect(
        fill = background,
        colour = background
      ),
    )
}

