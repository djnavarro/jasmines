#' Adds an overlay to the existing plot
#'
#' @param pic Existing plot
#' @param border Colour of border
#' @param fill Colour of fill
#' @param linewidth Width of border
#' @param data Data to be shown in overlay (if NULL, taken from pic)
#'
#' @export
style_overlay <- function(pic, border = NULL, fill = NULL, linewidth = 1, data = NULL) {

  if(is.null(data)) {
    start <- dplyr::filter(pic$data, time == 1)
  } else {
    start <- data
  }
  fillcolour <- fill

  # add hollow fill for seed if requested
  if(!is.null(fillcolour)) {
    pic <- pic +
      ggplot2::geom_polygon(
        data = start,
        mapping = ggplot2::aes(x = x, y = y, group = factor(id)),
        inherit.aes = FALSE,
        colour = fillcolour,
        fill = fillcolour,
        show.legend = FALSE
      )
  }

  # add outline for seed if requested
  if(!is.null(border)) {
    pic <- pic +
      ggplot2::geom_path(
        data = start,
        mapping = ggplot2::aes(x = x, y = y, group = factor(id)),
        inherit.aes = FALSE,
        colour = border,
        show.legend = FALSE,
        size = linewidth
      )
  }

  return(pic)

}
