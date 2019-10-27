#' Export image to a file
#'
#' @param input art object to print
#' @param filename filename
#' @param width defaults to 3000 pixels (10in at 300dpi)
#' @param height defaults to 3000 pixels (10in at 300dpi)
#' @param dpi defaults to 300dpi
#'
#' @export
export_image <- function(
  input,
  filename,
  width = 10,
  height = 10,
  dpi = 300
) {
  ggplot2::ggsave(
    filename = filename, plot = input,
    width = width, height = height,
    dpi = dpi
  )
}


#' Export animation to a file
#'
#' @param input art object to print
#' @param filename filename
#' @param nframes defaults to 200
#' @param detail number of interpolated frames, defaults to 5
#' @param type defaults to "cairo"
#'
#' @export
export_animation <- function(
  input,
  filename,
  nframes = 200,
  detail = 5,
  type = "cairo"
) {
  input %>% gganimate::animate(
    nframes = nframes,
    detail = detail,
    type = type
  )
  gganimate::anim_save(filename)
  }


