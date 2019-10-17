
img_rescale <- function(from, to, scale) {
  im <- magick::image_read(from)
  im <- magick::image_resize(
    image = im,
    geometry = magick::geometry_size_percent(scale))
  magick::image_write(im, to)
}

img_convert <- function(from, to, format) {
  im <- magick::image_read(from)
  im <- magick::image_convert(im, format)
  magick::image_write(im, to)
}


#' Short cut for generating multiple versions of the file
#'
#' @param file the original tiff file (8k x 8k)
#'
#' @export
make_allvers <- function(file) {
  tiff8000 <- file
  tiff4000 <- gsub("8000", "4000", tiff8000, fixed = TRUE)
  tiff2000 <- gsub("8000", "2000", tiff8000, fixed = TRUE)
  png8000 <- gsub(".tiff", ".png", tiff8000, fixed = TRUE)
  png4000 <- gsub("8000", "4000", png8000, fixed = TRUE)
  png2000 <- gsub("8000", "2000", png8000, fixed = TRUE)

  cat("."); img_rescale(tiff8000, tiff4000, scale = 50)
  cat("."); img_rescale(tiff8000, tiff2000, scale = 25)
  cat("."); img_convert(tiff8000, png8000, format = "png")
  cat("."); img_rescale(png8000, png4000, scale = 50)
  cat("."); img_rescale(png8000, png2000, scale = 25)

}


#' Monocolour version of theme_void
#'
#' @param background
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
