#' Convert image to hex
#'
#' @param from Path to original file
#' @param to Path to destination file
#' @param background Background colour
#' @param crop Proportion of (smaller dimension of) image to crop to
#' @param text Text annotation
#' @param text_colour Colour of text annotation
#'
#' @export
as_hex <- function(from, to, background = "#ffffff00", crop = 1, text = NULL,
                   text_colour = "white", text_size = 50) {

  # read the image
  image <- magick::image_read(path = from)

  # height and width
  height <- magick::image_info(image)$height
  width <- magick::image_info(image)$width

  # horizontal crop if needed
  if(width > height) {
    geom <- magick::geometry_area(height, height, (width-height)/2, 0)
    image <- magick::image_crop(image, geom)
    width <- height
  }

  # vertical crop if needed
  if(height > width) {
    geom <- magick::geometry_area(width, width, 0, (height-width)/2)
    image <- magick::image_crop(image, geom)
    height <- width
  }

  # now crop the square image if needed
  if(crop != 1) {
    geom <- magick::geometry_area(
      width*crop, height*crop, width*(1-crop)/2, height*(1-crop)/2
    )
    image <- magick::image_crop(image, geom)
    height <- height * crop
    width <- width * crop
  }

  trim_corner <- paste0("0x", round(height*.25))
  trim_edge <- paste0(round(height*.067), "x0")

  # set the background colour
  image <- magick::image_background(image, color = background, flatten = FALSE)

  # this is really inefficient, but it was the
  # first thing I thought of and I'm too lazy to
  # think of a better way right now...
  image <- magick::image_rotate(image, 30)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, -60)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, 240)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, -60)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_corner)
  image <- magick::image_rotate(image, 210)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_edge)
  image <- magick::image_rotate(image, 180)
  image <- magick::image_trim(image)
  image <- magick::image_chop(image, trim_edge)
  image <- magick::image_rotate(image, -180)
  image <- magick::image_trim(image)

  # add annotation
  if(!is.null(text)) {
    image <- magick::image_annotate(
      image = image,
      color = text_colour,
      text = text,
      gravity = "SouthWest",
      degrees = -30,
      size = text_size,
      location = magick::geometry_point(width/2-width/16, height/30)
    )
  }

  magick::image_write(image, path = to)
}
