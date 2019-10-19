
#' @importFrom magrittr %>%
NULL

utils::globalVariables(c("x", "y", "value"))
utils::globalVariables(c("Horizontal", "Vertical", "Series", "Time"))
utils::globalVariables(c(".", "V1", "V2", "V3", "V4", "V5", "V6", "aes",
                         "char_ind", "id", "series", "time", "x2", "xpos", "y2", "ypos"))


#
# img_rescale <- function(from, to, scale) {
#   im <- magick::image_read(from)
#   im <- magick::image_resize(
#     image = im,
#     geometry = magick::geometry_size_percent(scale))
#   magick::image_write(im, to)
# }
#
# img_convert <- function(from, to, format) {
#   im <- magick::image_read(from)
#   im <- magick::image_convert(im, format)
#   magick::image_write(im, to)
# }
#
# make_allvers <- function(file) {
#   tiff8000 <- file
#   tiff4000 <- gsub("8000", "4000", tiff8000, fixed = TRUE)
#   tiff2000 <- gsub("8000", "2000", tiff8000, fixed = TRUE)
#   png8000 <- gsub(".tiff", ".png", tiff8000, fixed = TRUE)
#   png4000 <- gsub("8000", "4000", png8000, fixed = TRUE)
#   png2000 <- gsub("8000", "2000", png8000, fixed = TRUE)
#
#   cat("."); img_rescale(tiff8000, tiff4000, scale = 50)
#   cat("."); img_rescale(tiff8000, tiff2000, scale = 25)
#   cat("."); img_convert(tiff8000, png8000, format = "png")
#   cat("."); img_rescale(png8000, png4000, scale = 50)
#   cat("."); img_rescale(png8000, png2000, scale = 25)
#
# }
#
