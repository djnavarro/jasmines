#' Save to file
#'
#' @param input art object to print
#' @param file filename
#' @param background defaults to black
#' @param width defaults to 3000 pixels
#' @param height defaults to 3000 pixels
#'
#' @export
art_write <- function(
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
#
#
# ### if the input is a ggplot 2 object
#
#   # create the png if requested
#   if(!is.null(file)) {
#
#   }
#
#
# ## if it's gganimate
#
# # just in case
# op <- graphics::par(bg = background)
#
# # create
# if(!is.null(file)) {
#   pic %>% gganimate::animate(
#     nframes = 200,
#     detail = 5,
#     type = "cairo"
#   )
#   gganimate::anim_save(file)
#   graphics::par(op)
#
