
#' Draw a ribbon image
#'
#' @param ribbon data frame with x, y, order, id, time
#' @param file path to tiff output file or NULL (the default) to display on screen
#' @param burnin how many iterations should we discard as burnin?
#' @param alpha_init how transparent is each line?
#' @param alpha_decay how does transparency decay over iterations?
#' @param line_width how wide are the lines?
#' @param seed_col what colour to draw the seed? (default: NULL)
#' @param seed_fill what colour to fill the seed? (default: NULL)
#' @param palette function generating a palette, or a list of such functions, with length equal to the number of objects in the seed
#' @param width width in pixels (default = 3000, for printing try 8000)
#' @param height height in pixels (default = 3000, for printing try 8000)
#' @param background colour of the background in the plot
#'
#' @export
style_ribbon <- function(
  ribbon,
  file = NULL,
  burnin = 0,      # how many of the iterations do we not draw?
  alpha_init = .3, # transparency of each line
  alpha_decay = 0, # rate of decay
  line_width = 6, # width of each line
  seed_col = NULL,
  seed_fill = NULL,
  palette = palette_viridis(), # function to generate palette (args: n, alpha)
  width = 3000, # width in pixels ()
  height = 3000,
  background = "black"
) {

  # the seed is the first element of
  seed <- ribbon %>% dplyr::filter(time == 1)
  iterations <- max(ribbon$time) - 1

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
      al = alpha_init +  (1 - alpha_decay)^(time - 1)
    )

  # get colour values
  col_set <- palette(n = max(ribbon$order))

  # create basic object
  pic <- ggplot2::ggplot(
    data = ribbon,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      alpha = al,
      group = id,
      colour = factor(order)
    )
  ) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::scale_color_manual(values = col_set) +
    theme_mono(background)

  return(pic)
}

# }
#
#   # create the png device if requested
#   if(!is.null(file)) {
#     grDevices::png(
#       filename = file,
#       width = width,
#       height = height,
#       bg = background
#     )
#   }
#
#   # setup the plot area
#   op <- graphics::par(bg = background, pty = "s", mar = c(0,0,0,0))
#   graphics::plot.new()
#   graphics::plot.window(xlim = box[1:2], ylim = box[3:4])
#
#   # plot a series of curl iterations
#   for(i in 1:iterations) {
#     if(i > burnin) {
#
#       last <- ribbon %>% dplyr::filter(time == i)
#       this <- ribbon %>% dplyr::filter(time == i+1)
#
#       # if palette is a single function...
#       if(class(palette) == "function") {
#
#         # create a colour palette for this iteration
#         cols <- palette(nrow(seed), (alpha_init) * (1 - alpha_decay)^(i-1))
#
#         # supply a default order if the input lacks one
#         if(!("order" %in% names(this))) {
#           this$order <- 1:length(this$x)
#         }
#
#         # draw the segments
#         graphics::segments(
#           x0 = (last$x - xmin) / (xmax - xmin),
#           y0 = (last$y - ymin) / (ymax - ymin),
#           x1 = (this$x - xmin) / (xmax - xmin),
#           y1 = (this$y - ymin) / (ymax - ymin),
#           col = cols[this$order],
#           lwd = line_width,
#         )
#
#         # if it is a list of functions...
#       } else {
#
#         # create the list of colour palettes for this iteration
#         cols <- list();
#         for(j in 1:max(seed$id)) {
#           cols[[j]] <- palette[[j]](nrow(seed), (alpha_init) * (1 - alpha_decay)^(i-1))
#         }
#
#         # draw each object separately with its own palette
#         for(j in 1:max(seed$id)) {
#
#           last <- ribbon %>% dplyr::filter(time == i & id == j)
#           this <- ribbon %>% dplyr::filter(time == i+1 & id == j)
#
#           # supply a default order if the input lacks one
#           if(!("order" %in% names(this))) {
#             this$order <- 1:length(this$x)
#           }
#
#           # draw the segments
#           graphics::segments(
#             x0 = (last$x - xmin) / (xmax - xmin),
#             y0 = (last$y - ymin) / (ymax - ymin),
#             x1 = (this$x - xmin) / (xmax - xmin),
#             y1 = (this$y - ymin) / (ymax - ymin),
#             col = cols[[j]][this$order],
#             lwd = line_width,
#           )
#         }
#       }
#     }
#   }
#
#
#   # fill in the seed shape if requested
#   if(!is.null(seed_fill)) {
#     for(i in 1:max(seed$id)) {
#       s <- seed[seed$id == i,]
#       graphics::polygon(
#         x = (s$x - xmin)/(xmax - xmin),
#         y = (s$y - ymin)/(ymax - ymin),
#         col = seed_fill,
#         lwd= 1
#       )
#     }
#   }
#
#   # draw outline of seed shape if requested
#   if(!is.null(seed_col)) {
#     for(i in 1:max(seed$id)) {
#       s <- seed[seed$id == i,]
#       graphics::lines(
#         x = (s$x - xmin)/(xmax - xmin),
#         y = (s$y - ymin)/(ymax - ymin),
#         col = seed_col,
#         lwd= line_width * 1.5
#       )
#     }
#   }
# }