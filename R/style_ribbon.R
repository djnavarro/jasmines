
#' Style as a ribbon image
#'
#' @param data data frame with x, y, order, id, time
#' @param burnin how many iterations should we discard as burnin?
#' @param alpha_init how transparent is each line?
#' @param alpha_decay how does transparency decay over iterations?
#' @param seed_col what colour to draw the seed? (default: NULL)
#' @param seed_fill what colour to fill the seed? (default: NULL)
#' @param palette function generating a palette
#' @param background colour of the background in the plot
#' @param type type of geom to use ("segment", "curve" or "point")
#' @param ... arguments to pass to geom
#'
#' @return Returns a ggplot2 object
#' @export
style_ribbon <- function(
  data,
  burnin = 0,      # how many of the iterations do we not draw?
  alpha_init = .3, # transparency of each line
  alpha_decay = 0, # rate of decay
  seed_col = NULL,
  seed_fill = NULL,
  palette = palette_viridis(), # function to generate palette (args: n, alpha)
  background = "black",
  type = "segment",
  ...
) {

  ribbon <- data

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

  # the seed is the first element of the ribbon
  seed <- ribbon %>% dplyr::filter(time == 1)

  ribbon2 <- ribbon %>%
    dplyr::rename(xend = x, yend = y) %>%
    dplyr::mutate(time = time - 1) %>%
    dplyr::filter(time > 0)

  ribbon <- ribbon %>%
    dplyr::filter(time < max(time))

  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend
  ribbon$order <- ribbon2$order

  #return(ribbon)

  # get colour values
  col_set <- palette(n = max(ribbon$order))

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



  # add hollow fill for seed if requested
  if(!is.null(seed_fill)) {
    pic <- pic +
      ggplot2::geom_polygon(
        data = seed,
        mapping = ggplot2::aes(x = x, y = y, group = factor(id)),
        inherit.aes = FALSE,
        colour = seed_fill,
        fill = seed_fill,
        show.legend = FALSE)
  }

  # add outline for seed if requested
  if(!is.null(seed_col)) {
    pic <- pic +
      ggplot2::geom_path(
        data = seed,
        mapping = ggplot2::aes(x = x, y = y, group = factor(id)),
        inherit.aes = FALSE,
        colour = seed_col,
        show.legend = FALSE)
  }


  return(pic)
}


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
