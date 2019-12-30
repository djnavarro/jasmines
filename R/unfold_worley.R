#' Colour points using Worley noise
#'
#' @param data the data object
#' @param seed seed to pass to ambient::gen_worley()
#' @param output name of the primary unfolding variable to add (e.g., order)
#' @param ... arguments to pass to ambient::genworley()
#'
#' @export
unfold_worley <- function(data, seed = NULL, output = "order", ...) {
  if(is.null(seed)) seed <- getOption("jasmines.seed")
  data[[output]] <- ambient::gen_worley(data$x, data$y, seed = seed, ...)
  return(data)
}
