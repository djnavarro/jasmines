#' Unfold a scene with a worley operation
#'
#' @param data the data object
#' @param scatter should the noise seed be "scattered"?
#' @param output name of the primary unfolding variable to add (e.g., order)
#' @param ... arguments to pass to ambient::genworley()
#'
#' @export
unfold_worley <- function(data, scatter = FALSE, output = "order", ...) {
  if(scatter == TRUE) {
    data[[output]] <- ambient::gen_worley(data$x, data$y, ...)
  } else {
    data[[output]] <- ambient::gen_worley(data$x, data$y, seed = data$seed[1], ...)
  }
  return(data)
}
