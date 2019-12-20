# scene generators


# helper
entify <- function(.x) {do.call(.x$fn, .x)}

#' Create a scene with multiple entities laid out in a grid
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param entity The entity type (e.g., "line")
#' @param grain The number of points per entity
#' @param size The size of each entity
#' @param shape The shape of each entity
#' @param angle The angle of each entity
#'
#' @return A tibble with four columns: x, y, id and type
#' @details The \code{scene_grid()} function allows multiple entities to be
#' included in the initial object, laying out items in grid.
#' @export
scene_grid <- function(
  entity,
  nrow = 3,
  ncol = 3,
  grain = 50,
  size = 1,
  shape = 3,
  angle = 0
) {

  # create data frame with xpos and ypos values
  x <- (1:ncol) - ncol/2
  y <- (1:nrow) - nrow/2
  sc <- expand.grid(xpos = x, ypos = y)

  # add the name of the function to be called
  sc$fn <- paste("entity", entity, sep="_")

  # add the other parameters
  sc$id <- 1:(nrow*ncol)
  sc$grain <- grain
  sc$size <- size
  sc$angle <- angle
  sc$shape <- shape

  # map
  sc <- purrr::transpose(sc)
  sc <- purrr::map_dfr(sc, entify)
  return(sc)

}
