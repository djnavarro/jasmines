


# entity types ------------------------------------------------------------

#' Entity types
#'
#' @param seed Parameter specifying seed (default = NULL)
#' @param grain The number of points that comprise the entity
#' @param id A numeric identifier for the entity
#' @param shape Parameter controlling the shape of the entity (droplet, lissajous)
#' @param start Parameter controlling a start location for a line (lissajous)
#' @param end Parameter controlling an end location for a line (lissajous)
#' @param ... Parameters to be passed to locate_entity
#'
#' @details Primitive entities in jasmines are tibbles with four columns: x and y specify
#' co-ordinate values, the id is a number identifying the object, and the type is
#' a character label indicating what kind of entity it is. By default, entities are
#' assigned a random integer as the id code, but it is often wise for the calling
#' function to assign the id in a more predictable fashion. The shape parameter can sometimes
#' be a list.
#'
#' @return A tibble with four columns: x, y, id and type
#'
#' @name entitytype
NULL

radians <- function(grain, closed = TRUE) {
  if(closed) return(seq(0, 2*pi, length.out = grain))
  return(seq(0, 2*pi, length.out = grain + 1)[1:grain])
}

#' @export
#' @rdname entitytype
entity_circle <- function(seed = use_seed(1), grain = 50, id = NULL, ...) {
  th <- radians(grain)
  x <- cos(th)/2
  y <- sin(th)/2
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "circle")
  entity <- locate_entity(entity, ...)
  return(entity)
}

#' @export
#' @rdname entitytype
entity_line <- function(seed = use_seed(1), grain = 50, id = NULL, ...) {
  x <- seq(-.5, .5, length.out = grain)
  y <- rep.int(0, times = grain)
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "line")
  entity <- locate_entity(entity, ...)
  return(entity)
}

#' @export
#' @rdname entitytype
entity_heart <- function(seed = use_seed(1), grain = 50, id = NULL, ...) {
  th <- radians(grain)
  x <- (16 * sin(th)^3) / 17
  y <- (13 * cos(th) - 5 * cos(2*th) - 2 * cos(3*th) - cos(4*th)) / 17
  x <- x - mean(x)
  y <- y - mean(y)
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "heart")
  entity <- locate_entity(entity, ...)
  return(entity)
}

#' @export
#' @rdname entitytype
entity_droplet <- function(seed = use_seed(1), grain = 50, id = NULL, shape = 3, ...) {
  th <- radians(grain)
  x <- sin(th) * (sin(th/2))^shape
  y <- cos(th)
  x <- x - mean(x)
  y <- y - mean(y)
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "droplet")
  entity <- locate_entity(entity, ...)
  return(entity)
}

#' @export
#' @rdname entitytype
entity_lissajous <- function(seed = use_seed(1), grain = 500, id = NULL, start = 0, end = 30,
                             shape = list(a = 1, b = 1, w = .3, d = 1), ...) {
  t <- seq(start, end, length.out = grain)
  x <- shape$a * sin(shape$w * t + shape$d)
  y <- shape$b * sin(t)
  x <- x - mean(x)
  y <- y - mean(y)
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "lissajous")
  entity <- locate_entity(entity, ...)
  return(entity)
}

#' @export
#' @rdname entitytype
entity_gaussian <- function(seed = use_seed(1), grain = 50, id = NULL, ...) {
  set.seed(seed)
  x <- stats::rnorm(n = grain)
  y <- stats::rnorm(n = grain)
  entity <- new_entity(x = x, y = y, id = id, seed = seed, type = "gaussian")
  entity <- locate_entity(entity, ...)
  return(entity)
}


#' @export
#' @rdname entitytype
entity_null <- function(seed = use_seed(1), ...) {
  entity <- new_entity(x = numeric(0), y = numeric(0),
                       id = numeric(0), type = "null", seed = numeric(0))
  return(entity)
}



# manipulate entities -----------------------------------------------------

#' Locate entities
#'
#' @param entity The entity to be placed
#' @param xpos The horizontal location of the entity
#' @param ypos The vertical location of the entity
#' @param size Parameter controlling the size of the entity
#' @param angle Parameter controlling the orientation of the entity
#' @param ... Other arguments are ignored
#'
#' @return A tibble with four columns: x, y, id and type
#' @details When a jasmine entity is created it is implicitly assumed to be
#' located at the origin (xpos = 0, ypos = 0), to have size 1, and to have
#' a horizontal orientation (angle = 0). The locate_entity function allows the
#' entity to be transformed in simple ways: translation, dilation and rotations
#' @export
locate_entity <- function(entity, xpos = 0, ypos = 0, size = 1, angle = 0, ...) {

  xx <- entity$x
  yy <- entity$y

  # apply rotatation
  entity$x <- xx * cos(angle) - yy * sin(angle)
  entity$y <- yy * cos(angle) + xx * sin(angle)

  # apply shift and scale
  entity$x <- (size * entity$x) + xpos
  entity$y <- (size * entity$y) + ypos

  return(entity)
}



# entity constructor ------------------------------------------------------

new_entity <- function(x, y, id = NULL, type = NULL, seed = NULL) {

  # set default values for id & type
  if(is.null(id)) id <- round(stats::runif(1) * 1000000)
  if(is.null(type)) type <- NA_character_

  # type must always be a single string
  if(length(type) != 1) {stop("`type` for a jasmine entity must be length 1", call. = FALSE)}
  if(!is.character(type)) {stop("`type` label for a jasmine entity must be character", call. = FALSE)}

  # check input lengths
  if(type == "null") {
    if(length(x) != 0) {stop("`x` values for entity_null must be empty", call. = FALSE)}
    if(length(y) != 0) {stop("`y` values for entity_null must be empty", call. = FALSE)}
    if(length(id) != 0) {stop("`id` for entity_null must be empty", call. = FALSE)}
  } else {
    if(length(x) != length(y)) {stop("`x` and `y` co-ordinates for a jasmine entity must be equal length", call. = FALSE)}
    if(length(id) != 1) {stop("`id` for a jasmine entity must be length 1", call. = FALSE)}
  }

  # check input types
  if(!is.numeric(x)) {stop("`x` co-ordinate for a jasmine entity must be numeric", call. = FALSE)}
  if(!is.numeric(y)) {stop("`y` co-ordinate for a jasmine entity must be numeric", call. = FALSE)}
  if(!is.numeric(id)) {stop("`id` code for a jasmine entity must be numeric", call. = FALSE)}

  # construct the object and return
  entity <- tibble::tibble(x = x, y = y, ind = 1:length(x), id = id, type = type, seed = seed)
  return(entity)
}



