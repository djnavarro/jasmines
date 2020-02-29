# scene generators: "scenes" in the jasmines package are collections of
# entities. they have the same variables (x, y, id, type), but whereas
# all points in an entity have the same id, a scene consists of multiple
# entities that can have different ids


# helper function to create the entity
entify <- function(.x) {
  if(methods::existsFunction(.x$fn)) {
    return(do.call(.x$fn, .x)) # <- TODO: possibly wrap in try() ???
  }

  warning(
    "Could not find function '", .x$fn,
    "', treating as entity_null()", call. = FALSE)
  return(entity_null())
}



#' Create a scene with entities on a grid
#'
#' @param seed Seed number to attach
#' @param xpos Numeric vector specifying horizontal locations
#' @param ypos Numeric vector specifying vertical locations
#' @param entity The entity type (e.g., "line", "circle")
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
  seed = use_seed(1),
  xpos = 1:3,
  ypos = 1:3,
  entity = "circle",
  grain = 50,
  size = 1,
  shape = 3,
  angle = 0
) {

  # check that xpos & ypos are numeric
  if(!is.numeric(xpos)) {stop("`xpos must be numeric", call. = FALSE)}
  if(!is.numeric(ypos)) {stop("`ypos must be numeric", call. = FALSE)}

  # create grid
  sc <- expand.grid(xpos = xpos, ypos = ypos)

  # check lengths of entity parameter vectors
  if(!(length(entity) %in% c(1, nrow(sc)))) {stop("`entity` must be length 1 or equal to the number of entities in the grid", call. = FALSE)}
  if(!(length(grain)  %in% c(1, nrow(sc)))) {stop( "`grain` must be length 1 or equal to the number of entities in the grid", call. = FALSE)}
  if(!(length(size)   %in% c(1, nrow(sc)))) {stop(  "`size` must be length 1 or equal to the number of entities in the grid", call. = FALSE)}
  if(!(length(shape)  %in% c(1, nrow(sc)))) {stop( "`shape` must be length 1 or equal to the number of entities in the grid", call. = FALSE)}
  if(!(length(angle)  %in% c(1, nrow(sc)))) {stop( "`angle` must be length 1 or equal to the number of entities in the grid", call. = FALSE)}

  # add the name of the function to be called
  sc$fn <- paste("entity", entity, sep="_")

  # add the other parameters
  sc$id <- 1:nrow(sc)
  sc$grain <- grain
  sc$size <- size
  sc$angle <- angle
  sc$shape <- shape
  sc$seed <- seed

  # map
  sc <- purrr::transpose(sc)
  sc <- purrr::map_dfr(sc, entify)
  return(sc)

}



#' Create a scene with entities placed randomly
#'
#' @param seed Seed number to attach
#' @param n Number of entities
#' @param xpos Numeric vector specifying possible horizontal locations
#' @param ypos Numeric vector specifying possible vertical locations
#' @param entity Character vector specifying possible entity types (e.g., "line", "circle")
#' @param grain Numeric vector specifying possible grains
#' @param size Numeric vector specifying possible sizes
#' @param shape Numeric vector specifying possible shapes
#' @param angle Numeric vector specifying possible angles
#'
#' @return A tibble with four columns: x, y, id and type
#' @export
scene_mix <- function(
  seed = use_seed(1),
  n = 5,
  xpos = (1:20)/4,
  ypos = (1:20)/4,
  entity = c("circle", "line", "heart", "droplet"),
  grain = 100,
  size = (10:20)/20,
  shape = 3,
  angle = seq(0, 2*pi, length.out = 20)
) {

  # check that xpos & ypos are numeric
  if(!is.numeric(xpos))  {stop("`xpos must be numeric", call. = FALSE)}
  if(!is.numeric(ypos))  {stop("`ypos must be numeric", call. = FALSE)}
  if(!is.numeric(grain)) {stop("`grain must be numeric", call. = FALSE)}
  if(!is.numeric(size))  {stop("`size must be numeric", call. = FALSE)}
  if(!is.numeric(shape)) {stop("`shape must be numeric", call. = FALSE)}
  if(!is.numeric(angle)) {stop("`angle must be numeric", call. = FALSE)}

  # sampler that doesn't misbehave for singleton input
  do_sample <- function(x, size) {
    if(length(x) == 1) {return(rep.int(x, size))}
    return(sample(x, size = size, replace = TRUE))
  }

  sc <- tibble::tibble(
    fn = paste("entity", do_sample(entity, size = n), sep="_"),
    xpos   = do_sample(xpos,  size = n),
    ypos   = do_sample(ypos,  size = n),
    grain  = do_sample(grain, size = n),
    size   = do_sample(size,  size = n),
    shape  = do_sample(shape, size = n),
    angle  = do_sample(angle, size = n),
    seed   = seed
  )
  sc$id <- 1:nrow(sc)

  # map
  sc <- purrr::transpose(sc)
  sc <- purrr::map_dfr(sc, entify)
  return(sc)

}

