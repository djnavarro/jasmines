
#' Create a scene comprised of concentric circles
#'
#' @param seed Seed number to attach
#' @param points Total number of interior points
#' @param rings How many rings to spread across?
#' @param size Diameter of the outermost ring
#' @export
scene_discs <- function(seed = use_seed(1), points = 100, rings = 3, size = 2) {
  radius <- (size/2) * (1:rings)/rings
  circumference <- 2 * pi * radius
  proportion <- circumference / sum(circumference)
  counts <- round(points * proportion)

  unfold <- function(radius, grain, id) {

    theta <- seq(0, 2*pi, length.out = grain + 1)
    theta <- theta[-1]
    return(tibble::tibble(
      x = radius * cos(theta),
      y = radius * sin(theta),
      id = id
    ))
  }

  points <- purrr::pmap_dfr(
    .l = list(radius, counts, 1:length(radius)),
    .f = unfold
  )
  points$type <- "circle"
  points$seed <- seed
  return(points)
}


#' Create a scene comprised of lines of varying length and orientation
#'
#' @param seed Seed number to attach
#' @param n how many sticks
#' @param grain how many points along each stick
#'
#' @return a tibble with columns x, y and id
#' @export
scene_sticks <- function(seed = use_seed(1), n = 10, grain = 100) {
  make_stick <- function(id, grain) {
    return(tibble::tibble(
      x = seq(stats::runif(1), stats::runif(1), length.out = grain),
      y = seq(stats::runif(1), stats::runif(1), length.out = grain),
      id = id
    ))
  }
  points <- purrr::map_dfr(1:n, make_stick, grain = grain)
  points$type <- "line"
  points$seed <- seed
  return(points)
}


#' Create a scene comprised of horizontal or vertical lines
#'
#' @param seed Seed number to attach
#' @param n Number of rows
#' @param grain The number of points per row
#' @param vertical Flip the x/y co-ords to produce columns?
#'
#' @return A tibble with columns: x, y, id, type, seed
#' @export
scene_rows <- function(seed = use_seed(1), n = 10, grain = 100, vertical = FALSE) {
  make_row <- function(id, grain, vertical = FALSE) {
    if(!vertical) {
    return(tibble::tibble(
      x = seq(0, 1, length.out = grain),
      y = id/(n+1),
      id = id
    ))
    } else{
      return(tibble::tibble(
        x = id/(n+1),
        y = seq(1, 0, length.out = grain),
        id = id
      ))
    }
  }
  points <- purrr::map_dfr(1:n, make_row, grain = grain, vertical = vertical)
  points$type <- "line"
  points$seed <- seed
  return(points)
}


#' Create a scene comprised of circles of varying size and location
#'
#' @param seed Seed number to attach
#' @param n Number of circles
#' @param grain The number of points per row
#'
#' @return A tibble with four columns: x, y, id and type
#' @export
scene_bubbles <- function(seed = use_seed(1), n = 2, grain = 100) {

  make_bubble <- function(id, grain) {

    radius <- stats::runif(1)
    origin_x <- stats::runif(1)
    origin_y <- stats::runif(1)
    th <- seq(0, 2*pi, length.out = grain)

    return(tibble::tibble(
      x = radius * cos(th) + origin_x,
      y = radius * sin(th) + origin_y,
      id = id
    ))
  }

  points <- purrr::map_dfr(1:n, make_bubble, grain = grain)
  points$type <- "circle"
  points$seed <- seed
  return(points)
}







