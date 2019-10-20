
#' Seed shaped like a heart
#'
#' @param n number of dot points
#'
#' @return tibble
#' @export
seed_heart <- function(n = 100) {
  t <- seq(0, 2*pi, length.out = n)
  tibble::tibble(
    x = (16*sin(t)^3) / 17,
    y = (13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)) / 17,
    id = 1
  )
}



#' Seed shaped from text
#'
#' @param text the message
#'
#' @return a tibble
#' @export
seed_text <- function(text) {
  char_map <- make_dotted_letters()
  char_map <- dplyr::filter(char_map, value == 1)
  char_set <- stringr::str_split(text, "", simplify = TRUE)
  dots <- purrr::map_dfr(
    .x = char_set,
    .f = ~ dplyr::filter(char_map, char == .x),
    .id = "char_ind"
  ) %>%
    dplyr::mutate(
      char_ind = as.numeric(char_ind),
      x = x + 6 * char_ind,
      id = 1:nrow(.)
    ) %>%
    dplyr::mutate(  # to match scale used in bridges
      x = x/10,
      y = y/10
    ) %>%
    dplyr::select(char, char_ind, x, y, id)
  return(dots)
}


#' Seed from a set of random sticks
#'
#' @param n how many sticks
#' @param grain how many points along each stick
#'
#' @return a tibble with columns x, y and id
#' @export
seed_sticks <- function(n = 10, grain = 1000) {
  make_stick <- function(id, grain) {
    return(tibble::tibble(
      x = seq(stats::runif(1), stats::runif(1), length.out = grain),
      y = seq(stats::runif(1), stats::runif(1), length.out = grain),
      id = id
    ))
  }
  points <- purrr::map_dfr(1:n, make_stick, grain = grain)
  return(points)
}

#' Seed with evenly spaced rows
#'
#' @param n how many rows
#' @param grain how many points along each row
#'
#' @return a tibble with columns x, y and id
#' @export
seed_rows <- function(n = 10, grain = 1000) {
  make_row <- function(id, grain) {
    return(tibble::tibble(
      x = seq(0, 1, length.out = grain),
      y = id/(n+1),
      id = id
    ))
  }
  points <- purrr::map_dfr(1:n, make_row, grain = grain)
  return(points)
}



#' Seed with a random set of bubbles
#'
#' @param n how many bubbles
#' @param grain how many points along each stick
#'
#' @return a tibble with columns x, y and id
#' @export
seed_bubbles <- function(n = 2, grain = 1000) {

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
  return(points)
}
