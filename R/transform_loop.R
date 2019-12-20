# # utility function to generate points from 0 to 2\pi
# radians <- function(npoints) {
#   seq(0, 2*pi, length.out = npoints + 1)[1:npoints]
# }

#' Take a seed object and convert each point to a loop
#'
#' @param data data
#' @param points number of time points
#' @param radius radius of the circle
#'
#' @return a tibble with x, y, id and time
#' @export
time_loop <- function(data, points = 20, radius = 1) {

  enloop <- function(df) {
    th <- radians(points)
    return(tibble::tibble(
      x = radius * cos(th) + df$x,
      y = radius * sin(th) + df$y,
      time = 1:points,
      id = df$id
    ))
  }

  looped_data <- data %>%
    purrr::transpose() %>%
    purrr::map_dfr(~enloop(.x))

  return(looped_data)
}
