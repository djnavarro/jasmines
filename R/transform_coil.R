#' Coil
#'
#' @param bridge bridge tibble
#' @param scale length of swirl
#' @export
coil <- function(
  bridge = time_meander(seed = 50, length = 50),
  scale = .02
) {

  bridge <- bridge %>%
    dplyr::mutate(len = sqrt(x^2 + y^2))

  curl <- ambient::curl_noise(
    generator = ambient::gen_simplex,
    x = bridge$x,
    y = bridge$y
  )

  bridge$x2 <- bridge$x + (curl$x * scale * bridge$len)
  bridge$y2 <- bridge$y + (curl$y * scale * bridge$len)

  return(bridge)
}
