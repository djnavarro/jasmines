
#' @importFrom dplyr %>%
NULL

#' Creates a collage of noise samples
#'
#' @param generator the function (in ambient) to generate noise
#' @param palette name of a scico palette
#' @param grain how detailed is each panel
#' @param ... arguments to pass to the generator
#'
#' @return A ggplot object
#' @export
collage <- function(
  generator = ambient::noise_worley,
  palette = "tokyo",
  grain = 500,
  ...
) {

  make_frame <- function(generator, grain, fr, ...) {

    noise <- generator(dim = c(grain, grain), ...) %>%
      as.matrix() %>%
      tibble::as_tibble()

    noise$x <- 1:grain
    noise <- noise %>%
      tidyr::gather(key = y, value = value, -x) %>%
      dplyr::mutate(
        y = y %>%
          stringr::str_remove("V") %>%
          as.numeric(),
        fr = fr)

    return(noise)
  }

  noise <- purrr::map(
    .x = 1:9,
    .f = function(f){
        make_frame(
          generator = generator,
          grain = grain,
          fr = f,
          ...
        )
    }
  ) %>% purrr::reduce(dplyr::bind_rows)

  pic <- noise %>%
    ggplot2::ggplot(ggplot2::aes(x=x, y=y, colour=value)) +
    ggplot2::geom_raster(
      ggplot2::aes(fill=value),
      interpolate = TRUE,
      show.legend = FALSE) +
    scico::scale_fill_scico(palette = palette) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~fr) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank()
    )

  return(pic)
}

