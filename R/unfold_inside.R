
#' Unfold a scene with an inside operation
#'
#' @param data Data
#' @param output String specifying a Column name
#'
#' @return Returns the original data tibble with a new column added
#' @export
unfold_inside <- function(data, output = "inside") {

  is_inside <- function(x, y, time) {
    sp::point.in.polygon(x, y, x[time==1], y[time==1])
  }

  data %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(!!output := is_inside(x, y, time)) %>%
    dplyr::ungroup()
}
