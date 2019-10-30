#' Transform a temporal object by time reversing it
#'
#' @param data the object
#' @param append should the time reversed version be appended (loop back) or substituted?
#'
#' @return The same object but with time indices reversed
#' @export
time_reverse <- function(data, append = FALSE) {
  max_time <- max(data$time)
  y <- data
  if(append) {
    y$time <- 2*max_time - y$time + 1
    y <- dplyr::arrange(y, time)
    return(dplyr::bind_rows(data,y))
  } else {
    y$time <- max_time - y$time + 1
    y <- dplyr::arrange(y, time)
    return(y)
  }
}
