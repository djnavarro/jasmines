# time_reverse <- function(data, append = FALSE) {
#   max_time <- max(data$time)
#   y <- data
#   if(append) {
#     y$time <- 2*max_time - y$time + 1
#     y <- dplyr::arrange(y, time)
#     return(dplyr::bind_rows(data,y))
#   } else {
#     y$time <- max_time - y$time + 1
#     y <- dplyr::arrange(y, time)
#     return(y)
#   }
# }
