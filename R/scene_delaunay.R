#' Create a scene using Delaunay triangulation
#'
#' @param seed The RNG seed
#' @param n Number of vertices
#' @param grain Number of points along each line
#'
#' @return A tibble with four columns: x, y, id and type
#' @export
scene_delaunay <- function(seed = use_seed(1), n = 20, grain = 50) {

	# create the delaunay tiling
	points <- spatstat.core::runifpoint(n)
	del <- spatstat.geom::delaunay(points)
	del <- as.data.frame(del)
	names(del) <- c("x", "y", "id")
	del$id <- gsub("Tile ", "", del$id)
	del$id <- as.numeric(del$id)

	# define an interpolation function
	interp <- function(a, b) {

		# don't interpolate lines in different tiles
		if(a$id != b$id) {
			return(tibble::tibble(x = numeric(0), y = numeric(0), id = numeric(0)))
		}

		# otherwise...
		tibble::tibble(
			x = seq(a$x, b$x, length.out = grain),
			y = seq(a$y, b$y, length.out = grain),
			id = rep(a$id, grain)
		)
	}

	del <- purrr::transpose(del)
	del <- purrr::map2_dfr(del[-1], del[-length(del)], interp)
	del$type <- "triangle"
  del$seed <- seed

	return(del)

}



