#' Animated GIF of Sydney skyline
#'
#' @param file path to gif
#' @details Creates an animated gif of the Sydney skyline. If \code{file}
#' is specified it generates the gif, otherwise it creates the sequence
#' of plots (useful if calling within R Markdown files)
#' @export
skyline <- function(file = NULL) {

  pb <- progress::progress_bar$new(total = 34)

  # initialise the grob tree
  skygrob <- make_skygrob()

  # some convenient variables
  np <- length(skygrob$children)
  part_name <- names(skygrob$children)
  part_colr <- grDevices::rainbow(np, s = .4)

  # function to change the colour of the windows
  window_light <- function(object, name, col) {
    return(grid::editGrob(
      grob = object,
      gPath = paste0("windows_", name),
      gp = grid::gpar(fill = col),
      grep = TRUE,
      global = TRUE,
      warn = FALSE
    ))
  }

  draw_state <- function(skygrob, times = 1) {
    for(i in 1:times) {
      grid::grid.newpage()
      grid::grid.draw(skygrob)
      pb$tick()
    }
  }

  cycle_windows <- function(skygrob, times = 1) {
    for(i in 1:times) {
      for(p in sample(np)) {
        skygrob <- window_light(skygrob, part_name[p], part_colr[p])
        draw_state(skygrob)
        skygrob <- window_light(skygrob, part_name[p], "black")
      }
    }
  }

  flash_windows <- function(skygrob, times = 1, length = 2) {
    for(i in 1:times) {
      for(p in 1:np) {
        skygrob <- window_light(skygrob, part_name[p], part_colr[p])
      }
      draw_state(skygrob, length)
      for(p in 1:np) {
        skygrob <- window_light(skygrob, part_name[p], "black")
      }
      draw_state(skygrob, length)
    }
  }



  # write the animation: if a file name is specified,
  # write the results to a GIF...
  if(!is.null(file)) {
    animation::saveGIF(
      expr = {

        draw_state(skygrob, 2)
        cycle_windows(skygrob)
        draw_state(skygrob, 4)
        flash_windows(skygrob, 3)
        draw_state(skygrob, 2)

      },
      movie.name = file,
      interval = .2
    )

  # or, if no file is specified, assume the calling function
  # knows what to do with a sequence of plots (e.g., .Rmd) so
  # just produce those outputs...
  } else {

    draw_state(skygrob, 2)
    cycle_windows(skygrob)
    draw_state(skygrob, 4)
    flash_windows(skygrob, 3)
    draw_state(skygrob, 2)

  }

  return(invisible(NULL))
}



# vividify S4 -------------------------------------------------------------


#' @importClassesFrom grImport Picture

# create an S4 generic function
methods::setGeneric(
  name =  "vividify",
  def  = function(object, ...) {
    standardGeneric("vividify")
  }
)

# S4 vividify method for PictureText objects is simple: just
# grobify the underlying object
methods::setMethod(
  "vividify",
  signature(object = "PictureText"),
  function (object, ...) {
    grImport::grobify(object)
  }
)

# S4 vividify method for PictureFill objects is a little more
# complicated. The objects need to be labelled in terms of
# whether they are part of the silhouette (black in the base
# image) or one of the windows (other colours)
methods::setMethod(
  "vividify",
  signature(object = "PictureFill"),
  function (object, partname, partfill, ...) {

    # label all the grobs in terms of whether they
    # describe a window or part of the silhouette
    prefix <- get_prefix(object@rgb, partname)

    # create the grob
    out <- grid::pathGrob(
      x = object@x, y = object@y,
      default.units="native",
      name = grid::grobName(prefix = prefix),
      gp = grid::gpar(col = NA, fill = partfill),
      ...
    )
    return(out)
  }
)


# helper function to generate the prefix: slihouette_ if the
# colour is black, window_ for other colours
get_prefix <- function(col, name) {
  if(col == "#000000") {
    return(paste0("silhouette_", name))
  }
  return(paste0("windows_", name))
}






# get the image and parts -------------------------------------------------

# read the raw file, write the xml file to a temp directory
# then import the xml file as a picture. this seems inefficient
# but i don't know a better way
get_image <- function() {

  tmp_path <- tempdir()
  zip_path <- system.file("extdata", "skyline.zip", package = "jasmines", mustWork = TRUE)
  eps_path <- file.path(tmp_path, "original.eps")
  xml_path <- file.path(tmp_path, "251102-P4FWZV-557.xml")
  utils::unzip(zip_path, exdir = tmp_path)
  grImport::PostScriptTrace(file = eps_path, outfilename = xml_path)
  return(grImport::readPicture(xml_path))
}

# return a list that specifies which elements of the picture
# belong to the same "part" of the image
get_parts <- function() {
  return(list(
    building0 = 5:80,
    sydney_tower = 81:117,
    building1 = 118:216,
    building2 = 218:392,
    building3 = 393:481,
    building4 = 482:538,
    building5 = 539:648,
    building6 = 649:867,
    building7 = 868:1091,
    building8 = 1092:1207,
    building9 = 1208:1289,
    building10 = 1290:1334,
    scene_base = c(3:4, 1335, 1345),
    opera_house = c(1336:1344, 1346:1364)
  ))
}

# function that adds a "part" of the scene to the grob
# tree that specifies the overall scene
add_part <- function(object, part, name) {
  return(grid::addGrob(
    gTree = object,
    child = grImport::pictureGrob(
      picture = part,
      xscale = c(0, 5000),
      yscale = c(0, 5000),
      FUN = vividify,
      name = name,
      partname = name,
      partfill = "black"
    )
  ))
}

# construct the scene
make_skygrob <- function() {

  img_basic <- get_image()
  img_parts <- get_parts()

  skygrob <- grid::gTree()

  for(p in 1:length(img_parts)) {
    skygrob <- add_part(
      object = skygrob,
      part = img_basic[img_parts[[p]]],
      name = names(img_parts[p])
    )
  }

  return(skygrob)
}


# snippet of code from the original version that assigns
# each child in the grob tree it's own viewport and allows
# the entire viewport to be shifted. it doesn't work with
# the current version but i want to cannibalise it later

# # create random y positions
# scene$x_pos <- .5
# scene$y_pos <- runif(np) * .9 + .05
#
# # give each of the parts its own viewport
# for(p in scene$part) {
#   skygrob$children[[p]]$vp <- grid::viewport()
# }
#
# # define the grob drawing function
# construct_part <- function(data, x=NA, y=NA) {
#   grid::editGrob(
#     skygrob$children[[data$part]],
#     vp = grid::viewport(x = data$x_pos, y = data$y_pos)
#   )
# }




