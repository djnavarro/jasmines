#' Function factory for manual palettes
#'
#' @param ... colour names
#'
#' @return a function that takes arguments n and alpha
#' @export
palette_manual <- function(...) {
  colours <- c(...)
  palette <- function(n = 50, alpha = 1) {
    m <- ceiling(n/length(colours))
    cols <- as.vector(t(replicate(m, colours)))
    cols <- grDevices::adjustcolor(cols, alpha.f = 1)
    return(cols[1:n])
  }
  return(palette)
}


#' Function factory for adjusted palettes
#'
#' @param name Name of the base palette
#' @param prefix Vector of colours to preprend to the named palette
#' @param ... Arguments to be passed to grDevices::adjustcolor
#'
#' @return A modified
#' @export
#'
palette_adjust <- function(name, prefix, ...) {
  adjust <- list(...)
  return(function(n, alpha = 1, ...) {
    pal <- palette_named(name)
    adjust$col <- pal(n = n-length(prefix), alpha = alpha, ...)
    suffix <- do.call(what = grDevices::adjustcolor, args = adjust)
    return(c(prefix, suffix))
  })
}


#' Function factory for prespecified palettes
#'
#' @param name name of the palette
#' @param ... arguments to be passed to other functions
#'
#' @return a function that takes arguments n and alpha
#' @export
palette_named <- function(name = NULL, ...) {

  # names
  scico_names <- c(
    "acton", "bamako", "batlow", "berlin", "bilbao", "broc", "buda",
    "cork", "davos", "devon", "grayC", "hawaii", "imola", "lajolla",
    "lapaz", "lisbon", "nuuk", "oleron", "oslo", "roma", "tofino",
    "tokyo", "turku", "vik"
  )
  viridis_names <- c("magma", "inferno", "plasma", "viridis")
  jasmine_names <- c("ropensci", "blood", "base")
  base_names <- c("rainbow")
  all_names <- sort(c(scico_names, viridis_names, jasmine_names, base_names))

  # check input
  if(is.null(name)) {
    return(all_names)
  }
  if(!(name %in% all_names)) {
    stop("unknown palette name, type `palette_named()` for a list", call. = FALSE)
  }

  # return paletting function
  if(name %in% scico_names) {
    return(palette_scico(palette = name, ...))
  }
  if(name %in% viridis_names) {
    return(palette_viridis(option = name, ...))
  }
  if(name %in% jasmine_names) {
    if(name == "ropensci") return(palette_ropensci(...))
    if(name == "blood") return(palette_blood(...))
    if(name == "base") return(palette_base(...))
  }
  if(name %in% base_names) {
    if(name == "rainbow") return(palette_rainbow(...))
  }
}

palette_rainbow <- function(...) {
  function(n, alpha = 1) {grDevices::rainbow(n = n, alpha = alpha, ...)}
}

palette_scico <- function(...) {
  function(n, alpha = 1) scico::scico(n = n, alpha = alpha, ...)
}

palette_viridis <- function(...) {
  function(n, alpha = 1) viridis::viridis(n, alpha, ...)
}

palette_base <- function(...) {
  named_colours <- sample(grDevices::colours(), replace = TRUE)
  function(n, alpha = 1) {
    grDevices::adjustcolor(named_colours[1:n], alpha.f = alpha)
  }
}

palette_blood <- function(...) {
  return(palette_manual(
    "#7c0a02", "#92000a", "#880000", "#8a0303", "#8a0303", "#740707",
    "#560e07", "#490805", "#400303", "#220000", "#b00000", "#660000",
    "#7e3517"
  ))
}

palette_ropensci <- function(type = "plain", ...) {

  # the plain ropensci style
  if(type == "plain") {
    return(palette_manual(
      "#64B7AA", "#67BAB1", "#69BAB3", "#6EBEBA", "#71C1BC", "#74C3C1",
      "#79CAC7", "#7ACCC9", "#7ACCCF", "#7CCCC8", "#80CED0", "#80CFD3",
      "#88D2DD", "#8AD3DD", "#8AD3E4", "#8BD3DA", "#8CD4E1", "#8DD3D8",
      "#8DD4E0", "#9BD9E6", "#9CD8E0", "#9CD9E7", "#9FDAE4", "#A0DCF4",
      "#A3DDF8", "#A6DCE7", "#A6DDF0", "#A8DEED", "#ADE0F0", "#B1E1F1",
      "#B2E1ED", "#B9E3EC", "#BBE4ED", "#CCEAF1", "#CDEBF1", "#D4EDF3",
      "#E4F3F7"
    ))
  }

  # the plain ropensci style with whitening
  if(type == "whitened") {
    return(palette_manual(
      "#64B7AA", "#67BAB1", "#69BAB3", "#6EBEBA", "#71C1BC", "#74C3C1",
      "#79CAC7", "#7ACCC9", "#7ACCCF", "#7CCCC8", "#80CED0", "#80CFD3",
      "#88D2DD", "#8AD3DD", "#8AD3E4", "#8BD3DA", "#8CD4E1", "#8DD3D8",
      "#8DD4E0", "#9BD9E6", "#9CD8E0", "#9CD9E7", "#9FDAE4", "#A0DCF4",
      "#F8F8FF", "#F8F8FF", "#F8F8FF", "#F8F8FF", "#F8F8FF", "#F8F8FF", # patch of ghostwhite
      "#A3DDF8", "#A6DCE7", "#A6DDF0", "#A8DEED", "#ADE0F0", "#B1E1F1",
      "#B2E1ED", "#B9E3EC", "#BBE4ED", "#CCEAF1", "#CDEBF1", "#D4EDF3",
      "#E4F3F7"
    ))
  }

  stop("unknown palette `type`", call. = FALSE)
}
