
# inserts plausible.io trackers into pkgdown site pages

site <- "jasmines.djnavarro.net"

# lines to be inserted into header
new_lines <- c(
  ' ',
  '<!-- adds plausible.io -->',
  paste0('<script async defer data-domain="', site, '" src="https://plausible.io/js/plausible.js"></script>'),
  ' '
)

# files into which lines need insertion
html_files <- list.files(
  path = here::here("docs"),
  pattern = "html$",
  recursive = TRUE,
  full.names = TRUE
)

# function to insert lines
insert_lines <- function(file, new_lines) {
  lines <- brio::read_lines(file)
  ind <- stringr::str_which(lines, "</head>")
  if(is.null(ind)) rlang::warn(paste0("no </head> line found in: ", file))
  if(length(ind) > 1) rlang::warn(paste0("multiple </head> lines found in: ", file))

  # don't add lines if the tracker is already there
  tracker_ind <- stringr::str_which(lines, stringr::fixed(site))
  if(length(tracker_ind) == 0) {

    # assume file doesn't begin or end with </head>
    lines <- c(
      lines[1:(ind[1]-1)],
      new_lines,
      lines[ind[1]:length(lines)]
    )
    brio::write_lines(lines, file)

  }
}

# insert for all files
purrr::walk(html_files, insert_lines, new_lines = new_lines)
