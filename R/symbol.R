# nocov start

# For use in setting the `tidyclust_symbol` active binding in `.onLoad()`

tidyclust_symbol_utf8 <- list(
  "success" = "\u2713"
)

tidyclust_symbol_windows <- list(
  "success" = "\u221A"
)

tidyclust_symbol_ascii <- list(
  "success" = "v"
)

## -----------------------------------------------------------------------------

# To remove the crayon dependency, use the cli analogs. However, these
# produce ansi_string objects and some of our logging code needs
# the character values. Will not be needed for cli >= 2.1.0.9000
yellow <- function(...) as.character(cli::col_yellow(...))
black <- function(...) as.character(cli::col_black(...))
white <- function(...) as.character(cli::col_white(...))
red <- function(...) as.character(cli::col_red(...))
yellow <- function(...) as.character(cli::col_yellow(...))
green <- function(...) as.character(cli::col_green(...))
blue <- function(...) as.character(cli::col_blue(...))
silver <- function(...) as.character(cli::col_silver(...))
bold <- function(...) as.character(cli::style_bold(...))

# ------------------------------------------------------------------------------

# For use in setting the `tidyclust_color` active binding in `.onLoad()`

tidyclust_color_dark <- list(
  symbol = list(
    "warning" = yellow,
    "go" = white,
    "danger" = red,
    "success" = green,
    "info" = blue
  ),
  message = list(
    "warning" = yellow,
    "go" = white,
    "danger" = red,
    "success" = white,
    "info" = white
  )
)

tidyclust_color_light <- list(
  symbol = list(
    "warning" = yellow,
    "go" = black,
    "danger" = red,
    "success" = green,
    "info" = blue
  ),
  message = list(
    "warning" = yellow,
    "go" = black,
    "danger" = red,
    "success" = black,
    "info" = black
  )
)

# ------------------------------------------------------------------------------

# cli:::is_latex_output()
is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }

  get("is_latex_output", asNamespace("knitr"))()
}

# cli:::is_windows()
is_windows <- function() {
  .Platform$OS.type == "windows"
}

# nocov end

#' Get colors for tidyclust text.
#'
#' @keywords internal
#' @return a list of `cli` functions.
#' @export
#' @rdname empty_ellipses
get_tidyclust_colors <- function() tidyclust_color
