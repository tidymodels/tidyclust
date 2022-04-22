## -----------------------------------------------------------------------------

# To remove the crayon dependency, use the cli analogs. However, these
# produce ansi_string objects and some of our logging code needs
# the character values. Will not be needed for cli >= 2.1.0.9000
yellow <- function(...) as.character(cli::col_yellow(...))
black  <- function(...) as.character(cli::col_black(...))
white  <- function(...) as.character(cli::col_white(...))
red    <- function(...) as.character(cli::col_red(...))
yellow <- function(...) as.character(cli::col_yellow(...))
green  <- function(...) as.character(cli::col_green(...))
blue   <- function(...) as.character(cli::col_blue(...))
silver <- function(...) as.character(cli::col_silver(...))
bold   <- function(...) as.character(cli::style_bold(...))

# ------------------------------------------------------------------------------

# For use in setting the `celery_color` active binding in `.onLoad()`

celery_color_dark <- list(
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

celery_color_light <- list(
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


get_celery_colors <- function() celery_color
