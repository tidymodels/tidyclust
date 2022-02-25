# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

check_args.default <- function(object) {
  invisible(object)
}
