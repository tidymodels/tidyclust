new_empty_quosure <- function(expr) {
  new_quosure(expr, env = empty_env())
}
