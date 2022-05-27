new_empty_quosure <- function(expr) {
  rlang::new_quosure(expr, env = rlang::empty_env())
}
