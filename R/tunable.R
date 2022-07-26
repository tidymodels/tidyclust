# Lazily registered in .onLoad()
# Unit tests are in extratests
# nocov start
tunable_cluster_spec <- function(x, ...) {
  mod_env <- rlang::ns_env("tidyclust")$tidyclust

  if (is.null(x$engine)) {
    abort("Please declare an engine first using `set_engine()`.", call. = FALSE)
  }

  arg_name <- paste0(mod_type(x), "_args")
  if (!(any(arg_name == names(mod_env)))) {
    abort(
      paste(
        "The `tidyclust` model database doesn't know about the arguments for ",
        "model `", mod_type(x), "`. Was it registered?",
        sep = ""
        ),
      call. = FALSE
    )
  }

  arg_vals <-
    mod_env[[arg_name]] %>%
    dplyr::filter(engine == x$engine) %>%
    dplyr::select(name = tidyclust, call_info = func) %>%
    dplyr::full_join(
      tibble::tibble(name = c(names(x$args), names(x$eng_args))),
      by = "name"
    ) %>%
    dplyr::mutate(
      source = "cluster_spec",
      component = mod_type(x),
      component_id = dplyr::if_else(name %in% names(x$args), "main", "engine")
    )

  if (nrow(arg_vals) > 0) {
    has_info <- map_lgl(arg_vals$call_info, is.null)
    rm_list <- !(has_info & (arg_vals$component_id == "main"))

    arg_vals <- arg_vals[rm_list, ]
  }
  arg_vals %>% dplyr::select(name, call_info, source, component, component_id)
}

mod_type <- function(.mod) class(.mod)[class(.mod) != "cluster_spec"][1]

add_engine_parameters <- function(pset, engines) {
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    engine_names <- pset$name[is_engine_param]
    pset <- pset[!is_engine_param, ]
    pset <-
      dplyr::bind_rows(pset, engines %>% dplyr::filter(name %in% engines$name))
  }
  pset
}

# Lazily registered in .onLoad()
tunable_k_means <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "stats") {
    res <- add_engine_parameters(res, stats_k_means_engine_args)
  }
  res
}

stats_k_means_engine_args <-
  tibble::tibble(
    name = c(
      "centers"
    ),
    call_info = list(
      list(pkg = "tidyclust", fun = "num_clusters")
    ),
    source = "cluster_spec",
    component = "k_means",
    component_id = "engine"
  )

# nocov end
