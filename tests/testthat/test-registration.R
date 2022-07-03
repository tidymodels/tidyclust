test_that("adding a new model", {
  set_new_model_tidyclust("sponge")

  mod_items <- get_model_env_tidyclust() %>% rlang::env_names()
  sponges <- grep("sponge", mod_items, value = TRUE)
  exp_obj <- c(
    "sponge_modes", "sponge_fit", "sponge_args",
    "sponge_predict", "sponge_pkgs", "sponge"
  )
  expect_identical(sort(sponges), sort(exp_obj))

  expect_identical(
    get_from_env_tidyclust("sponge"),
    tibble(engine = character(0), mode = character(0))
  )

  expect_identical(
    get_from_env_tidyclust("sponge_pkgs"),
    tibble(engine = character(0), pkg = list(), mode = character(0))
  )

  expect_identical(
    get_from_env_tidyclust("sponge_modes"), "unknown"
  )

  expect_identical(
    get_from_env_tidyclust("sponge_args"),
    dplyr::tibble(
      engine = character(0), tidyclust = character(0),
      original = character(0), func = vector("list"),
      has_submodel = logical(0)
    )
  )

  expect_identical(
    get_from_env_tidyclust("sponge_fit"),
    tibble(engine = character(0), mode = character(0), value = vector("list"))
  )

  expect_identical(
    get_from_env_tidyclust("sponge_predict"),
    tibble(
      engine = character(0), mode = character(0),
      type = character(0), value = vector("list")
    )
  )

  expect_snapshot(error = TRUE, set_new_model_tidyclust())
  expect_snapshot(error = TRUE, set_new_model_tidyclust(2))
  expect_snapshot(error = TRUE, set_new_model_tidyclust(letters[1:2]))
})

test_that("adding a new mode", {
  set_model_mode_tidyclust("sponge", "partition")

  expect_equal(get_from_env_tidyclust("sponge_modes"), c("unknown", "partition"))

  expect_snapshot(error = TRUE, set_model_mode_tidyclust("sponge"))
})

test_that("adding a new engine", {
  set_model_engine_tidyclust("sponge", mode = "partition", eng = "gum")

  expect_identical(
    get_from_env_tidyclust("sponge"),
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(get_from_env_tidyclust("sponge_modes"), c("unknown", "partition"))

  expect_snapshot(error = TRUE, set_model_engine_tidyclust("sponge", eng = "gum"))
  expect_snapshot(error = TRUE,
    set_model_engine_tidyclust("sponge", mode = "partition")
  )
  expect_snapshot(
    error = TRUE,
    set_model_engine_tidyclust("sponge", mode = "regression", eng = "gum")
  )
})

test_that("adding a new package", {
  set_dependency_tidyclust("sponge", "gum", "trident")

  expect_snapshot(error = TRUE,
    set_dependency_tidyclust("sponge", "gum", letters[1:2])
  )
  expect_snapshot(error = TRUE,
    set_dependency_tidyclust("sponge", "gummies", "trident")
  )
  expect_snapshot(error = TRUE,
    set_dependency_tidyclust("sponge", "gum", "trident", mode = "regression")
  )

  expect_identical(
    get_from_env_tidyclust("sponge_pkgs"),
    tibble(engine = "gum", pkg = list("trident"), mode = "partition")
  )

  set_dependency_tidyclust("sponge", "gum", "juicy-fruit", mode = "partition")
  expect_identical(
    get_from_env_tidyclust("sponge_pkgs"),
    tibble(
      engine = "gum",
      pkg = list(c("trident", "juicy-fruit")),
      mode = "partition"
    )
  )

  expect_identical(
    get_dependency_tidyclust("sponge"),
    tibble(
      engine = "gum",
      pkg = list(c("trident", "juicy-fruit")),
      mode = "partition"
    )
  )
})

test_that("adding a new argument", {
  set_model_arg_tidyclust(
    model = "sponge",
    eng = "gum",
    tidyclust = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  set_model_arg_tidyclust(
    model = "sponge",
    eng = "gum",
    tidyclust = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  args <- get_from_env_tidyclust("sponge_args")
  expect_equal(sum(args$tidyclust == "modeling"), 1)

  expect_identical(
    get_from_env_tidyclust("sponge_args"),
    tibble(
      engine = "gum", tidyclust = "modeling", original = "modelling",
      func = list(list(pkg = "foo", fun = "bar")),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "lunchroom",
      eng = "gum",
      tidyclust = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "modeling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "modeling",
      original = "modelling",
      func = "foo::bar",
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = 2
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar")
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "yodeling",
      original = "yodelling",
      func = c(foo = "a", bar = "b"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "yodeling",
      original = "yodelling",
      func = c(foo = "a"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(
    error = TRUE,
    set_model_arg_tidyclust(
      model = "sponge",
      eng = "gum",
      tidyclust = "yodeling",
      original = "yodelling",
      func = c(fun = 2, pkg = 1),
      has_submodel = FALSE
    )
  )
})

test_that("adding a new fit", {
  fit_vals <-
    list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "foo", fun = "bar"),
      defaults = list()
    )

  set_fit_tidyclust(
    model = "sponge",
    eng = "gum",
    mode = "partition",
    value = fit_vals
  )

  fit_env_data <- get_from_env_tidyclust("sponge_fit")
  expect_identical(
    fit_env_data[1:2],
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(
    fit_env_data$value[[1]],
    fit_vals
  )

  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "cactus",
      eng = "gum",
      mode = "partition",
      value = fit_vals
    )
  )

  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "nose",
      mode = "partition",
      value = fit_vals
    )
  )

  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      value = fit_vals
    )
  )

  for (i in seq_along(fit_vals)) {
    expect_snapshot(
      error = TRUE,
      set_fit_tidyclust(
        model = "sponge",
        eng = "gum",
        mode = "partition",
        value = fit_vals[-i]
      )
    )
  }

  fit_vals_0 <- fit_vals
  fit_vals_0$interface <- "loaf"
  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_0
    )
  )

  fit_vals_1 <- fit_vals
  fit_vals_1$defaults <- 2
  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_1
    )
  )

  fit_vals_2 <- fit_vals
  fit_vals_2$func <- "foo:bar"
  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_2
    )
  )

  fit_vals_3 <- fit_vals
  fit_vals_3$interface <- letters
  expect_snapshot(
    error = TRUE,
    set_fit_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_3
    )
  )

  expect_identical(
    get_fit_tidyclust("sponge")[, 1:2],
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(
    get_fit_tidyclust("sponge")$value[[1]],
    fit_vals
  )
})

test_that("adding a new predict method", {
  cluster_vals <-
    list(
      pre = I,
      post = NULL,
      func = c(fun = "predict"),
      args = list(x = quote(2))
    )

  set_pred_tidyclust(
    model = "sponge",
    eng = "gum",
    mode = "partition",
    type = "cluster",
    value = cluster_vals
  )

  pred_env_data <- get_from_env_tidyclust("sponge_predict")
  expect_identical(
    pred_env_data[1:3],
    tibble(engine = "gum", mode = "partition", type = "cluster")
  )

  expect_equal(
    pred_env_data$value[[1]],
    cluster_vals
  )

  expect_identical(
    get_pred_type_tidyclust("sponge", "cluster")[1:3],
    tibble(engine = "gum", mode = "partition", type = "cluster")
  )

  expect_equal(
    get_pred_type_tidyclust("sponge", "cluster")$value[[1]],
    cluster_vals
  )

  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "cactus",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals
    )
  )

  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "nose",
      mode = "partition",
      type = "cluster",
      value = cluster_vals
    )
  )


  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "eggs",
      value = cluster_vals
    )
  )

  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      type = "cluster",
      value = cluster_vals
    )
  )

  for (i in seq_along(cluster_vals)) {
    expect_snapshot(
      error = TRUE,
      set_pred_tidyclust(
        model = "sponge",
        eng = "gum",
        mode = "partition",
        type = "cluster",
        value = cluster_vals[-i]
      )
    )
  }

  cluster_vals_0 <- cluster_vals
  cluster_vals_0$pre <- "I"
  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_0
    )
  )

  cluster_vals_1 <- cluster_vals
  cluster_vals_1$post <- "I"
  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_1
    )
  )

  cluster_vals_2 <- cluster_vals
  cluster_vals_2$func <- "foo:bar"
  expect_snapshot(
    error = TRUE,
    set_pred_tidyclust(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_2
    )
  )
})

test_that("showing model info", {
  expect_snapshot(
    show_model_info_tidyclust("k_means")
  )
})
