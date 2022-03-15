library(celery)
library(dplyr)
library(rlang)
library(testthat)

# ------------------------------------------------------------------------------

# There's currently an issue comparing tibbles so we do it col-by-col
test_by_col <- function(a, b) {
  for (i in union(names(a), names(b))) {
    expect_equal(a[[i]], b[[i]])
  }
}

# ------------------------------------------------------------------------------

test_that('adding a new model', {
  set_new_model_celery("sponge")

  mod_items <- get_model_env_celery() %>% env_names()
  sponges <- grep("sponge", mod_items, value = TRUE)
  exp_obj <- c('sponge_modes', 'sponge_fit', 'sponge_args',
               'sponge_predict', 'sponge_pkgs', 'sponge')
  expect_equal(sort(sponges), sort(exp_obj))

  expect_equal(
    get_from_env_celery("sponge"),
    tibble(engine = character(0), mode = character(0))
  )

  test_by_col(
    get_from_env_celery("sponge_pkgs"),
    tibble(engine = character(0), pkg = list(), mode = character(0))
  )

  expect_equal(
    get_from_env_celery("sponge_modes"), "unknown"
  )

  test_by_col(
    get_from_env_celery("sponge_args"),
    dplyr::tibble(engine = character(0), celery = character(0),
                  original = character(0), func = vector("list"),
                  has_submodel = logical(0))
  )

  test_by_col(
    get_from_env_celery("sponge_fit"),
    tibble(engine = character(0), mode = character(0), value = vector("list"))
  )

  test_by_col(
    get_from_env_celery("sponge_predict"),
    tibble(engine = character(0), mode = character(0),
           type = character(0), value = vector("list"))
  )

  expect_snapshot(error = TRUE,set_new_model_celery())
  expect_snapshot(error = TRUE,set_new_model_celery(2))
  expect_snapshot(error = TRUE,set_new_model_celery(letters[1:2]))
})


# ------------------------------------------------------------------------------

test_that('adding a new mode', {
  set_model_mode_celery("sponge", "partition")

  expect_equal(get_from_env_celery("sponge_modes"), c("unknown", "partition"))

  expect_snapshot(error = TRUE,set_model_mode_celery("sponge"))

})


# ------------------------------------------------------------------------------

test_that('adding a new engine', {
  set_model_engine_celery("sponge", mode = "partition", eng = "gum")

  test_by_col(
    get_from_env_celery("sponge"),
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(get_from_env_celery("sponge_modes"), c("unknown", "partition"))

  expect_snapshot(error = TRUE,set_model_engine_celery("sponge", eng = "gum"))
  expect_snapshot(error = TRUE,set_model_engine_celery("sponge", mode = "partition"))
  expect_snapshot(error = TRUE,
    set_model_engine_celery("sponge", mode = "regression", eng = "gum")
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new package', {
  set_dependency_celery("sponge", "gum", "trident")

  expect_snapshot(error = TRUE,set_dependency_celery("sponge", "gum", letters[1:2]))
  expect_snapshot(error = TRUE,set_dependency_celery("sponge", "gummies", "trident"))
  expect_snapshot(error = TRUE,set_dependency_celery("sponge",  "gum", "trident", mode = "regression"))

  test_by_col(
    get_from_env_celery("sponge_pkgs"),
    tibble(engine = "gum", pkg = list("trident"), mode = "partition")
  )

  set_dependency_celery("sponge", "gum", "juicy-fruit", mode = "partition")
  test_by_col(
    get_from_env_celery("sponge_pkgs"),
    tibble(engine = "gum",
           pkg = list(c("trident", "juicy-fruit")),
           mode = "partition")
  )

  test_by_col(
    get_dependency_celery("sponge"),
    tibble(engine = "gum",
           pkg = list(c("trident", "juicy-fruit")),
           mode = "partition")
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new argument', {
  set_model_arg_celery(
    model = "sponge",
    eng = "gum",
    celery = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  set_model_arg_celery(
    model = "sponge",
    eng = "gum",
    celery = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  args <- get_from_env_celery("sponge_args")
  expect_equal(sum(args$celery == "modeling"), 1)

  test_by_col(
    get_from_env_celery("sponge_args"),
    tibble(engine = "gum", celery = "modeling", original = "modelling",
           func = list(list(pkg = "foo", fun = "bar")),
           has_submodel = FALSE)
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "lunchroom",
      eng = "gum",
      celery = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "modeling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "modeling",
      original = "modelling",
      func = "foo::bar",
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = 2
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar")
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "yodeling",
      original = "yodelling",
      func = c(foo = "a", bar = "b"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "yodeling",
      original = "yodelling",
      func = c(foo = "a"),
      has_submodel = FALSE
    )
  )

  expect_snapshot(error = TRUE,
    set_model_arg_celery(
      model = "sponge",
      eng = "gum",
      celery = "yodeling",
      original = "yodelling",
      func = c(fun = 2, pkg = 1),
      has_submodel = FALSE
    )
  )
})



# ------------------------------------------------------------------------------

test_that('adding a new fit', {
  fit_vals <-
    list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "foo", fun = "bar"),
      defaults = list()
    )

  set_fit_celery(
    model = "sponge",
    eng = "gum",
    mode = "partition",
    value = fit_vals
  )

  fit_env_data <- get_from_env_celery("sponge_fit")
  test_by_col(
    fit_env_data[ 1:2],
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(
    fit_env_data$value[[1]],
    fit_vals
  )

  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "cactus",
      eng = "gum",
      mode = "partition",
      value = fit_vals
    )
  )

  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "nose",
      mode = "partition",
      value = fit_vals
    )
  )

  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      value = fit_vals
    )
  )

  for (i in 1:length(fit_vals)) {
    expect_snapshot(error = TRUE,
      set_fit_celery(
        model = "sponge",
        eng = "gum",
        mode = "partition",
        value = fit_vals[-i]
      )
    )
  }

  fit_vals_0 <- fit_vals
  fit_vals_0$interface <- "loaf"
  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_0
    )
  )

  fit_vals_1 <- fit_vals
  fit_vals_1$defaults <- 2
  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_1
    )
  )

  fit_vals_2 <- fit_vals
  fit_vals_2$func <- "foo:bar"
  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_2
    )
  )

  fit_vals_3 <- fit_vals
  fit_vals_3$interface <- letters
  expect_snapshot(error = TRUE,
    set_fit_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      value = fit_vals_3
    )
  )

  test_by_col(
    get_fit_celery("sponge")[, 1:2],
    tibble(engine = "gum", mode = "partition")
  )

  expect_equal(
    get_fit_celery("sponge")$value[[1]],
    fit_vals
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new predict method', {
  cluster_vals <-
    list(
      pre = I,
      post = NULL,
      func = c(fun = "predict"),
      args = list(x = quote(2))
    )

  set_pred_celery(
    model = "sponge",
    eng = "gum",
    mode = "partition",
    type = "cluster",
    value = cluster_vals
  )

  pred_env_data <- get_from_env_celery("sponge_predict")
  test_by_col(
    pred_env_data[ 1:3],
    tibble(engine = "gum", mode = "partition", type = "cluster")
  )

  expect_equal(
    pred_env_data$value[[1]],
    cluster_vals
  )

  test_by_col(
    get_pred_type_celery("sponge", "cluster")[ 1:3],
    tibble(engine = "gum", mode = "partition", type = "cluster")
  )

  expect_equal(
    get_pred_type_celery("sponge", "cluster")$value[[1]],
    cluster_vals
  )

  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "cactus",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals
    )
  )

  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "nose",
      mode = "partition",
      type = "cluster",
      value = cluster_vals
    )
  )


  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "eggs",
      value = cluster_vals
    )
  )

  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      type = "cluster",
      value = cluster_vals
    )
  )

  for (i in 1:length(cluster_vals)) {
    expect_snapshot(error = TRUE,
      set_pred_celery(
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
  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_0
    )
  )

  cluster_vals_1 <- cluster_vals
  cluster_vals_1$post <- "I"
  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_1
    )
  )

  cluster_vals_2 <- cluster_vals
  cluster_vals_2$func <- "foo:bar"
  expect_snapshot(error = TRUE,
    set_pred_celery(
      model = "sponge",
      eng = "gum",
      mode = "partition",
      type = "cluster",
      value = cluster_vals_2
    )
  )

})

test_that('showing model info', {
  expect_snapshot(
    show_model_info_celery("k_means")
  )
})
