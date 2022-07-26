# adding a new model

    Code
      set_new_model_tidyclust()
    Error <rlang_error>
      Please supply a character string for a model name (e.g. `'k_means'`)

---

    Code
      set_new_model_tidyclust(2)
    Error <rlang_error>
      Please supply a character string for a model name (e.g. `'k_means'`)

---

    Code
      set_new_model_tidyclust(letters[1:2])
    Error <rlang_error>
      Please supply a character string for a model name (e.g. `'k_means'`)

# adding a new mode

    Code
      set_model_mode_tidyclust("sponge")
    Error <rlang_error>
      Please supply a character string for a mode (e.g. `'partition'`).

# adding a new engine

    Code
      set_model_engine_tidyclust("sponge", eng = "gum")
    Error <rlang_error>
      Please supply a character string for a mode (e.g. `'partition'`).

---

    Code
      set_model_engine_tidyclust("sponge", mode = "partition")
    Error <rlang_error>
      Please supply a character string for an engine name (e.g. `'stats'`)

---

    Code
      set_model_engine_tidyclust("sponge", mode = "regression", eng = "gum")
    Error <rlang_error>
      'regression' is not a known mode for model `sponge()`.

# adding a new package

    Code
      set_dependency_tidyclust("sponge", "gum", letters[1:2])
    Error <rlang_error>
      Please supply a single character value for the package name.

---

    Code
      set_dependency_tidyclust("sponge", "gummies", "trident")
    Error <rlang_error>
      The engine 'gummies' has not been registered for model 'sponge'.

---

    Code
      set_dependency_tidyclust("sponge", "gum", "trident", mode = "regression")
    Error <rlang_error>
      mode 'regression' is not a valid mode for 'sponge'

# adding a new argument

    Code
      set_model_arg_tidyclust(model = "lunchroom", eng = "gum", tidyclust = "modeling",
        original = "modelling", func = list(pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Error <rlang_error>
      Model `lunchroom` has not been registered.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "modeling",
        func = list(pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Error <rlang_error>
      Please supply a character string for the argument.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", original = "modelling",
        func = list(pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Error <rlang_error>
      Please supply a character string for the argument.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "modeling",
        original = "modelling", func = "foo::bar", has_submodel = FALSE)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "modeling",
        original = "modelling", func = list(pkg = "foo", fun = "bar"), has_submodel = 2)
    Error <rlang_error>
      The `submodels` argument should be a single logical.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "modeling",
        original = "modelling", func = list(pkg = "foo", fun = "bar"))
    Error <simpleError>
      argument "has_submodel" is missing, with no default

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "yodeling",
        original = "yodelling", func = c(foo = "a", bar = "b"), has_submodel = FALSE)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "yodeling",
        original = "yodelling", func = c(foo = "a"), has_submodel = FALSE)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

---

    Code
      set_model_arg_tidyclust(model = "sponge", eng = "gum", tidyclust = "yodeling",
        original = "yodelling", func = c(fun = 2, pkg = 1), has_submodel = FALSE)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

# adding a new fit

    Code
      set_fit_tidyclust(model = "cactus", eng = "gum", mode = "partition", value = fit_vals)
    Error <rlang_error>
      Model `cactus` has not been registered.

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "nose", mode = "partition", value = fit_vals)
    Error <rlang_error>
      Engine 'nose' is not supported for `sponge()`. See `show_engines('sponge')`.

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "frog", value = fit_vals)
    Error <rlang_error>
      'frog' is not a known mode for model `sponge()`.

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals[
        -i])
    Error <rlang_error>
      The `fit` module should have elements: `defaults`, `func`, `interface`, `protect`

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals[
        -i])
    Error <rlang_error>
      The `fit` module should have elements: `defaults`, `func`, `interface`, `protect`

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals[
        -i])
    Error <rlang_error>
      The `fit` module should have elements: `defaults`, `func`, `interface`, `protect`

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals[
        -i])
    Error <rlang_error>
      The `fit` module should have elements: `defaults`, `func`, `interface`, `protect`

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals_0)
    Error <rlang_error>
      The `interface` element should have a single value of: `data.frame`, `formula`, `matrix`

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals_1)
    Error <rlang_error>
      The `defaults` element should be a list: 

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals_2)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

---

    Code
      set_fit_tidyclust(model = "sponge", eng = "gum", mode = "partition", value = fit_vals_3)
    Error <rlang_error>
      The `interface` element should have a single value of: `data.frame`, `formula`, `matrix`

# adding a new predict method

    Code
      set_pred_tidyclust(model = "cactus", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals)
    Error <rlang_error>
      Model `cactus` has not been registered.

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "nose", mode = "partition", type = "cluster",
        value = cluster_vals)
    Error <rlang_error>
      Engine 'nose' is not supported for `sponge()`. See `show_engines('sponge')`.

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "eggs",
        value = cluster_vals)
    Error <rlang_error>
      The prediction type should be one of: 'cluster', 'raw'

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "frog", type = "cluster",
        value = cluster_vals)
    Error <rlang_error>
      'frog' is not a known mode for model `sponge()`.

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals[-i])
    Error <rlang_error>
      The `predict` module should have elements: `args`, `func`, `post`, `pre`

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals[-i])
    Error <rlang_error>
      The `predict` module should have elements: `args`, `func`, `post`, `pre`

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals[-i])
    Error <rlang_error>
      The `predict` module should have elements: `args`, `func`, `post`, `pre`

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals[-i])
    Error <rlang_error>
      The `predict` module should have elements: `args`, `func`, `post`, `pre`

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals_0)
    Error <rlang_error>
      The `pre` module should be null or a function: 

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals_1)
    Error <rlang_error>
      The `post` module should be null or a function: 

---

    Code
      set_pred_tidyclust(model = "sponge", eng = "gum", mode = "partition", type = "cluster",
        value = cluster_vals_2)
    Error <rlang_error>
      `func` should be a named vector with element 'fun' and the optional  elements 'pkg', 'range', 'trans', and 'values'. `func` and 'pkg' should both be single character strings.

# showing model info

    Code
      show_model_info_tidyclust("k_means")
    Output
      Information for `k_means`
       modes: unknown, partition 
      
       engines: 
         partition: ClusterR, stats
      
       arguments: 
         stats:    
            num_clusters --> centers
         ClusterR: 
            num_clusters --> clusters
      
       fit modules:
           engine      mode
            stats partition
         ClusterR partition
      
       prediction modules:
              mode   engine methods
         partition ClusterR cluster
         partition    stats cluster
      

