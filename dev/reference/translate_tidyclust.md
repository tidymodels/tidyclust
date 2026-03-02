# Resolve a Model Specification for a Computational Engine

`translate_tidyclust()` will translate_tidyclust a model specification
into a code object that is specific to a particular engine (e.g. R
package). It translate tidyclust generic parameters to their
counterparts.

## Usage

``` r
translate_tidyclust(x, ...)

# Default S3 method
translate_tidyclust(x, engine = x$engine, ...)
```

## Arguments

- x:

  A model specification.

- ...:

  Not currently used.

- engine:

  The computational engine for the model (see
  [`?set_engine`](https://parsnip.tidymodels.org/reference/set_engine.html)).

## Value

Prints translated code.

## Details

`translate_tidyclust()` produces a *template* call that lacks the
specific argument values (such as `data`, etc). These are filled in once
[`fit()`](https://generics.r-lib.org/reference/fit.html) is called with
the specifics of the data for the model. The call may also include
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html) arguments
if these are in the specification. To handle the
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
arguments, you need to use the [tune
package](https://tune.tidymodels.org/). For more information see
<https://www.tidymodels.org/start/tuning/>

It does contain the resolved argument names that are specific to the
model fitting function/engine.

This function can be useful when you need to understand how `tidyclust`
goes from a generic model specific to a model fitting function.

**Note**: this function is used internally and users should only use it
to understand what the underlying syntax would be. It should not be used
to modify the cluster specification.
