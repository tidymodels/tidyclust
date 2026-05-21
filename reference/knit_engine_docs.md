# Knit engine-specific documentation

Knit engine-specific documentation

## Usage

``` r
knit_engine_docs(pattern = NULL)
```

## Arguments

- pattern:

  A regular expression to specify which files to knit. The default knits
  all engine documentation files.

## Value

A tibble with column `file` for the file name and `result` (a character
vector that echos the output file name or, when there is a failure, the
error message).
