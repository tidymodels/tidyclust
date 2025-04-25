# https://github.com/tidymodels/parsnip/blob/main/R/engine_docs.R

#' Knit engine-specific documentation
#' @param pattern A regular expression to specify which files to knit. The
#' default knits all engine documentation files.
#' @return A tibble with column `file` for the file name and `result` (a
#' character vector that echos the output file name or, when there is
#' a failure, the error message).
#' @keywords internal
#' @export
knit_engine_docs <- function(pattern = NULL) {
  rmd_files <- list.files("man/rmd", pattern = "\\.Rmd", full.names = TRUE)

  if (!is.null(pattern)) {
    target_exists <- grepl(pattern, rmd_files)
    files <- rmd_files[target_exists]
  } else {
    files <- rmd_files[!grepl("(template-)|(setup\\.)|(aaa\\.)", rmd_files)]
  }
  outputs <- gsub("Rmd$", "md", files)

  res <- map2(files, outputs, \(.x, .y) try(knitr::knit(.x, .y), silent = TRUE))
  is_error <- map_lgl(res, \(.x) inherits(.x, "try-error"))

  if (any(is_error)) {
    # In some cases where there are issues, the md file is empty.
    errors <- res[which(is_error)]
    error_nms <- basename(files)[which(is_error)]
    errors <-
      map_chr(errors, \(.x) cli::ansi_strip(as.character(.x))) |>
      map2_chr(error_nms, \(.x, .y) paste0(.y, ": ", .x)) |>
      map_chr(\(.x) gsub("Error in .f(.x[[i]], ...) :", "", .x, fixed = TRUE))
    cat("There were failures duing knitting:\n\n")
    cat(errors)
    cat("\n\n")
  }

  res <- map_chr(res, as.character)

  issues <- list_md_problems()
  if (nrow(issues) > 0) {
    cat("There are some issues with the help files:\n")
    print(issues)
  }

  invisible(tibble::tibble(file = basename(files), result = res))
}

#' Locate and show errors/warnings in engine-specific documentation
#' @return A tibble with column `file` for the file name, `line` indicating
#'   the line where the error/warning occurred, and `problem` showing the
#'   error/warning message.
#' @keywords internal
#' @export
list_md_problems <- function() {
  md_files <- list.files("man/rmd", pattern = "\\.md", full.names = TRUE)

  get_errors <- function(file) {
    lines <- readLines(file)
    line <- grep("## (Error|Warning)", lines)
    problem <- lines[line]
    tibble::tibble(basename(file), line, problem)
  }

  map(md_files, get_errors) |> vctrs::vec_rbind()
}
