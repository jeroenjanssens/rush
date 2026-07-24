#' Main entry point for rush
#'
#' @param ... character vector of parameters
#'
#' @importFrom tibble tribble
#' @export
rush <- function(...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse flags
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  flags <- parse_arguments(...)

  if (flags$verbose) {
    cli::cat_rule("Arguments", file = stderr())
    cli::cat_bullet(purrr::map2(names(flags), flags, format_flag),
                    bullet_col = "yellow", file = stderr())
    cli::cat_rule(file = stderr())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build the body of the script
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # The generated script is self-contained: it is executed by `ir run` in a
  # separate process, so every package it needs is declared in its own `#|`
  # frontmatter and every helper it uses is inlined. `rush()` itself only
  # assembles and launches the script.

  body_file <- tempfile()
  body <- file(body_file, open = "w")
  on.exit(unlink(body_file), add = TRUE)

  # Packages resolved by `ir` (via pak) before the script runs. The output
  # dispatch at the end of every script relies on cli, tibble, and readr.
  pkgs <- c("rlang", "cli", "tibble", "readr")

  if (is.integer(flags$seed)) {
    code_expression(body, set.seed(!!flags$seed))
  }

  if (flags$command == "run") {
    if (is.null(flags$expression) && length(flags$file) == 0) {
      cli::cli_abort(c(
        "No expression to run.",
        i = "Provide an R expression, e.g. {.code rush run '1 + 1'}.",
        i = "See {.code rush run -h} for usage."
      ))
    }

    # Load libraries
    if (flags$tidyverse) {
      code_library(body, "tidyverse")
      code_library(body, "glue")
      pkgs <- c(pkgs, "tidyverse", "glue")
    }
    if (!is.null(flags$library)) {
      purrr::walk(flags$library, function(e) code_library(body, e))
      pkgs <- c(pkgs, as.character(flags$library))
    }

    # Read files
    if (length(flags$file) >= 1) {
      pkgs <- c(pkgs, emit_read_files(body, flags$file, flags))
    }

    # Add expressions, capturing the value of the last one as `result`. With
    # no expression and a single file, echo the data frame that was read.
    if (!is.null(flags$expression)) {
      emit_result_exprs(body, flags$expression)
    } else if (length(flags$file) == 1) {
      code_expression(body, result <- df)
    }
  }

  if (flags$command == "plot") {
    pkgs <- c(pkgs, "ggplot2", "fs",
              "github::coolbutuseless/devout",
              "github::jeroenjanssens/miniansi",
              "github::coolbutuseless/devoutansi")

    if (flags$tidyverse) {
      code_library(body, "tidyverse")
      code_library(body, "glue")
      pkgs <- c(pkgs, "tidyverse", "glue")
    } else {
      code_library(body, "ggplot2")
    }
    if (!is.null(flags$library)) {
      purrr::walk(flags$library, function(e) code_library(body, e))
      pkgs <- c(pkgs, as.character(flags$library))
    }

    # Default to standard input when no file is given. Multiple files are read
    # into a `dfs` list; combine them into `df` yourself with, e.g.,
    # --pre 'df <- dplyr::bind_rows(dfs)'.
    plot_files <- flags$file %||% "-"
    if (length(plot_files) == 0) plot_files <- "-"
    pkgs <- c(pkgs, emit_read_files(body, plot_files, flags))

    if (!is.null(flags$pre)) {
      purrr::walk(flags$pre, function(e) code_expression(body, !!e))
    }

    # Build the aesthetic mapping from the dedicated aesthetic flags, plus any
    # extra aesthetics supplied through --aes.
    aes_names <- c("x", "y", "z", "color", "alpha", "shape", "group", "size",
                   "fill")
    aes_call <- rlang::call2("aes", !!!purrr::compact(flags[aes_names]))
    if (!is.null(flags$aes)) {
      aes_call <- rlang::call_modify(aes_call, !!!flags$aes, .homonyms = "last")
    }

    plot_call <- rlang::call2("ggplot", rlang::sym("df"), aes_call)

    # Pick a geom. As qplot did, guess point when a y column is given and a
    # histogram otherwise; --geom overrides the guess.
    geom <- flags$geom
    if (geom == "auto") {
      geom <- if (!is.null(flags$y)) "point" else "histogram"
    }
    plot_call <- rlang::call2("+", plot_call,
                              rlang::call2(paste0("geom_", geom)))

    # Log-transform the requested axes.
    if (!is.null(flags$log)) {
      if (stringr::str_detect(flags$log, "x")) {
        plot_call <- rlang::call2("+", plot_call, rlang::call2("scale_x_log10"))
      }
      if (stringr::str_detect(flags$log, "y")) {
        plot_call <- rlang::call2("+", plot_call, rlang::call2("scale_y_log10"))
      }
    }

    # Facet. A two-sided formula (row ~ col) uses facet_grid, which also
    # supports marginal facets; a one-sided formula (~ var) uses facet_wrap.
    if (!is.null(flags$facets)) {
      if (length(flags$facets) == 3) {
        facet_call <- rlang::call2("facet_grid", flags$facets)
        if (isTRUE(flags$margins)) {
          facet_call <- rlang::call_modify(facet_call, margins = TRUE)
        }
      } else {
        facet_call <- rlang::call2("facet_wrap", flags$facets)
      }
      plot_call <- rlang::call2("+", plot_call, facet_call)
    }

    # Axis labels and title.
    labs_args <- purrr::compact(list(x = flags$xlab, y = flags$ylab,
                                     title = flags$title))
    if (length(labs_args) > 0) {
      plot_call <- rlang::call2("+", plot_call,
                                rlang::call2("labs", !!!labs_args))
    }

    if (!is.null(flags$post)) {
      # Keep the plot in `p` so --post code can extend it, then let the last
      # --post expression produce the result.
      code_expression(body, !!rlang::call2("<-", rlang::sym("p"), plot_call))
      emit_result_exprs(body, flags$post)
    } else {
      code_expression(body, !!rlang::call2("<-", rlang::sym("result"), plot_call))
    }
  }

  close(body)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble the self-contained script
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  filename <- tempfile(fileext = ".R")
  on.exit(unlink(filename), add = TRUE)

  writeLines(c(
    frontmatter(unique(pkgs)),
    "",
    script_preamble(flags),
    "",
    readLines(body_file),
    "",
    dispatch_block(flags$command)
  ), filename)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run (or, with --dry-run, print) the script
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (flags$dry_run) {
    code <- readLines(filename)
    if (isatty(stdout())) code <- prettycode::highlight(code)
    cat(code, sep = "\n")
    return(invisible())
  }

  ir <- Sys.which("ir")
  if (ir == "") {
    cli::cli_abort(c(
      "The {.pkg ir} command-line tool could not be found on the {.envvar PATH}.",
      i = "See {.url https://r-lib.github.io/ir/} for installation instructions."
    ))
  }

  # When rush is itself launched as an `ir` tool, the launcher sets
  # R_DEFAULT_PACKAGES to rush's own default packages and that value would be
  # inherited by the child. Reset it to R's normal default set so the
  # self-contained script sees the usual base packages (e.g. datasets).
  default_pkgs <- "datasets,utils,grDevices,graphics,stats,methods"

  # Inherit the parent's stdin and stdout ("") so that piped standard input
  # (the `-` file), TTY detection, and binary standard output all work inside
  # the self-contained script exactly as if it were run directly. Standard
  # error carries package chatter (e.g. readr's column specs) and is
  # discarded unless --verbose was given.
  stderr_to <- if (flags$verbose) "" else NULL
  proc <- processx::process$new(
    ir, c("run", filename),
    stdin = "", stdout = "", stderr = stderr_to, cleanup = TRUE,
    env = c("current", R_DEFAULT_PACKAGES = default_pkgs)
  )
  proc$wait()
  invisible(proc$get_exit_status())
}
