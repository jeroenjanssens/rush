#' Main entry point for rush
#'
#' @param ... character vector of parameters
#'
#' @export
rush <- function(...) {
  flags <- parse_arguments(...)

  if (flags$verbose) {
    cli::cat_rule("Arguments", file = stderr())
    cli::cat_bullet(
      purrr::map2(names(flags), flags, format_flag),
      bullet_col = "yellow",
      file = stderr()
    )
    cli::cat_rule(file = stderr())
  }

  body_file <- tempfile()
  body <- file(body_file, open = "w")
  on.exit(unlink(body_file), add = TRUE)

  pkgs <- c("rlang", "cli", "tibble", "readr")

  if (is.integer(flags$seed)) {
    code_expression(body, set.seed(!!flags$seed))
  }

  result <- switch(flags$command,
    run = build_run_body(body, flags),
    sql = build_sql_body(body, flags),
    plot = build_plot_body(body, flags)
  )
  pkgs <- c(pkgs, result)

  if (is_parquet_output(flags$output)) {
    pkgs <- c(pkgs, "nanoparquet")
  }

  close(body)

  filename <- tempfile(fileext = ".R")
  on.exit(unlink(filename), add = TRUE)

  writeLines(
    c(
      frontmatter(unique(pkgs)),
      "",
      script_preamble(flags),
      "",
      readLines(body_file),
      "",
      dispatch_block(flags$command)
    ),
    filename
  )

  if (flags$dry_run) {
    print_dry_run(filename)
    return(invisible())
  }

  run_generated_script(filename, flags)
}

build_run_body <- function(con, flags) {
  if (is.null(flags$expression) && length(flags$file) == 0) {
    cli::cli_abort(c(
      "No expression to run.",
      i = "Provide an R expression, e.g. {.code rush run '1 + 1'}.",
      i = "See {.code rush run -h} for usage."
    ))
  }

  pkgs <- emit_setup_libraries(con, flags)

  if (length(flags$file) >= 1) {
    pkgs <- c(pkgs, emit_read_files(con, flags$file, flags))
  }

  if (!is.null(flags$expression)) {
    emit_result_exprs(con, flags$expression)
  } else if (length(flags$file) == 1) {
    code_expression(con, result <- df)
  }

  pkgs
}

build_sql_body <- function(con, flags) {
  if (is.null(flags$query)) {
    cli::cli_abort(c(
      "No query to run.",
      i = "Provide a SQL query, e.g. {.code rush sql 'SELECT 1'}.",
      i = "See {.code rush sql -h} for usage."
    ))
  }

  pkgs <- emit_setup_libraries(con, flags)
  emit_sql(con, flags$query, flags$file, flags)
  c(pkgs, "duckdb", "DBI")
}

build_plot_body <- function(con, flags) {
  pkgs <- c(
    "ggplot2",
    "fs",
    "github::coolbutuseless/devout",
    "github::jeroenjanssens/miniansi",
    "github::coolbutuseless/devoutansi"
  )

  pkgs <- c(pkgs, emit_setup_libraries(con, flags, default = "ggplot2"))

  plot_files <- flags$file %||% "-"
  if (length(plot_files) == 0) {
    plot_files <- "-"
  }
  pkgs <- c(pkgs, emit_read_files(con, plot_files, flags))

  if (!is.null(flags$pre)) {
    purrr::walk(flags$pre, function(e) code_expression(con, !!e))
  }

  plot_call <- build_plot_call(flags)

  if (!is.null(flags$post)) {
    code_expression(con, !!rlang::call2("<-", rlang::sym("p"), plot_call))
    emit_result_exprs(con, flags$post)
  } else {
    code_expression(
      con,
      !!rlang::call2("<-", rlang::sym("result"), plot_call)
    )
  }

  pkgs
}

build_plot_call <- function(flags) {
  aes_names <- c("x", "y", "z", "color", "alpha", "shape", "group", "size", "fill")
  aes_call <- rlang::call2("aes", !!!purrr::compact(flags[aes_names]))
  if (!is.null(flags$aes)) {
    aes_call <- rlang::call_modify(aes_call, !!!flags$aes, .homonyms = "last")
  }

  plot_call <- rlang::call2("ggplot", rlang::sym("df"), aes_call)

  geom <- flags$geom
  if (geom == "auto") {
    geom <- if (!is.null(flags$y)) "point" else "histogram"
  }
  plot_call <- rlang::call2("+", plot_call, rlang::call2(paste0("geom_", geom)))

  if (!is.null(flags$log)) {
    if (!flags$log %in% c("x", "y", "xy")) {
      cli::cli_abort(c(
        "{.arg --log} must be one of {.val x}, {.val y}, or {.val xy}.",
        x = "Got {.val {flags$log}}."
      ))
    }
    if (grepl("x", flags$log, fixed = TRUE)) {
      plot_call <- rlang::call2("+", plot_call, rlang::call2("scale_x_log10"))
    }
    if (grepl("y", flags$log, fixed = TRUE)) {
      plot_call <- rlang::call2("+", plot_call, rlang::call2("scale_y_log10"))
    }
  }

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

  labs_args <- purrr::compact(list(
    x = flags$xlab,
    y = flags$ylab,
    title = flags$title
  ))
  if (length(labs_args) > 0) {
    plot_call <- rlang::call2("+", plot_call, rlang::call2("labs", !!!labs_args))
  }

  plot_call
}

print_dry_run <- function(filename) {
  air <- Sys.which("air")
  if (air != "") {
    tryCatch(
      processx::run(air, c("format", filename)),
      error = function(e) NULL
    )
  }
  code <- readLines(filename)
  if (isatty(stdout())) {
    code <- prettycode::highlight(code)
  }
  cat(code, sep = "\n")
}

run_generated_script <- function(filename, flags) {
  if (flags$no_ir) {
    exe <- Sys.which("Rscript")
    if (exe == "") {
      cli::cli_abort(c(
        "{.code Rscript} could not be found on the {.envvar PATH}.",
        i = "It is needed to run the script when {.code --no-ir} is set."
      ))
    }
    exe_args <- filename
  } else {
    exe <- Sys.which("ir")
    if (exe == "") {
      cli::cli_abort(c(
        "The {.pkg ir} command-line tool could not be found on the {.envvar PATH}.",
        i = "See {.url https://r-lib.github.io/ir/} for installation instructions.",
        i = "Alternatively, run against your installed packages with {.code --no-ir}."
      ))
    }
    exe_args <- c("run", filename)
  }

  default_pkgs <- "datasets,utils,grDevices,graphics,stats,methods"
  stderr_to <- if (flags$verbose) "" else NULL
  proc <- processx::process$new(
    exe,
    exe_args,
    stdin = "",
    stdout = "",
    stderr = stderr_to,
    cleanup = TRUE,
    env = c("current", R_DEFAULT_PACKAGES = default_pkgs)
  )
  proc$wait()
  invisible(proc$get_exit_status())
}
