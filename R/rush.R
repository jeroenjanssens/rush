#' @keywords internal
#' @export
rush <- function(...) {
  flags <- parse_arguments(...)
  flags <- resolve_flags(flags)
  generate_script(flags$command, flags)
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

  first_kind <- file_kind(plot_files[[1]], flags$input_format %||% "auto")
  if (first_kind %in% c("duckdb", "sqlite")) {
    writeLines("if (is.list(df) && !is.data.frame(df)) df <- df[[1L]]", con)
  }

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

build_convert_body <- function(con, flags) {
  if (length(flags$file) == 0) {
    cli::cli_abort(c(
      "No input file to convert.",
      i = "Provide at least one file, e.g. {.code rush convert data.csv -o data.parquet}.",
      i = "See {.code rush convert -h} for usage."
    ))
  }
  if (
    is.null(flags$output) &&
      is.null(flags$output_template) &&
      identical(flags$output_format, "auto")
  ) {
    cli::cli_abort(c(
      "No output format specified.",
      i = "Use {.code -o <file>} to write to a file, or {.code -O <format>} to write to stdout.",
      i = "See {.code rush convert -h} for usage."
    ))
  }

  pkgs <- emit_setup_libraries(con, flags)
  pkgs <- c(pkgs, emit_read_files(con, flags$file, flags))

  is_multi <- length(flags$file) > 1 ||
    any(
      vapply(
        flags$file,
        function(f) file_kind(f, flags$input_format %||% "auto"),
        character(1)
      ) %in%
        c("duckdb", "sqlite")
    )
  output_is_db <- flags$resolved_output_format %in% c("duckdb", "sqlite")

  if (!is.null(flags$output_template)) {
    code_expression(con, result <- dfs)
  } else if (output_is_db) {
    code_expression(con, result <- dfs)
  } else if (is_multi) {
    cli::cli_abort(c(
      "Multiple inputs require an output template.",
      i = "Use {.code %%(file_name)s} or {.code %%(file_index)d} in the output path.",
      i = "Example: {.code rush convert -o 'out_%%(file_name)s.csv' a.parquet b.parquet}.",
      i = "Or use {.code rush run} with an expression to select specific data."
    ))
  } else {
    code_expression(con, result <- df)
  }

  pkgs
}

build_plot_call <- function(flags) {
  aes_names <- c(
    "x",
    "y",
    "z",
    "color",
    "alpha",
    "shape",
    "group",
    "size",
    "fill"
  )
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
    plot_call <- rlang::call2(
      "+",
      plot_call,
      rlang::call2("labs", !!!labs_args)
    )
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
  if (isatty(stdout()) && requireNamespace("prettycode", quietly = TRUE)) {
    code <- prettycode::highlight(code)
  }
  cat(code, sep = "\n")
}

run_generated_script <- function(filename, flags) {
  use_ir <- !flags$no_ir && !nzchar(Sys.getenv("RUSH_NO_IR", ""))
  if (!use_ir) {
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
