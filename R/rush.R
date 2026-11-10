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

  if (run_may_plot(flags)) {
    pkgs <- c(pkgs, plot_packages())
  }

  pkgs
}

# GitHub packages that render a ggplot to the terminal. They come from forks
# of coolbutuseless's packages, pinned to commits so that changes upstream or
# in the forks only reach rush when these SHAs are updated. All three are
# listed because ir needs each GitHub dependency declared explicitly.
terminal_plot_packages <- c(
  devout = "github::jeroenjanssens/devout@fae1037ac2acbc6f134e200d19d5246f31048a1f",
  miniansi = "github::jeroenjanssens/miniansi@d0cb29600c4d014097dbf1b9c282e1d1ee3e8931",
  devoutansi = "github::jeroenjanssens/devoutansi@f27201d6c8b1735b707c6a627cddeefb31b8a990"
)

# Packages needed to render a ggplot to the terminal or to a file.
plot_packages <- function() {
  c("ggplot2", "fs", unname(terminal_plot_packages))
}

# Whether a `rush run` expression may return a ggplot, in which case the
# generated script must declare the plotting packages. Checked statically,
# because the result type is only known once the script runs.
run_may_plot <- function(flags) {
  if ("ggplot2" %in% as.character(flags$library)) {
    return(TRUE)
  }
  code <- unlist(lapply(flags$expression, deparse))
  any(grepl("ggplot|qplot", code))
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
  pkgs <- plot_packages()

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

# `compact`: for compact scripts (dry runs), which load rush; inline scripts
# don't, so they can't call rush functions.
build_plot_call <- function(flags, compact = FALSE) {
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
  # --geom-args: arguments for the geometry, e.g. geom_density(adjust = 3)
  geom_args <- flags$geom_args %||% list()
  geom_call <- if (geom != "auto") {
    rlang::call2(paste0("geom_", geom), !!!geom_args)
  } else if (!is.null(flags$y)) {
    rlang::call2("geom_point", !!!geom_args)
  } else if (compact) {
    # A bar chart or histogram, depending on the type of x (decided when the
    # script runs, so it also works for stdin)
    rlang::call2("geom_auto", !!!geom_args, .ns = "rush")
  } else {
    # The same choice as geom_auto(), without depending on rush
    rlang::expr({
      .x <- rlang::eval_tidy(quote(!!flags$x), df)
      if (
        (is.numeric(.x) && !is.factor(.x)) ||
          inherits(.x, c("Date", "POSIXt", "difftime", "hms"))
      ) {
        geom_histogram(!!!geom_args)
      } else {
        geom_bar(!!!geom_args)
      }
    })
  }
  plot_call <- rlang::call2("+", plot_call, geom_call)

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
  # Without --verbose, the script's stderr (package startup messages, ir's
  # progress) is only shown when the script fails, so errors aren't hidden.
  stderr_file <- if (flags$verbose) "" else tempfile(fileext = ".log")
  if (!flags$verbose) {
    on.exit(unlink(stderr_file), add = TRUE)
  }
  proc <- processx::process$new(
    exe,
    exe_args,
    stdin = "",
    stdout = "",
    stderr = stderr_file,
    cleanup = TRUE,
    env = c("current", R_DEFAULT_PACKAGES = default_pkgs)
  )
  proc$wait()
  status <- proc$get_exit_status()
  if (!flags$verbose && !identical(status, 0L) && file.exists(stderr_file)) {
    cat(readLines(stderr_file, warn = FALSE), sep = "\n", file = stderr())
  }
  invisible(status)
}
