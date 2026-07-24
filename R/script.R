`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

code_expression <- function(con, ...) {
  writeLines(as.character(rlang::enexprs(...)), con)
}

code_library <- function(con, name) {
  writeLines(paste0("library(", name, ")"), con)
}

# Emit a list of expressions, assigning the value of the last one to `result`
# so the dispatch block at the end of the script knows what to print or save.
emit_result_exprs <- function(con, exprs) {
  n <- length(exprs)
  if (n > 1) {
    purrr::walk(exprs[-n], function(e) code_expression(con, !!e))
  }
  last <- rlang::call2("<-", rlang::sym("result"), exprs[[n]])
  code_expression(con, !!last)
}

# The `#| packages:` frontmatter that turns the generated script into a
# self-describing `ir` script. `ir run` resolves these refs with pak before
# executing the script.
frontmatter <- function(packages) {
  c(
    "#!/usr/bin/env -S ir run",
    "#| packages:",
    paste0("#|   - ", packages)
  )
}

# A minimal, dependency-free version of janitor::make_clean_names, used only
# to turn multiple input file names into distinct data-frame names.
clean_names_simple <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

# Emit the code that reads the input file(s), shared by `run` and `plot`. A
# single file is read into a data frame named `df`; multiple files are each
# read into a named element of a list `dfs`. Use "-" for standard input.
# The reader is chosen by file extension: `.parquet`/`.pq` via nanoparquet,
# `.duckdb`/`.ddb` (a database file, whose tables each become an element of
# `dfs`) via DuckDB, and everything else as delimited text. Returns the extra
# packages the emitted read code depends on.
emit_read_files <- function(con, files, flags) {
  pkgs <- character(0)

  is_duckdb <- function(path) {
    tolower(tools::file_ext(path)) %in% c("duckdb", "ddb")
  }

  # A read expression for a single delimited or Parquet file (not a database).
  read_call <- function(path) {
    if (tolower(tools::file_ext(path)) %in% c("parquet", "pq")) {
      pkgs <<- c(pkgs, "nanoparquet")
      read_expr <- expr(nanoparquet::read_parquet(!!path))
    } else {
      if (path == "-") path <- expr(file("stdin", "rb", raw = TRUE))
      read_expr <- expr(readr::read_delim(!!path, delim = !!flags$delimiter,
                                          col_names = !!(!flags$no_header)))
    }
    if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
    read_expr
  }

  # A DuckDB database holds any number of tables, so read each of them into a
  # named element of `dfs`. The table count is only known at run time, so this
  # is emitted as a small runtime loop rather than resolved here.
  emit_duckdb <- function(path) {
    pkgs <<- c(pkgs, "duckdb", "DBI")
    read <- if (!flags$no_clean_names) {
      "    dfs[[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))"
    } else {
      "    dfs[[.t]] <<- DBI::dbReadTable(.con, .t)"
    }
    writeLines(c(
      "local({",
      paste0("  .con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ",
             encodeString(path, quote = "\""), ", read_only = TRUE)"),
      "  on.exit(DBI::dbDisconnect(.con, shutdown = TRUE))",
      "  for (.t in DBI::dbListTables(.con)) {",
      read,
      "  }",
      "})"
    ), con)
  }

  # A lone delimited or Parquet file reads straight into `df`. Anything else
  # (a database, or several files) populates the `dfs` list.
  if (length(files) == 1 && !is_duckdb(files[[1]])) {
    code_expression(con, `<-`(df, !!read_call(files)))
  } else {
    df_names <-
      ifelse(files == "-", "stdin",
             tools::file_path_sans_ext(basename(files))) |>
      clean_names_simple()

    code_expression(con, dfs <- list())
    for (i in seq_along(files)) {
      if (is_duckdb(files[[i]])) {
        emit_duckdb(files[[i]])
      } else {
        df_name <- rlang::parse_expr(paste0("dfs$", df_names[[i]]))
        code_expression(con, !!rlang::call2("<-", df_name, read_call(files[[i]])))
      }
    }
    # A single database with exactly one table behaves like any other single
    # input, so also expose that table as `df`.
    if (length(files) == 1) {
      code_expression(con, if (length(dfs) == 1) df <- dfs[[1]])
    }
  }

  if (!flags$no_clean_names) pkgs <- c(pkgs, "janitor")
  unique(pkgs)
}

# Emit the code for the `sql` command: register each input file as a DuckDB
# view (or attach a database) named after its base name, run the query, and
# leave the result in `result` for the output dispatch. Use "-" for a `stdin`
# view reading CSV from standard input.
emit_sql <- function(con, query, files, flags) {
  sql_str <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

  writeLines("con <- DBI::dbConnect(duckdb::duckdb())", con)

  if (length(files) > 0) {
    names <-
      ifelse(files == "-", "stdin",
             tools::file_path_sans_ext(basename(files))) |>
      clean_names_simple()

    for (i in seq_along(files)) {
      path <- files[[i]]
      nm <- names[[i]]
      ext <- tolower(tools::file_ext(path))
      if (ext %in% c("parquet", "pq")) {
        stmt <- paste0("CREATE VIEW ", nm, " AS SELECT * FROM read_parquet(",
                       sql_str(path), ")")
      } else if (ext %in% c("duckdb", "ddb")) {
        stmt <- paste0("ATTACH ", sql_str(path), " AS ", nm, " (READ_ONLY)")
      } else if (path == "-") {
        # DuckDB's CSV reader seeks to sniff the schema, which a pipe does not
        # support, so buffer standard input to a seekable temporary file first.
        writeLines(c(
          ".stdin_tmp <- tempfile(fileext = \".csv\")",
          "local({",
          "  .in <- file(\"stdin\", \"rb\"); .out <- file(.stdin_tmp, \"wb\")",
          "  on.exit({ close(.in); close(.out) })",
          "  repeat { .b <- readBin(.in, \"raw\", 1048576L); if (length(.b) == 0) break; writeBin(.b, .out) }",
          "})",
          paste0("invisible(DBI::dbExecute(con, paste0(\"CREATE VIEW ", nm,
                 " AS SELECT * FROM read_csv_auto('\", .stdin_tmp, \"')\")))")
        ), con)
        next
      } else {
        stmt <- paste0("CREATE VIEW ", nm, " AS SELECT * FROM read_csv_auto(",
                       sql_str(path), ")")
      }
      writeLines(paste0("invisible(DBI::dbExecute(con, ",
                        encodeString(stmt, quote = "\""), "))"), con)
    }
  }

  writeLines(paste0("result <- DBI::dbGetQuery(con, ",
                    encodeString(query, quote = "\""), ")"), con)
  writeLines("DBI::dbDisconnect(con, shutdown = TRUE)", con)
}

# Bake the runtime context that the dispatch block needs into an R list
# literal, so the generated script stays self-contained.
script_preamble <- function(flags) {
  output_format <-
    if (!is.null(flags$output) &&
        tolower(tools::file_ext(flags$output)) %in% c("parquet", "pq")) {
      "parquet"
    } else {
      "delim"
    }
  ctx <- list(
    output        = flags$output,
    output_format = output_format,
    width         = flags$width,
    height        = flags$height,
    units         = flags$units,
    dpi           = flags$dpi,
    delimiter     = flags$delimiter %||% ",",
    has_post      = !is.null(flags$post)
  )
  lines <- vapply(names(ctx), function(nm) {
    paste0("  ", nm, " = ", paste(deparse(ctx[[nm]]), collapse = ""))
  }, character(1))
  c(".rush <- list(",
    paste0(lines, c(rep(",", length(lines) - 1L), "")),
    ")")
}

# The output-dispatch code appended to every generated script. It runs inside
# the `ir run` subprocess, which inherits the parent's stdio, so TTY detection
# and binary output behave exactly as if rush had produced the output itself.
dispatch_block <- function(command) {
  if (command == "plot") dispatch_plot() else dispatch_run()
}

dispatch_run <- function() {
'#~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.has_tty <- isatty(stdout())
.stdout_binary <- function() {
  if (.Platform$OS.type == "windows") file("stdout", "wb", raw = TRUE)
  else file("/dev/stdout", "wb", raw = TRUE)
}
if (.has_tty) options(width = if (is.null(.rush$width)) cli::console_width() else .rush$width)

if (rlang::is_atomic(result)) {
  cli::cat_line(result)
} else if (rlang::is_bare_list(result)) {
  result <- tibble::enframe(result)
}

if (is.data.frame(result)) {
  if (.has_tty && is.null(.rush$output)) {
    options(tibble.width = if (is.null(.rush$width)) cli::console_width() else .rush$width)
    print(tibble::as_tibble(result), n = .rush$height)
  } else if (identical(.rush$output_format, "parquet")) {
    nanoparquet::write_parquet(result, .rush$output)
  } else {
    con <- if (is.null(.rush$output)) .stdout_binary() else .rush$output
    readr::write_delim(result, con, delim = .rush$delimiter)
  }
}
'
}

dispatch_plot <- function() {
'#~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.has_tty <- isatty(stdout())
.stdout_binary <- function() {
  if (.Platform$OS.type == "windows") file("stdout", "wb", raw = TRUE)
  else file("/dev/stdout", "wb", raw = TRUE)
}

out <- .rush$output
w <- .rush$width
h <- .rush$height
if (is.null(out)) out <- if (.has_tty) "ansi" else "png"

if (out %in% c("ansi", "ascii")) {
  if (is.null(w)) w <- cli::console_width()
  devoutansi::ansi(width = w, height = h, plain_ascii = TRUE, char_lookup_table = 2)
  if (!.rush$has_post) {
    result <- result +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  print(result)
  invisible(grDevices::dev.off())
} else {
  if (fs::path_ext(out) == "") {
    output_filename <- tempfile()
    device <- out
    cat_output <- TRUE
  } else {
    output_filename <- out
    device <- NULL
    cat_output <- FALSE
  }
  if (is.null(w)) w <- 6
  if (is.null(h)) h <- 4
  ggplot2::ggsave(output_filename, result, device = device,
                  width = w, height = h, units = .rush$units, dpi = .rush$dpi)
  if (cat_output) {
    contents <- readBin(output_filename, raw(), n = 1e8)
    writeBin(contents, .stdout_binary())
  }
}
'
}
