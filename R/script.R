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

file_kind <- function(path) {
  if (path == "-") return("stdin")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "pq")) return("parquet")
  if (ext %in% c("duckdb", "ddb")) return("duckdb")
  "delim"
}

is_parquet_output <- function(output) {
  !is.null(output) && tolower(tools::file_ext(output)) %in% c("parquet", "pq")
}

input_names <- function(files) {
  nms <- ifelse(
    files == "-",
    "stdin",
    tools::file_path_sans_ext(basename(files))
  ) |>
    clean_names_simple()
  # Prefix with "x" if name starts with a digit to ensure valid R identifier
  needs_prefix <- grepl("^[0-9]", nms)
  nms[needs_prefix] <- paste0("x", nms[needs_prefix])
  make.unique(nms, sep = "_")
}

emit_read_files <- function(con, files, flags) {
  pkgs <- character(0)

  read_call <- function(path) {
    read_pkgs <- character(0)
    kind <- file_kind(path)
    if (kind == "parquet") {
      read_pkgs <- "nanoparquet"
      read_expr <- expr(nanoparquet::read_parquet(!!path))
    } else {
      if (path == "-") {
        path <- expr(file("stdin", "rb", raw = TRUE))
      }
      read_expr <- expr(readr::read_delim(
        !!path,
        delim = !!flags$delimiter,
        col_names = !!(!flags$no_header)
      ))
    }
    if (!flags$no_clean_names) {
      read_expr <- expr(janitor::clean_names(!!read_expr))
    }
    list(expr = read_expr, packages = read_pkgs)
  }

  emit_duckdb <- function(path) {
    read <- if (!flags$no_clean_names) {
      "    dfs[[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))"
    } else {
      "    dfs[[.t]] <<- DBI::dbReadTable(.con, .t)"
    }
    writeLines(
      c(
        "local({",
        paste0(
          "  .con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ",
          encodeString(path, quote = "\""),
          ", read_only = TRUE)"
        ),
        "  on.exit(DBI::dbDisconnect(.con, shutdown = TRUE))",
        "  for (.t in DBI::dbListTables(.con)) {",
        read,
        "  }",
        "})"
      ),
      con
    )
    c("duckdb", "DBI")
  }

  if (length(files) == 1 && file_kind(files[[1]]) != "duckdb") {
    rc <- read_call(files)
    pkgs <- c(pkgs, rc$packages)
    code_expression(con, `<-`(df, !!rc$expr))
  } else {
    df_names <- input_names(files)

    code_expression(con, dfs <- list())
    for (i in seq_along(files)) {
      if (file_kind(files[[i]]) == "duckdb") {
        pkgs <- c(pkgs, emit_duckdb(files[[i]]))
      } else {
        rc <- read_call(files[[i]])
        pkgs <- c(pkgs, rc$packages)
        assign_expr <- rlang::call2(
          "<-",
          rlang::call2("[[", rlang::sym("dfs"), df_names[[i]]),
          rc$expr
        )
        code_expression(con, !!assign_expr)
      }
    }
    if (length(files) == 1) {
      code_expression(con, if (length(dfs) == 1) df <- dfs[[1]])
    }
  }

  if (!flags$no_clean_names) {
    pkgs <- c(pkgs, "janitor")
  }
  unique(pkgs)
}

emit_sql <- function(con, query, files, flags) {
  sql_str <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  sql_id <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')

  writeLines("con <- DBI::dbConnect(duckdb::duckdb())", con)
  writeLines("on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)", con)

  if (length(files) > 0) {
    names <- input_names(files)

    for (i in seq_along(files)) {
      path <- files[[i]]
      nm <- names[[i]]
      kind <- file_kind(path)
      if (kind == "parquet") {
        stmt <- paste0(
          "CREATE VIEW ",
          sql_id(nm),
          " AS SELECT * FROM read_parquet(",
          sql_str(path),
          ")"
        )
      } else if (kind == "duckdb") {
        stmt <- paste0(
          "ATTACH ",
          sql_str(path),
          " AS ",
          sql_id(nm),
          " (READ_ONLY)"
        )
      } else if (kind == "stdin") {
        writeLines(
          c(
            ".stdin_tmp <- tempfile(fileext = \".csv\")",
            "local({",
            "  .in <- file(\"stdin\", \"rb\"); .out <- file(.stdin_tmp, \"wb\")",
            "  on.exit({ close(.in); close(.out) })",
            "  repeat { .b <- readBin(.in, \"raw\", 1048576L); if (length(.b) == 0) break; writeBin(.b, .out) }",
            "})",
            paste0(
              "invisible(DBI::dbExecute(con, paste0(\"CREATE VIEW ",
              sql_id(nm),
              " AS SELECT * FROM read_csv_auto('\", .stdin_tmp, \"')\")))"
            )
          ),
          con
        )
        next
      } else {
        stmt <- paste0(
          "CREATE VIEW ",
          sql_id(nm),
          " AS SELECT * FROM read_csv_auto(",
          sql_str(path),
          ")"
        )
      }
      writeLines(
        paste0(
          "invisible(DBI::dbExecute(con, ",
          encodeString(stmt, quote = "\""),
          "))"
        ),
        con
      )
    }
  }

  writeLines(
    paste0(
      "result <- DBI::dbGetQuery(con, ",
      encodeString(query, quote = "\""),
      ")"
    ),
    con
  )
}

script_preamble <- function(flags) {
  output_format <- if (is_parquet_output(flags$output)) "parquet" else "delim"
  ctx <- list(
    output = flags$output,
    output_format = output_format,
    width = flags$width,
    height = flags$height,
    units = flags$units,
    dpi = flags$dpi,
    delimiter = flags$delimiter %||% ",",
    has_post = !is.null(flags$post)
  )
  lines <- vapply(
    names(ctx),
    function(nm) {
      paste0("  ", nm, " = ", paste(deparse(ctx[[nm]]), collapse = ""))
    },
    character(1)
  )
  c(".rush <- list(", paste0(lines, c(rep(",", length(lines) - 1L), "")), ")")
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
