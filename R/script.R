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

emit_setup_libraries <- function(con, flags, default = character()) {
  pkgs <- character(0)
  if (flags$tidyverse) {
    code_library(con, "tidyverse")
    code_library(con, "glue")
    pkgs <- c(pkgs, "tidyverse", "glue")
  } else if (length(default) > 0) {
    purrr::walk(default, function(e) code_library(con, e))
  }
  if (!is.null(flags$library)) {
    purrr::walk(flags$library, function(e) code_library(con, e))
    pkgs <- c(pkgs, as.character(flags$library))
  }
  pkgs
}

# A minimal, dependency-free version of janitor::make_clean_names, used only
# to turn multiple input file names into distinct data-frame names.
clean_names_simple <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

format_supports_nesting <- function(fmt) {
  fmt %in% c("json", "jsonl", "yaml", "toml", "xml", "rds")
}

file_kind <- function(path, input_format = "auto") {
  if (!identical(input_format, "auto")) {
    if (input_format %in% c("csv", "tsv")) return("delim")
    if (input_format == "zsav") return("sav")
    return(input_format)
  }
  if (path == "-") return("stdin")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "pq")) return("parquet")
  if (ext %in% c("duckdb", "ddb")) return("duckdb")
  if (ext == "json") return("json")
  if (ext %in% c("jsonl", "ndjson")) return("jsonl")
  if (ext %in% c("xlsx", "xls")) return("xlsx")
  if (ext %in% c("arrow", "ipc", "feather")) return("arrow")
  if (ext %in% c("sav", "zsav")) return("sav")
  if (ext == "por") return("por")
  if (ext == "dta") return("dta")
  if (ext == "sas7bdat") return("sas7bdat")
  if (ext == "xpt") return("xpt")
  if (ext %in% c("sqlite", "db")) return("sqlite")
  if (ext == "fwf") return("fwf")
  if (ext == "rds") return("rds")
  if (ext == "ods") return("ods")
  if (ext %in% c("fasta", "fa", "fna")) return("fasta")
  if (ext %in% c("fastq", "fq")) return("fastq")
  if (ext %in% c("yaml", "yml")) return("yaml")
  if (ext == "toml") return("toml")
  if (ext == "xml") return("xml")
  "delim"
}

resolve_output_format <- function(output, output_format) {
  if (!identical(output_format, "auto")) {
    if (output_format %in% c("csv", "tsv")) return("delim")
    return(output_format)
  }
  if (!is.null(output)) {
    ext <- tolower(tools::file_ext(output))
    if (ext %in% c("parquet", "pq")) return("parquet")
    if (ext == "json") return("json")
    if (ext %in% c("jsonl", "ndjson")) return("jsonl")
    if (ext %in% c("arrow", "ipc", "feather")) return("arrow")
    if (ext == "xlsx") return("xlsx")
    if (ext == "sav") return("sav")
    if (ext == "zsav") return("zsav")
    if (ext == "dta") return("dta")
    if (ext == "sas7bdat") return("sas7bdat")
    if (ext == "xpt") return("xpt")
    if (ext %in% c("duckdb", "ddb")) return("duckdb")
    if (ext %in% c("sqlite", "db")) return("sqlite")
    if (ext == "rds") return("rds")
    if (ext == "ods") return("ods")
    if (ext %in% c("fasta", "fa", "fna")) return("fasta")
    if (ext %in% c("fastq", "fq")) return("fastq")
    if (ext %in% c("yaml", "yml")) return("yaml")
    if (ext == "toml") return("toml")
    if (ext == "xml") return("xml")
  }
  "delim"
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
    kind <- file_kind(path, flags$input_format %||% "auto")
    if (kind == "parquet") {
      read_pkgs <- "nanoparquet"
      read_expr <- expr(nanoparquet::read_parquet(!!path))
    } else if (kind == "json") {
      read_pkgs <- "jsonlite"
      src <- if (path == "-") expr(file("stdin")) else path
      read_expr <- expr(jsonlite::fromJSON(!!src))
      if (!format_supports_nesting(flags$resolved_output_format)) {
        read_expr <- expr(jsonlite::flatten(!!read_expr))
      }
    } else if (kind == "jsonl") {
      read_pkgs <- "jsonlite"
      src <- if (path == "-") expr(file("stdin")) else expr(file(!!path))
      read_expr <- expr(jsonlite::stream_in(!!src, verbose = FALSE))
      if (!format_supports_nesting(flags$resolved_output_format)) {
        read_expr <- expr(jsonlite::flatten(!!read_expr))
      }
    } else if (kind == "xlsx") {
      read_pkgs <- "readxl"
      read_expr <- if (!is.null(flags$sheet)) {
        expr(readxl::read_excel(!!path, sheet = !!flags$sheet))
      } else {
        expr(readxl::read_excel(!!path))
      }
    } else if (kind == "arrow") {
      read_pkgs <- "arrow"
      read_expr <- expr(arrow::read_ipc_file(!!path))
    } else if (kind == "sav") {
      read_pkgs <- "haven"
      read_expr <- expr(haven::read_sav(!!path))
    } else if (kind == "por") {
      read_pkgs <- "haven"
      read_expr <- expr(haven::read_por(!!path))
    } else if (kind == "dta") {
      read_pkgs <- "haven"
      read_expr <- expr(haven::read_dta(!!path))
    } else if (kind == "sas7bdat") {
      read_pkgs <- "haven"
      read_expr <- expr(haven::read_sas(!!path))
    } else if (kind == "xpt") {
      read_pkgs <- "haven"
      read_expr <- expr(haven::read_xpt(!!path))
    } else if (kind == "fwf") {
      read_expr <- expr(readr::read_fwf(!!path, col_positions = readr::fwf_empty(!!path)))
    } else if (kind == "rds") {
      read_expr <- expr(readRDS(!!path))
    } else if (kind == "ods") {
      read_pkgs <- "readODS"
      read_expr <- expr(readODS::read_ods(!!path))
    } else if (kind == "fasta") {
      read_pkgs <- "microseq"
      read_expr <- expr(microseq::readFasta(!!path))
    } else if (kind == "fastq") {
      read_pkgs <- "microseq"
      read_expr <- expr(microseq::readFastq(!!path))
    } else if (kind == "yaml") {
      read_pkgs <- "yaml"
      read_expr <- expr(as.data.frame(yaml::read_yaml(!!path)))
    } else if (kind == "toml") {
      read_pkgs <- "RcppTOML"
      read_expr <- expr(as.data.frame(RcppTOML::parseTOML(!!path)))
    } else if (kind == "xml") {
      read_pkgs <- "xml2"
      read_expr <- expr(xml2::as_list(xml2::read_xml(!!path)))
    } else {
      if (path == "-") {
        path <- expr(file("stdin", "rb", raw = TRUE))
      }
      read_expr <- expr(readr::read_delim(
        !!path,
        delim = !!flags$resolved_input_delimiter,
        col_names = !!(!flags$no_header)
      ))
    }
    if (!flags$no_clean_names) {
      read_expr <- expr(janitor::clean_names(!!read_expr))
    }
    list(expr = read_expr, packages = read_pkgs)
  }

  emit_duckdb <- function(path, name) {
    read <- if (!flags$no_clean_names) {
      paste0("    dfs[[\"", name, "\"]][[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))")
    } else {
      paste0("    dfs[[\"", name, "\"]][[.t]] <<- DBI::dbReadTable(.con, .t)")
    }
    writeLines(
      c(
        paste0("dfs[[\"", name, "\"]] <- list()"),
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

  emit_sqlite <- function(path, name) {
    read <- if (!flags$no_clean_names) {
      paste0("    dfs[[\"", name, "\"]][[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))")
    } else {
      paste0("    dfs[[\"", name, "\"]][[.t]] <<- DBI::dbReadTable(.con, .t)")
    }
    writeLines(
      c(
        paste0("dfs[[\"", name, "\"]] <- list()"),
        "local({",
        paste0(
          "  .con <- DBI::dbConnect(RSQLite::SQLite(), ",
          encodeString(path, quote = "\""),
          ")"
        ),
        "  on.exit(DBI::dbDisconnect(.con))",
        "  for (.t in DBI::dbListTables(.con)) {",
        read,
        "  }",
        "})"
      ),
      con
    )
    c("RSQLite", "DBI")
  }

  df_names <- input_names(files)
  code_expression(con, dfs <- list())

  for (i in seq_along(files)) {
    kind <- file_kind(files[[i]], flags$input_format %||% "auto")
    if (kind == "duckdb") {
      pkgs <- c(pkgs, emit_duckdb(files[[i]], df_names[[i]]))
    } else if (kind == "sqlite") {
      pkgs <- c(pkgs, emit_sqlite(files[[i]], df_names[[i]]))
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

  code_expression(con, df <- dfs[[1L]])

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
    kinds <- vapply(files, file_kind, character(1))

    if (any(kinds %in% c("json", "jsonl"))) {
      writeLines('invisible(DBI::dbExecute(con, "INSTALL json; LOAD json;"))', con)
    }

    for (i in seq_along(files)) {
      path <- files[[i]]
      nm <- names[[i]]
      kind <- kinds[[i]]
      if (kind == "parquet") {
        stmt <- paste0(
          "CREATE VIEW ",
          sql_id(nm),
          " AS SELECT * FROM read_parquet(",
          sql_str(path),
          ")"
        )
      } else if (kind %in% c("json", "jsonl")) {
        stmt <- paste0(
          "CREATE VIEW ",
          sql_id(nm),
          " AS SELECT * FROM read_json_auto(",
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
              gsub('"', '\\\\"', sql_id(nm)),
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
  ctx <- list(
    output = flags$output,
    output_template = flags$output_template,
    output_format = flags$resolved_output_format,
    width = flags$width,
    height = flags$height,
    units = flags$units,
    dpi = flags$dpi,
    delimiter = flags$resolved_output_delimiter,
    head = flags$head,
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

.write_result <- function(result, output) {
  if (!is.null(.rush$head)) result <- head(result, .rush$head)
  if (.has_tty && is.null(output)) {
    options(tibble.width = if (is.null(.rush$width)) cli::console_width() else .rush$width)
    print(tibble::as_tibble(result), n = .rush$height)
  } else if (identical(.rush$output_format, "parquet")) {
    nanoparquet::write_parquet(result, output)
  } else if (identical(.rush$output_format, "json")) {
    json <- jsonlite::toJSON(result, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
    if (is.null(output)) cat(json, "\n") else writeLines(json, output)
  } else if (identical(.rush$output_format, "jsonl")) {
    con_out <- if (is.null(output)) stdout() else file(output, "w")
    jsonlite::stream_out(result, con_out, verbose = FALSE)
    if (!is.null(output)) close(con_out)
  } else if (identical(.rush$output_format, "arrow")) {
    arrow::write_ipc_file(result, output)
  } else if (identical(.rush$output_format, "xlsx")) {
    writexl::write_xlsx(result, output)
  } else if (identical(.rush$output_format, "sav")) {
    haven::write_sav(result, output)
  } else if (identical(.rush$output_format, "zsav")) {
    haven::write_sav(result, output, compress = "zsav")
  } else if (identical(.rush$output_format, "dta")) {
    haven::write_dta(result, output)
  } else if (identical(.rush$output_format, "sas7bdat")) {
    haven::write_sas(result, output)
  } else if (identical(.rush$output_format, "xpt")) {
    haven::write_xpt(result, output)
  } else if (identical(.rush$output_format, "duckdb")) {
    .con <- DBI::dbConnect(duckdb::duckdb(), dbdir = output)
    on.exit(DBI::dbDisconnect(.con, shutdown = TRUE))
    if (is.data.frame(result)) {
      DBI::dbWriteTable(.con, "data", result)
    } else if (is.list(result)) {
      for (.tbl_name in names(result)) {
        DBI::dbWriteTable(.con, .tbl_name, result[[.tbl_name]])
      }
    }
  } else if (identical(.rush$output_format, "sqlite")) {
    .con <- DBI::dbConnect(RSQLite::SQLite(), output)
    on.exit(DBI::dbDisconnect(.con))
    if (is.data.frame(result)) {
      DBI::dbWriteTable(.con, "data", result)
    } else if (is.list(result)) {
      for (.tbl_name in names(result)) {
        DBI::dbWriteTable(.con, .tbl_name, result[[.tbl_name]])
      }
    }
  } else if (identical(.rush$output_format, "rds")) {
    saveRDS(result, output)
  } else if (identical(.rush$output_format, "ods")) {
    readODS::write_ods(result, output)
  } else if (identical(.rush$output_format, "fasta")) {
    microseq::writeFasta(result, output)
  } else if (identical(.rush$output_format, "fastq")) {
    microseq::writeFastq(result, output)
  } else if (identical(.rush$output_format, "yaml")) {
    yaml_out <- yaml::as.yaml(result)
    if (is.null(output)) cat(yaml_out) else writeLines(yaml_out, output)
  } else if (identical(.rush$output_format, "toml")) {
    toml_out <- RcppTOML::writeTOML(result)
    if (is.null(output)) cat(toml_out) else writeLines(toml_out, output)
  } else if (identical(.rush$output_format, "xml")) {
    xml_doc <- xml2::as_xml_document(list(data = as.list(result)))
    xml2::write_xml(xml_doc, if (is.null(output)) stdout() else output)
  } else {
    con <- if (is.null(output)) .stdout_binary() else output
    readr::write_delim(result, con, delim = .rush$delimiter %||% ",")
  }
}

if (!is.null(.rush$output_template)) {
  .expand_field <- function(value, spec) {
    if (grepl("l$", spec)) {
      tolower(sprintf(paste0("%", sub("l$", "s", spec)), value))
    } else if (grepl("u$", spec)) {
      toupper(sprintf(paste0("%", sub("u$", "s", spec)), value))
    } else {
      sprintf(paste0("%", spec), value)
    }
  }
  .expand_template <- function(tmpl, file_name, file_index, table_name, table_index) {
    while (grepl("%[(][^)]+[)]([^%]*[a-z])", tmpl)) {
      m <- regexec("%[(]([^)]+)[)]([^%]*[a-z])", tmpl)[[1]]
      full <- regmatches(tmpl, list(m))[[1]]
      field <- full[2]
      spec <- full[3]
      value <- switch(field,
        file_name = file_name, file_index = file_index,
        table_name = table_name, table_index = table_index,
        "")
      replacement <- .expand_field(value, spec)
      tmpl <- sub("%[(]([^)]+)[)]([^%]*[a-z])", replacement, tmpl)
    }
    tmpl
  }
  .file_index <- 0L
  for (.fname in names(result)) {
    .file_index <- .file_index + 1L
    .item <- result[[.fname]]
    if (is.data.frame(.item)) {
      .path <- .expand_template(.rush$output_template, .fname, .file_index, "", 0L)
      .write_result(.item, .path)
    } else if (is.list(.item)) {
      .table_index <- 0L
      for (.tname in names(.item)) {
        .table_index <- .table_index + 1L
        .path <- .expand_template(.rush$output_template, .fname, .file_index, .tname, .table_index)
        .write_result(.item[[.tname]], .path)
      }
    }
  }
} else if (rlang::is_atomic(result)) {
  cli::cat_line(result)
} else if (is.data.frame(result)) {
  .write_result(result, .rush$output)
} else if (rlang::is_bare_list(result)) {
  if (.rush$output_format %in% c("duckdb", "sqlite")) {
    .write_result(result, .rush$output)
  } else {
    result <- tibble::enframe(result)
    .write_result(result, .rush$output)
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
