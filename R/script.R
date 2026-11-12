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

# Whether the generated script runs with `ir`. `--no-ir` and the
# `RUSH_NO_IR` environment variable run it with `Rscript` instead.
use_ir <- function(flags) {
  !isTRUE(flags$no_ir) && !nzchar(Sys.getenv("RUSH_NO_IR", ""))
}

# The `#| packages:` frontmatter that turns the generated script into a
# self-describing `ir` script. `ir run` resolves these refs with pak before
# executing the script. A script that runs with `Rscript` uses the packages
# that are installed, so it gets only an `Rscript` shebang.
frontmatter <- function(packages, flags) {
  if (!use_ir(flags)) {
    return("#!/usr/bin/env Rscript")
  }
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
    if (input_format %in% c("csv", "tsv")) {
      return("delim")
    }
    if (input_format == "zsav") {
      return("sav")
    }
    return(input_format)
  }
  if (path == "-") {
    return("stdin")
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "pq")) {
    return("parquet")
  }
  if (ext %in% c("duckdb", "ddb")) {
    return("duckdb")
  }
  if (ext == "json") {
    return("json")
  }
  if (ext %in% c("jsonl", "ndjson")) {
    return("jsonl")
  }
  if (ext %in% c("xlsx", "xls")) {
    return("xlsx")
  }
  if (ext %in% c("arrow", "ipc", "feather")) {
    return("arrow")
  }
  if (ext %in% c("sav", "zsav")) {
    return("sav")
  }
  if (ext == "por") {
    return("por")
  }
  if (ext == "dta") {
    return("dta")
  }
  if (ext == "sas7bdat") {
    return("sas7bdat")
  }
  if (ext == "xpt") {
    return("xpt")
  }
  if (ext %in% c("sqlite", "db")) {
    return("sqlite")
  }
  if (ext == "fwf") {
    return("fwf")
  }
  if (ext == "rds") {
    return("rds")
  }
  if (ext == "ods") {
    return("ods")
  }
  if (ext %in% c("fasta", "fa", "fna")) {
    return("fasta")
  }
  if (ext %in% c("fastq", "fq")) {
    return("fastq")
  }
  if (ext %in% c("yaml", "yml")) {
    return("yaml")
  }
  if (ext == "toml") {
    return("toml")
  }
  if (ext == "xml") {
    return("xml")
  }
  "delim"
}

resolve_output_format <- function(output, output_format) {
  if (!identical(output_format, "auto")) {
    return(output_format)
  }
  if (!is.null(output)) {
    ext <- tolower(tools::file_ext(output))
    if (ext %in% c("parquet", "pq")) {
      return("parquet")
    }
    if (ext == "json") {
      return("json")
    }
    if (ext %in% c("jsonl", "ndjson")) {
      return("jsonl")
    }
    if (ext %in% c("arrow", "ipc", "feather")) {
      return("arrow")
    }
    if (ext == "xlsx") {
      return("xlsx")
    }
    if (ext == "sav") {
      return("sav")
    }
    if (ext == "zsav") {
      return("zsav")
    }
    if (ext == "dta") {
      return("dta")
    }
    if (ext == "sas7bdat") {
      return("sas7bdat")
    }
    if (ext == "xpt") {
      return("xpt")
    }
    if (ext %in% c("duckdb", "ddb")) {
      return("duckdb")
    }
    if (ext %in% c("sqlite", "db")) {
      return("sqlite")
    }
    if (ext == "rds") {
      return("rds")
    }
    if (ext == "ods") {
      return("ods")
    }
    if (ext %in% c("fasta", "fa", "fna")) {
      return("fasta")
    }
    if (ext %in% c("fastq", "fq")) {
      return("fastq")
    }
    if (ext %in% c("yaml", "yml")) {
      return("yaml")
    }
    if (ext == "toml") {
      return("toml")
    }
    if (ext == "xml") return("xml")
  }
  "delim"
}

# Extensions that rush reads but can't write
read_only_extensions <- c(
  xls = "Excel 97-2003",
  por = "SPSS portable",
  fwf = "fixed-width"
)

# Values of -O that rush can write data in
data_output_formats <- c(
  "csv",
  "tsv",
  "delim",
  "parquet",
  "json",
  "jsonl",
  "arrow",
  "xlsx",
  "sav",
  "zsav",
  "dta",
  "sas7bdat",
  "xpt",
  "duckdb",
  "sqlite",
  "rds",
  "ods",
  "fasta",
  "fastq",
  "yaml",
  "toml",
  "xml"
)

# Values of -O for plots: terminal art and ggsave() devices
plot_output_formats <- c(
  "ansi",
  "ascii",
  "png",
  "svg",
  "pdf",
  "jpeg",
  "jpg",
  "tiff",
  "bmp",
  "eps",
  "ps",
  "tex",
  "wmf"
)

# Abort when the output can't be written, instead of silently writing CSV.
# `rush plot` is skipped: there, the extension of -o picks a graphics device.
check_output_format <- function(flags) {
  command <- flags$command
  if (is.null(command) || identical(command, "plot")) {
    return(invisible())
  }
  fmt <- flags$output_format %||% "auto"
  if (identical(fmt, "auto") && !is.null(flags$output)) {
    ext <- tolower(tools::file_ext(flags$output))
    if (ext %in% names(read_only_extensions)) {
      cli::cli_abort(
        c(
          "Can't write {.file {flags$output}}.",
          x = "rush can read {read_only_extensions[[ext]]} ({.file .{ext}}) files, but not write them.",
          i = "See {.url https://rush.jeroenjanssens.com/formats.html} for the formats rush can write."
        ),
        call = NULL
      )
    }
  }
  if (!identical(fmt, "auto")) {
    allowed <- data_output_formats
    if (identical(command, "run")) {
      allowed <- c(allowed, plot_output_formats)
    }
    if (!fmt %in% allowed) {
      cli::cli_abort(
        c(
          "Can't write output format {.val {fmt}}.",
          i = "Use one of: {paste(allowed, collapse = ', ')}."
        ),
        call = NULL
      )
    }
  }
  invisible()
}

# Whether a spreadsheet input is read with all its sheets (--input-sheet "*"),
# which makes it a named list of data frames, like a database
all_sheets <- function(kind, flags) {
  kind %in% c("xlsx", "ods") && identical(flags$input_sheet, "*")
}

# Whether an input is read as a list of tables: a database, or a spreadsheet
# with all its sheets
multi_table_input <- function(path, flags) {
  kind <- file_kind(path, flags$input_format %||% "auto")
  kind %in% c("duckdb", "sqlite") || all_sheets(kind, flags)
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
      read_expr <- if (identical(flags$input_sheet, "*")) {
        expr(local({
          .sheets <- readxl::excel_sheets(!!path)
          stats::setNames(
            lapply(.sheets, function(.s) {
              readxl::read_excel(!!path, sheet = .s)
            }),
            .sheets
          )
        }))
      } else if (!is.null(flags$input_sheet)) {
        expr(readxl::read_excel(!!path, sheet = !!flags$input_sheet))
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
      fwf_names <- flags$resolved_col_names
      positions <- if (is.character(fwf_names)) {
        expr(readr::fwf_empty(!!path, col_names = !!fwf_names))
      } else {
        expr(readr::fwf_empty(!!path))
      }
      read_expr <- expr(readr::read_fwf(
        !!path,
        col_positions = !!positions,
        show_col_types = FALSE
      ))
    } else if (kind == "rds") {
      read_expr <- expr(readr::read_rds(!!path))
    } else if (kind == "ods") {
      read_pkgs <- "readODS"
      read_expr <- if (identical(flags$input_sheet, "*")) {
        expr(local({
          .sheets <- readODS::list_ods_sheets(!!path)
          stats::setNames(
            lapply(.sheets, function(.s) readODS::read_ods(!!path, sheet = .s)),
            .sheets
          )
        }))
      } else if (!is.null(flags$input_sheet)) {
        expr(readODS::read_ods(!!path, sheet = !!flags$input_sheet))
      } else {
        expr(readODS::read_ods(!!path))
      }
    } else if (kind == "fasta") {
      read_pkgs <- "microseq"
      read_expr <- expr(microseq::readFasta(!!path))
    } else if (kind == "fastq") {
      read_pkgs <- "microseq"
      read_expr <- expr(microseq::readFastq(!!path))
    } else if (kind == "yaml") {
      read_pkgs <- "yaml"
      src <- if (path == "-") expr(file("stdin")) else path
      read_expr <- expr(local({
        .parsed <- yaml::read_yaml(!!src)
        if (is.list(.parsed) && length(.parsed) > 0 && is.list(.parsed[[1]])) {
          do.call(rbind, lapply(.parsed, as.data.frame))
        } else {
          as.data.frame(.parsed)
        }
      }))
    } else if (kind == "toml") {
      read_pkgs <- "RcppTOML"
      if (path == "-") {
        path <- expr(local({
          .tmp <- tempfile(fileext = ".toml")
          writeLines(readLines(file("stdin")), .tmp)
          .tmp
        }))
      }
      read_expr <- expr(local({
        .parsed <- RcppTOML::parseTOML(!!path)
        .items <- .parsed[[1]]
        if (is.list(.items) && length(.items) > 0 && is.list(.items[[1]])) {
          as.data.frame(do.call(rbind, lapply(.items, as.data.frame)))
        } else {
          as.data.frame(.parsed)
        }
      }))
    } else if (kind == "xml") {
      read_pkgs <- "xml2"
      if (path == "-") {
        path <- expr(local({
          .tmp <- tempfile(fileext = ".xml")
          writeLines(readLines(file("stdin")), .tmp)
          .tmp
        }))
      }
      read_expr <- expr(local({
        .doc <- xml2::read_xml(!!path)
        .rows <- xml2::xml_children(.doc)
        do.call(
          rbind,
          lapply(.rows, function(.row) {
            .vals <- xml2::xml_text(xml2::xml_children(.row))
            names(.vals) <- xml2::xml_name(xml2::xml_children(.row))
            as.data.frame(as.list(.vals))
          })
        )
      }))
    } else {
      if (path == "-") {
        path <- expr(file("stdin", "rb", raw = TRUE))
      }
      delim <- flags$resolved_input_delimiter
      if (is.character(path) && tolower(tools::file_ext(path)) == "tsv") {
        delim <- "\t"
      }
      read_expr <- expr(readr::read_delim(
        !!path,
        delim = !!delim,
        col_names = !!(flags$resolved_col_names),
        show_col_types = FALSE
      ))
    }
    if (!flags$no_clean_names) {
      read_expr <- if (all_sheets(kind, flags)) {
        expr(lapply(!!read_expr, janitor::clean_names))
      } else {
        expr(janitor::clean_names(!!read_expr))
      }
    }
    list(expr = read_expr, packages = read_pkgs)
  }

  emit_duckdb <- function(path, name) {
    read <- if (!flags$no_clean_names) {
      paste0(
        "    dfs[[\"",
        name,
        "\"]][[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))"
      )
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
      paste0(
        "    dfs[[\"",
        name,
        "\"]][[.t]] <<- janitor::clean_names(DBI::dbReadTable(.con, .t))"
      )
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
  writeLines(
    "on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)",
    con
  )

  if (length(files) > 0) {
    names <- input_names(files)
    kinds <- vapply(files, file_kind, character(1))

    # DuckDB reads these directly; anything else would be read as CSV
    sql_kinds <- c("delim", "parquet", "json", "jsonl", "duckdb", "stdin")
    unsupported <- files[!kinds %in% sql_kinds]
    if (length(unsupported) > 0) {
      cli::cli_abort(
        c(
          "{.code rush sql} can't query {.file {unsupported}}.",
          i = "It queries CSV/TSV, Parquet, JSON/JSONL, and DuckDB files.",
          i = "Convert other formats first, e.g. {.code rush convert -o data.parquet {unsupported[[1]]}}, or use {.code rush run}."
        ),
        call = NULL
      )
    }

    if (any(kinds %in% c("json", "jsonl"))) {
      writeLines(
        'invisible(DBI::dbExecute(con, "INSTALL json; LOAD json;"))',
        con
      )
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
