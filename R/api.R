# Internal environment for storing rush configuration set by init()
.rush_env <- new.env(parent = emptyenv())

#' Initialize rush output configuration
#'
#' Sets up the output dispatch parameters for a rush script. Called at the top
#' of generated scripts (when not using `--no-rush`). Must be called before
#' [rush::write()].
#'
#' @param output Output file path, or NULL for stdout.
#' @param output_format Output format string (e.g. "delim", "json", "parquet").
#' @param delimiter Output delimiter for delimited formats.
#' @param head Maximum number of rows to output.
#' @param width Plot width or console width.
#' @param height Plot height or number of tibble rows to print.
#' @param units Plot size units.
#' @param dpi Plot resolution.
#' @param output_root Root element name for XML output.
#' @param output_record Record element name for XML/TOML output.
#' @param output_sheet Excel sheet name for xlsx output.
#' @param output_indent Indentation level for JSON/YAML output.
#' @param output_template Output path template for multi-file output.
#'
#' @export
init <- function(
  output = NULL,
  output_format = "delim",
  delimiter = ",",
  head = NULL,
  width = NULL,
  height = NULL,
  units = "in",
  dpi = 300,
  output_root = "root",
  output_record = "record",
  output_sheet = NULL,
  output_indent = 2L,
  output_template = NULL
) {
  .rush_env$output <- output
  .rush_env$output_format <- output_format
  .rush_env$delimiter <- delimiter
  .rush_env$head <- head
  .rush_env$width <- width
  .rush_env$height <- height
  .rush_env$units <- units
  .rush_env$dpi <- dpi
  .rush_env$output_root <- output_root
  .rush_env$output_record <- output_record
  .rush_env$output_sheet <- output_sheet
  .rush_env$output_indent <- output_indent
  .rush_env$output_template <- output_template

  .rush_env$has_tty <- isatty(stdout())
  if (.rush_env$has_tty) {
    options(
      width = if (is.null(width)) cli::console_width() else width
    )
  }

  invisible()
}

#' Read an input file
#'
#' Reads a data file, detecting format from extension. Used in generated rush
#' scripts to replace verbose inline read calls.
#'
#' @param path File path, or "-" for stdin.
#' @param format Input format. Defaults to "auto" (detect from extension).
#' @param delimiter Delimiter for delimited text files.
#' @param col_names Whether the file has a header row.
#' @param clean_names Whether to apply janitor::clean_names().
#' @param sheet Sheet name or number for Excel files.
#'
#' @return A data frame, or a named list of data frames for database inputs.
#' @export
read <- function(
  path,
  format = "auto",
  delimiter = ",",
  col_names = TRUE,
  clean_names = TRUE,
  sheet = NULL
) {
  kind <- file_kind(path, format)

  result <- switch(kind,
    stdin = readr::read_delim(
      file("stdin", "rb", raw = TRUE),
      delim = delimiter,
      col_names = col_names
    ),
    delim = {
      delim <- delimiter
      if (is.character(path) && tolower(tools::file_ext(path)) == "tsv") {
        delim <- "\t"
      }
      readr::read_delim(path, delim = delim, col_names = col_names)
    },
    parquet = nanoparquet::read_parquet(path),
    json = jsonlite::fromJSON(path),
    jsonl = jsonlite::stream_in(file(path), verbose = FALSE),
    xlsx = {
      if (!is.null(sheet)) {
        readxl::read_excel(path, sheet = sheet)
      } else {
        readxl::read_excel(path)
      }
    },
    arrow = arrow::read_ipc_file(path),
    sav = haven::read_sav(path),
    por = haven::read_por(path),
    dta = haven::read_dta(path),
    sas7bdat = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    fwf = readr::read_fwf(path, col_positions = readr::fwf_empty(path)),
    rds = readRDS(path),
    ods = readODS::read_ods(path),
    fasta = microseq::readFasta(path),
    fastq = microseq::readFastq(path),
    yaml = {
      src <- if (path == "-") file("stdin") else path
      parsed <- yaml::read_yaml(src)
      if (is.list(parsed) && length(parsed) > 0 && is.list(parsed[[1]])) {
        do.call(rbind, lapply(parsed, as.data.frame))
      } else {
        as.data.frame(parsed)
      }
    },
    toml = {
      actual_path <- if (path == "-") {
        tmp <- tempfile(fileext = ".toml")
        writeLines(readLines(file("stdin")), tmp)
        tmp
      } else {
        path
      }
      parsed <- RcppTOML::parseTOML(actual_path)
      items <- parsed[[1]]
      if (is.list(items) && length(items) > 0 && is.list(items[[1]])) {
        as.data.frame(do.call(rbind, lapply(items, as.data.frame)))
      } else {
        as.data.frame(parsed)
      }
    },
    xml = {
      actual_path <- if (path == "-") {
        tmp <- tempfile(fileext = ".xml")
        writeLines(readLines(file("stdin")), tmp)
        tmp
      } else {
        path
      }
      doc <- xml2::read_xml(actual_path)
      rows <- xml2::xml_children(doc)
      do.call(
        rbind,
        lapply(rows, function(row) {
          vals <- xml2::xml_text(xml2::xml_children(row))
          names(vals) <- xml2::xml_name(xml2::xml_children(row))
          as.data.frame(as.list(vals))
        })
      )
    },
    duckdb = {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      tables <- DBI::dbListTables(con)
      result <- stats::setNames(
        lapply(tables, function(t) DBI::dbReadTable(con, t)),
        tables
      )
      if (clean_names) {
        result <- lapply(result, janitor::clean_names)
      }
      return(result)
    },
    sqlite = {
      con <- DBI::dbConnect(RSQLite::SQLite(), path)
      on.exit(DBI::dbDisconnect(con))
      tables <- DBI::dbListTables(con)
      result <- stats::setNames(
        lapply(tables, function(t) DBI::dbReadTable(con, t)),
        tables
      )
      if (clean_names) {
        result <- lapply(result, janitor::clean_names)
      }
      return(result)
    },
    cli::cli_abort("Unsupported input format: {.val {kind}}")
  )

  if (clean_names && is.data.frame(result)) {
    result <- janitor::clean_names(result)
  }
  result
}

#' Write result output
#'
#' Dispatches the result to the appropriate output format based on the
#' configuration set by [rush::init()]. Handles data frames, atomic vectors,
#' lists, and ggplot objects.
#'
#' @param result The object to output.
#'
#' @export
write <- function(result) {
  cfg <- as.list(.rush_env)
  output <- cfg$output
  has_tty <- cfg$has_tty %||% isatty(stdout())

  stdout_binary <- function() {
    if (.Platform$OS.type == "windows") {
      file("stdout", "wb", raw = TRUE)
    } else {
      file("/dev/stdout", "wb", raw = TRUE)
    }
  }

  write_result <- function(result, output) {
    if (!is.null(cfg$head)) result <- utils::head(result, cfg$head)
    if (has_tty && is.null(output) && identical(cfg$output_format, "delim")) {
      options(
        tibble.width = if (is.null(cfg$width)) {
          cli::console_width()
        } else {
          cfg$width
        }
      )
      print(tibble::as_tibble(result), n = cfg$height)
    } else if (identical(cfg$output_format, "parquet")) {
      if (is.null(output)) stop("Parquet format requires --output (-o) file path")
      nanoparquet::write_parquet(result, output)
    } else if (identical(cfg$output_format, "json")) {
      pretty <- cfg$output_indent > 0L
      json <- jsonlite::toJSON(
        result, dataframe = "rows", pretty = pretty, auto_unbox = TRUE
      )
      if (is.null(output)) cat(json, "\n") else writeLines(json, output)
    } else if (identical(cfg$output_format, "jsonl")) {
      con_out <- if (is.null(output)) stdout() else file(output, "w")
      jsonlite::stream_out(result, con_out, verbose = FALSE)
      if (!is.null(output)) close(con_out)
    } else if (identical(cfg$output_format, "arrow")) {
      if (is.null(output)) stop("Arrow format requires --output (-o) file path")
      arrow::write_ipc_file(result, output)
    } else if (identical(cfg$output_format, "xlsx")) {
      xlsx_data <- if (!is.null(cfg$output_sheet)) {
        stats::setNames(list(result), cfg$output_sheet)
      } else {
        result
      }
      writexl::write_xlsx(xlsx_data, output)
    } else if (identical(cfg$output_format, "sav")) {
      haven::write_sav(result, output)
    } else if (identical(cfg$output_format, "zsav")) {
      haven::write_sav(result, output, compress = "zsav")
    } else if (identical(cfg$output_format, "dta")) {
      haven::write_dta(result, output)
    } else if (identical(cfg$output_format, "sas7bdat")) {
      haven::write_sas(result, output)
    } else if (identical(cfg$output_format, "xpt")) {
      haven::write_xpt(result, output)
    } else if (identical(cfg$output_format, "duckdb")) {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = output)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      if (is.data.frame(result)) {
        DBI::dbWriteTable(con, "data", result)
      } else if (is.list(result)) {
        for (tbl_name in names(result)) {
          DBI::dbWriteTable(con, tbl_name, result[[tbl_name]])
        }
      }
    } else if (identical(cfg$output_format, "sqlite")) {
      con <- DBI::dbConnect(RSQLite::SQLite(), output)
      on.exit(DBI::dbDisconnect(con))
      if (is.data.frame(result)) {
        DBI::dbWriteTable(con, "data", result)
      } else if (is.list(result)) {
        for (tbl_name in names(result)) {
          DBI::dbWriteTable(con, tbl_name, result[[tbl_name]])
        }
      }
    } else if (identical(cfg$output_format, "rds")) {
      saveRDS(result, output)
    } else if (identical(cfg$output_format, "ods")) {
      readODS::write_ods(result, output)
    } else if (identical(cfg$output_format, "fasta")) {
      microseq::writeFasta(result, output)
    } else if (identical(cfg$output_format, "fastq")) {
      microseq::writeFastq(result, output)
    } else if (identical(cfg$output_format, "yaml")) {
      rows <- lapply(seq_len(nrow(result)), function(i) {
        as.list(result[i, , drop = FALSE])
      })
      yaml_out <- yaml::as.yaml(rows, indent = cfg$output_indent)
      if (is.null(output)) cat(yaml_out) else writeLines(yaml_out, output)
    } else if (identical(cfg$output_format, "toml")) {
      dq <- rawToChar(as.raw(0x22))
      lines <- character(0)
      for (i in seq_len(nrow(result))) {
        lines <- c(lines, paste0("[[", cfg$output_record, "]]"))
        for (col in names(result)) {
          val <- result[[col]][i]
          key <- paste0(dq, col, dq)
          if (is.character(val) || is.factor(val)) {
            lines <- c(lines, paste0(key, " = ", dq, as.character(val), dq))
          } else if (is.logical(val)) {
            lines <- c(lines, paste0(key, " = ", tolower(val)))
          } else {
            lines <- c(lines, paste0(key, " = ", val))
          }
        }
        lines <- c(lines, "")
      }
      toml_out <- paste(lines, collapse = "\n")
      if (is.null(output)) cat(toml_out, "\n") else writeLines(toml_out, output)
    } else if (identical(cfg$output_format, "xml")) {
      root <- xml2::xml_new_root(cfg$output_root)
      for (i in seq_len(nrow(result))) {
        row_node <- xml2::xml_add_child(root, cfg$output_record)
        for (col in names(result)) {
          xml2::xml_add_child(row_node, col, as.character(result[[col]][i]))
        }
      }
      if (is.null(output)) {
        cat(as.character(root))
      } else {
        xml2::write_xml(root, output)
      }
    } else {
      con <- if (is.null(output)) stdout_binary() else output
      readr::write_delim(result, con, delim = cfg$delimiter %||% ",")
    }
  }

  # Plot dispatch
  if (inherits(result, "gg")) {
    out <- output
    w <- cfg$width
    h <- cfg$height
    if (is.null(out)) out <- if (has_tty) "ansi" else "png"

    if (out %in% c("ansi", "ascii")) {
      if (is.null(w)) w <- cli::console_width()
      devoutansi::ansi(
        width = w, height = h, plain_ascii = TRUE, char_lookup_table = 2
      )
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
      ggplot2::ggsave(
        output_filename, result,
        device = device,
        width = w, height = h, units = cfg$units, dpi = cfg$dpi
      )
      if (cat_output) {
        contents <- readBin(output_filename, raw(), n = 1e8)
        writeBin(contents, stdout_binary())
      }
    }
    return(invisible())
  }

  # Template dispatch
  if (!is.null(cfg$output_template)) {
    expand_field <- function(value, spec) {
      if (grepl("l$", spec)) {
        tolower(sprintf(paste0("%", sub("l$", "s", spec)), value))
      } else if (grepl("u$", spec)) {
        toupper(sprintf(paste0("%", sub("u$", "s", spec)), value))
      } else {
        sprintf(paste0("%", spec), value)
      }
    }
    expand_template <- function(tmpl, file_name, file_index, table_name, table_index) {
      while (grepl("%[(][^)]+[)]([^%]*[a-z])", tmpl)) {
        m <- regexec("%[(]([^)]+)[)]([^%]*[a-z])", tmpl)[[1]]
        full <- regmatches(tmpl, list(m))[[1]]
        field <- full[2]
        spec <- full[3]
        value <- switch(field,
          file_name = file_name, file_index = file_index,
          table_name = table_name, table_index = table_index,
          ""
        )
        replacement <- expand_field(value, spec)
        tmpl <- sub("%[(]([^)]+)[)]([^%]*[a-z])", replacement, tmpl)
      }
      tmpl
    }
    file_index <- 0L
    for (fname in names(result)) {
      file_index <- file_index + 1L
      item <- result[[fname]]
      if (is.data.frame(item)) {
        path <- expand_template(cfg$output_template, fname, file_index, "", 0L)
        write_result(item, path)
      } else if (is.list(item)) {
        table_index <- 0L
        for (tname in names(item)) {
          table_index <- table_index + 1L
          path <- expand_template(
            cfg$output_template, fname, file_index, tname, table_index
          )
          write_result(item[[tname]], path)
        }
      }
    }
  } else if (rlang::is_atomic(result)) {
    if (is.null(output)) {
      cli::cat_line(result)
    } else {
      write_result(tibble::tibble(x = result), output)
    }
  } else if (is.data.frame(result)) {
    write_result(result, output)
  } else if (rlang::is_bare_list(result)) {
    if (cfg$output_format %in% c("duckdb", "sqlite")) {
      write_result(result, output)
    } else {
      result <- tibble::enframe(result)
      write_result(result, output)
    }
  }

  invisible()
}
