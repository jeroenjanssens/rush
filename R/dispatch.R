# The code at the end of an inline (--no-rush) script that writes `result`.
#
# Everything that rush knows when it generates the script, such as the output
# format, the output file, and the options for them, is written into the code
# as literal values, so the script only contains the code it needs. Only what
# depends on the result itself, and on whether standard output is a terminal,
# is decided when the script runs.
#
# The script runs in the `ir run` (or `Rscript`) subprocess, which inherits
# the parent's stdio, so TTY detection and binary output behave exactly as if
# rush had produced the output itself.

# R code for a value, e.g. "," becomes "\",\"" and NULL becomes "NULL"
lit <- function(x) {
  paste(deparse(x), collapse = "")
}

# Lines of R code from a template, where `<<name>>` is replaced by the value
# of `name`. Leading indentation common to all lines is removed.
code <- function(x, .envir = parent.frame()) {
  x <- glue::glue(x, .open = "<<", .close = ">>", .null = "", .envir = .envir)
  strsplit(as.character(x), "\n", fixed = TRUE)[[1]]
}

indent_lines <- function(lines, n = 1L) {
  ifelse(nzchar(lines), paste0(strrep("  ", n), lines), lines)
}

# One branch of an if-else chain; `cond = NULL` is the final `else`
branch <- function(cond, ...) {
  list(cond = cond, body = c(...))
}

# An if-else chain of branches
code_chain <- function(branches) {
  branches <- purrr::compact(branches)
  if (length(branches) == 1 && is.null(branches[[1]]$cond)) {
    return(branches[[1]]$body)
  }
  out <- character()
  for (i in seq_along(branches)) {
    cond <- branches[[i]]$cond
    open <- if (i == 1) {
      paste0("if (", cond, ") {")
    } else if (is.null(cond)) {
      "} else {"
    } else {
      paste0("} else if (", cond, ") {")
    }
    out <- c(out, open, indent_lines(branches[[i]]$body))
  }
  c(out, "}")
}

dispatch_block <- function(command, flags) {
  # Helpers that the generated code turns out to need
  needs <- new.env(parent = emptyenv())
  needs$tty <- FALSE
  needs$stdout_binary <- FALSE

  body <- if (command == "plot") {
    plot_lines(flags, needs, restyle = is.null(flags$post))
  } else {
    dispatch_data(command, flags, needs)
  }

  header <- character()
  if (needs$tty) {
    header <- c(header, ".has_tty <- isatty(stdout())")
  }
  if (needs$stdout_binary) {
    header <- c(
      header,
      code(
        '
        .stdout_binary <- function() {
          if (.Platform$OS.type == "windows") {
            file("stdout", "wb", raw = TRUE)
          } else {
            file("/dev/stdout", "wb", raw = TRUE)
          }
        }'
      )
    )
  }
  if (length(header) > 0) {
    header <- c(header, "")
  }
  c(header, body)
}

# The console width, for printing on a terminal
width_code <- function(flags) {
  if (is.null(flags$width)) "cli::console_width()" else lit(flags$width)
}

# --- Plots ------------------------------------------------------------------

# Write the ggplot in `result`: as ANSI or ASCII art, saved to a file, or as
# image bytes on standard output. Without -o or an explicit -O, that's ANSI
# art on a terminal and PNG bytes otherwise. `restyle` swaps in a minimal
# theme for terminal output.
plot_lines <- function(flags, needs, restyle = FALSE) {
  fmt <- flags$resolved_output_format
  target <- flags$output %||%
    (if (!fmt %in% c("auto", "delim")) fmt)

  if (is.null(target)) {
    needs$tty <- TRUE
    return(code_chain(list(
      branch(".has_tty", terminal_plot_lines(flags, "ansi", restyle)),
      branch(NULL, ggsave_lines(flags, "png", needs))
    )))
  }
  if (target %in% c("ansi", "ascii")) {
    terminal_plot_lines(flags, target, restyle)
  } else {
    ggsave_lines(flags, target, needs)
  }
}

terminal_plot_lines <- function(flags, style, restyle) {
  size <- paste0(
    "width = ",
    width_code(flags),
    if (!is.null(flags$height)) paste0(", height = ", lit(flags$height))
  )
  lines <- if (restyle) {
    code(
      "
      result <- result +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid = ggplot2::element_blank())"
    )
  }
  if (style == "ascii") {
    # Render to a file so that trailing whitespace can be trimmed
    c(
      lines,
      code(
        '
        .ascii_file <- tempfile(fileext = ".txt")
        devoutansi::ansi(
          filename = .ascii_file,
          <<size>>,
          plain_ascii = TRUE,
          char_lookup_table = 2,
          colour = FALSE
        )
        print(result)
        invisible(grDevices::dev.off())
        cat(sub(" +$", "", readLines(.ascii_file, warn = FALSE)), sep = "\\n")'
      )
    )
  } else {
    c(
      lines,
      code(
        "
        devoutansi::ansi(<<size>>)
        print(result)
        invisible(grDevices::dev.off())"
      )
    )
  }
}

# Save the plot with ggsave(). A target without an extension, such as "png",
# is a device: the plot is saved to a temporary file and written to standard
# output.
ggsave_lines <- function(flags, target, needs) {
  args <- paste0(
    "width = ",
    lit(flags$width %||% 6),
    ", height = ",
    lit(flags$height %||% 4),
    ", units = ",
    lit(flags$units),
    ", dpi = ",
    lit(flags$dpi)
  )
  if (tools::file_ext(target) != "") {
    return(code("ggplot2::ggsave(<<lit(target)>>, result, <<args>>)"))
  }
  needs$stdout_binary <- TRUE
  code(
    "
    .file <- tempfile()
    ggplot2::ggsave(.file, result, device = <<lit(target)>>, <<args>>)
    writeBin(readBin(.file, raw(), n = file.size(.file)), .stdout_binary())
    unlink(.file)"
  )
}

# --- Data -------------------------------------------------------------------

dispatch_data <- function(command, flags, needs) {
  check <- result_type_check(flags)
  template <- flags$output_template
  if (!is.null(template)) {
    # Every file that the template expands to gets the same writer
    return(c(
      check,
      ".write_result <- function(result, output) {",
      indent_lines(writer_lines(flags, "output", needs)),
      "}",
      "",
      template_lines(template)
    ))
  }

  out <- if (!is.null(flags$output)) lit(flags$output)
  fmt <- flags$resolved_output_format

  # A query always returns a data frame
  if (command == "sql") {
    return(writer_lines(flags, out, needs))
  }

  # With --result-type, the result has one known type, so its code can write
  # it directly
  type <- flags$result_type %||% "auto"
  if (command == "run" && type != "auto") {
    return(c(check, typed_result_lines(type, flags, out, needs)))
  }

  write <- function(x) paste0(".write_result(", x, ")")
  lines <- c(
    ".write_result <- function(result) {",
    indent_lines(writer_lines(flags, out, needs)),
    "}",
    ""
  )

  # The result of a conversion is a data frame or a list of them
  if (command == "convert") {
    return(c(
      lines,
      code_chain(list(
        branch("is.data.frame(result)", write("result")),
        branch(NULL, list_lines(fmt, write))
      ))
    ))
  }

  if (is.null(out)) {
    lines <- c(lines, print_width_line(flags, needs))
  }
  plot_possible <- fmt == "delim" || fmt %in% plot_output_formats

  c(
    lines,
    code_chain(list(
      if (plot_possible) {
        branch('inherits(result, "gg")', plot_lines(flags, needs))
      },
      branch(
        result_type_conds[["matrix"]],
        matrix_lines(write)
      ),
      branch(
        "rlang::is_atomic(result) && !is.null(names(result)) && length(dim(result)) <= 1",
        "# A named vector keeps its names",
        table_to_vector_lines(),
        write("tibble::enframe(result)")
      ),
      branch(
        "rlang::is_atomic(result)",
        if (is.null(out)) {
          "cli::cat_line(result)"
        } else {
          write("tibble::tibble(x = result)")
        }
      ),
      branch("is.data.frame(result)", write("result")),
      branch("rlang::is_bare_list(result)", list_lines(fmt, write)),
      branch(
        "!is.null(result)",
        "# Anything else, such as a function or a model, is printed as R prints it",
        "print(result)"
      )
    ))
  )
}

# Print other objects, such as models, as wide as the terminal
print_width_line <- function(flags, needs) {
  needs$tty <- TRUE
  paste0("if (.has_tty) options(width = ", width_code(flags), ")")
}

matrix_lines <- function(write) {
  c(
    "# A matrix (or two-way table) keeps its row and column names",
    code(
      '
      .m <- unclass(result)
      if (is.null(colnames(.m))) colnames(.m) <- paste0("V", seq_len(ncol(.m)))
      .rownames <- if (!is.null(rownames(.m))) "rowname"'
    ),
    write(
      'tibble::as_tibble(.m, rownames = .rownames, .name_repair = "minimal")'
    )
  )
}

# A table() or summary() is a named vector with a class; drop the class
table_to_vector_lines <- function() {
  code(
    '
    if (inherits(result, c("table", "summaryDefault"))) {
      result <- stats::setNames(as.vector(unclass(result)), names(result))
    }'
  )
}

# The code for a result of the type given with --result-type. Only one value
# is written, so the writer is inlined instead of defined as a function.
typed_result_lines <- function(type, flags, out, needs) {
  write <- function(x) {
    c(
      if (x != "result") paste0("result <- ", x),
      writer_lines(flags, out, needs)
    )
  }
  switch(
    type,
    "data-frame" = write("result"),
    matrix = matrix_lines(write),
    vector = if (is.null(out)) {
      # An unnamed vector is written one value per line; a named vector
      # keeps its names
      code_chain(list(
        branch("is.null(names(result))", "cli::cat_line(result)"),
        branch(NULL, table_to_vector_lines(), write("tibble::enframe(result)"))
      ))
    } else {
      c(
        table_to_vector_lines(),
        write(
          "if (is.null(names(result))) tibble::tibble(x = result) else tibble::enframe(result)"
        )
      )
    },
    list = list_lines(flags$resolved_output_format, write),
    plot = plot_lines(flags, needs),
    print = c(print_width_line(flags, needs), "print(result)")
  )
}

# --- Result types -------------------------------------------------------------

result_types <- c(
  "auto",
  "data-frame",
  "vector",
  "matrix",
  "list",
  "plot",
  "print"
)

result_type_aliases <- c(dataframe = "data-frame", data.frame = "data-frame")

# The condition that a result of each type meets, and how to name it
result_type_conds <- c(
  "data-frame" = "is.data.frame(result)",
  vector = "rlang::is_atomic(result) && length(dim(result)) <= 1",
  matrix = "rlang::is_atomic(result) && length(dim(result)) == 2",
  list = "rlang::is_bare_list(result)",
  plot = 'inherits(result, "gg")'
)

result_type_names <- c(
  "data-frame" = "a data frame",
  vector = "a vector",
  matrix = "a matrix",
  list = "a list",
  plot = "a ggplot"
)

# Check --result-type and its combination with the output options, and turn
# an alias such as "dataframe" into "data-frame"
resolve_result_type <- function(flags) {
  type <- flags$result_type %||% "auto"
  if (type %in% names(result_type_aliases)) {
    type <- result_type_aliases[[type]]
  }
  if (!type %in% result_types) {
    cli::cli_abort(
      c(
        "{.arg --result-type} must be one of {.or {.val {result_types}}}.",
        x = "Got {.val {type}}."
      ),
      call = NULL
    )
  }
  fmt <- flags$resolved_output_format
  if (type == "plot" && !(fmt == "delim" || fmt %in% plot_output_formats)) {
    cli::cli_abort(
      c(
        "A plot can't be written as {.val {fmt}}.",
        i = "Use {.code -O} with a plot format, such as {.val png} or {.val ansi}."
      ),
      call = NULL
    )
  }
  if (
    type %in%
      c("data-frame", "vector", "matrix", "list") &&
      fmt %in% plot_output_formats
  ) {
    cli::cli_abort(
      c(
        "{.code -O {fmt}} is for plots, but {.code --result-type {type}} isn't a plot.",
        i = "Use {.code --result-type plot}, or a data format such as {.val csv}."
      ),
      call = NULL
    )
  }
  if (
    type == "print" &&
      (!is.null(flags$output) ||
        !is.null(flags$output_template) ||
        !identical(flags$output_format %||% "auto", "auto"))
  ) {
    cli::cli_abort(
      c(
        "{.code --result-type print} prints the result, so it can't be combined with {.code -o} or {.code -O}.",
        i = "Use another result type to write the result to a file or in a format."
      ),
      call = NULL
    )
  }
  if (!is.null(flags$output_template) && !type %in% c("auto", "list")) {
    cli::cli_abort(
      c(
        "An output template writes a list of data frames, so it needs {.code --result-type list}.",
        x = "Got {.code --result-type {type}}."
      ),
      call = NULL
    )
  }
  flags$result_type <- type
  flags
}

# Code that stops the script when the result isn't of the type given with
# --result-type, so that a wrong guess doesn't give odd output
result_type_check <- function(flags) {
  type <- flags$result_type %||% "auto"
  if (!type %in% names(result_type_conds)) {
    return(NULL)
  }
  cond <- result_type_conds[[type]]
  if (grepl(" ", cond, fixed = TRUE)) {
    cond <- paste0("(", cond, ")")
  }
  message <- paste0(
    "The result isn't ",
    result_type_names[[type]],
    " (--result-type ",
    type,
    "): its class is "
  )
  c(
    paste0("if (!", cond, ") {"),
    paste0(
      "  stop(",
      lit(message),
      ", paste(class(result), collapse = \"/\"), call. = FALSE)"
    ),
    "}"
  )
}

# Write a list: as tables in a database, as sheets in a spreadsheet, or as one
# row per element otherwise
list_lines <- function(fmt, write) {
  if (fmt %in% c("duckdb", "sqlite")) {
    return(write("result"))
  }
  if (fmt %in% c("xlsx", "ods")) {
    return(c(
      "# Each data frame becomes a sheet; a database read as input contributes",
      "# its tables under their own names",
      code(
        '
        .sheets <- list()
        .nms <- if (is.null(names(result))) rep("", length(result)) else names(result)
        for (.i in seq_along(result)) {
          .item <- result[[.i]]
          .name <- if (nzchar(.nms[[.i]])) .nms[[.i]] else paste0("Sheet", .i)
          if (is.data.frame(.item)) {
            .sheets[[.name]] <- .item
          } else if (rlang::is_bare_list(.item) && length(.item) > 0 &&
                     all(vapply(.item, is.data.frame, logical(1)))) {
            .sheets[names(.item)] <- .item
          } else {
            .sheets <- NULL
            break
          }
        }
        if (length(.sheets) == 0) {
          stop("Can\'t write this list to a spreadsheet: ",
               "every element must be a data frame, which becomes a sheet.", call. = FALSE)
        }'
      ),
      write(".sheets")
    ))
  }
  c(
    "# Atomic elements become text, so that CSV doesn't write them as empty",
    code(
      '
      .flat <- vapply(result, function(.x) {
        if (is.atomic(.x)) paste(format(.x), collapse = ", ") else NA_character_
      }, character(1))
      result <- if (anyNA(.flat)) tibble::enframe(result) else tibble::enframe(.flat)'
    ),
    write("result")
  )
}

# The code that writes the data frame (or list of them) in `result` in the
# output format. `out` is R code for the output path, or NULL for standard
# output.
writer_lines <- function(flags, out, needs) {
  fmt <- flags$resolved_output_format
  to_stdout <- is.null(out)
  o <- out %||% "NULL"
  needs_file <- function(name) {
    paste0('stop("', name, ' format requires --output (-o) file path")')
  }
  indent <- flags$output_indent
  sheet <- flags$output_sheet

  lines <- switch(
    fmt,
    parquet = if (to_stdout) {
      needs_file("Parquet")
    } else {
      code("nanoparquet::write_parquet(result, <<o>>)")
    },
    json = c(
      code(
        '
        .json <- jsonlite::toJSON(
          result,
          dataframe = "rows",
          pretty = <<lit(if (indent > 0L) indent else FALSE)>>,
          auto_unbox = TRUE
        )'
      ),
      if (to_stdout) {
        'cat(.json, "\\n", sep = "")'
      } else {
        code("writeLines(.json, <<o>>)")
      }
    ),
    jsonl = if (to_stdout) {
      "jsonlite::stream_out(result, stdout(), verbose = FALSE)"
    } else {
      code(
        '
        .con_out <- file(<<o>>, "w")
        jsonlite::stream_out(result, .con_out, verbose = FALSE)
        close(.con_out)'
      )
    },
    arrow = if (to_stdout) {
      needs_file("Arrow")
    } else {
      code("arrow::write_ipc_file(result, <<o>>)")
    },
    xlsx = c(
      if (!is.null(sheet)) {
        code(
          "if (is.data.frame(result)) result <- stats::setNames(list(result), <<lit(sheet)>>)"
        )
      },
      code("writexl::write_xlsx(result, <<o>>)")
    ),
    sav = code("haven::write_sav(result, <<o>>)"),
    zsav = code('haven::write_sav(result, <<o>>, compress = "zsav")'),
    dta = code("haven::write_dta(result, <<o>>)"),
    sas7bdat = code("haven::write_sas(result, <<o>>)"),
    xpt = code("haven::write_xpt(result, <<o>>)"),
    duckdb = database_lines(
      code("DBI::dbConnect(duckdb::duckdb(), dbdir = <<o>>)"),
      "DBI::dbDisconnect(.con, shutdown = TRUE)"
    ),
    sqlite = database_lines(
      code("DBI::dbConnect(RSQLite::SQLite(), <<o>>)"),
      "DBI::dbDisconnect(.con)"
    ),
    rds = code("readr::write_rds(result, <<o>>)"),
    ods = if (is.null(sheet)) {
      code("readODS::write_ods(result, <<o>>)")
    } else {
      code(
        "
        if (is.data.frame(result)) {
          readODS::write_ods(result, <<o>>, sheet = <<lit(sheet)>>)
        } else {
          readODS::write_ods(result, <<o>>)
        }"
      )
    },
    fasta = sequence_lines("writeFasta", o, to_stdout, needs),
    fastq = sequence_lines("writeFastq", o, to_stdout, needs),
    yaml = c(
      code(
        "
        .rows <- lapply(seq_len(nrow(result)), function(.i) {
          as.list(result[.i, , drop = FALSE])
        })
        .yaml <- yaml::as.yaml(.rows, indent = <<lit(indent)>>)"
      ),
      if (to_stdout) "cat(.yaml)" else code("writeLines(.yaml, <<o>>)")
    ),
    toml = c(
      code(
        r'[
        .lines <- character(0)
        for (.i in seq_len(nrow(result))) {
          .lines <- c(.lines, <<lit(paste0("[[", flags$output_record, "]]"))>>)
          for (.col in names(result)) {
            .val <- result[[.col]][.i]
            .key <- paste0("\"", .col, "\"")
            if (is.character(.val) || is.factor(.val)) {
              .lines <- c(.lines, paste0(.key, " = \"", as.character(.val), "\""))
            } else if (is.logical(.val)) {
              .lines <- c(.lines, paste0(.key, " = ", tolower(.val)))
            } else {
              .lines <- c(.lines, paste0(.key, " = ", .val))
            }
          }
          .lines <- c(.lines, "")
        }
        .toml <- paste(.lines, collapse = "\n")]'
      ),
      if (to_stdout) 'cat(.toml, "\\n")' else code("writeLines(.toml, <<o>>)")
    ),
    xml = c(
      code(
        "
        .root <- xml2::xml_new_root(<<lit(flags$output_root)>>)
        for (.i in seq_len(nrow(result))) {
          .row_node <- xml2::xml_add_child(.root, <<lit(flags$output_record)>>)
          for (.col in names(result)) {
            xml2::xml_add_child(.row_node, .col, as.character(result[[.col]][.i]))
          }
        }"
      ),
      if (to_stdout) {
        "cat(as.character(.root))"
      } else {
        code("xml2::write_xml(.root, <<o>>)")
      }
    ),
    delim_lines(flags, out, needs)
  )
  c(
    if (!is.null(flags$head)) {
      paste0("result <- head(result, ", lit(flags$head), ")")
    },
    lines
  )
}

# Delimited text, such as CSV. With the default output format, standard output
# gets a printed tibble on a terminal.
delim_lines <- function(flags, out, needs) {
  delim <- flags$resolved_output_delimiter %||% ","
  header <- !flags$resolved_no_output_header
  con <- out
  if (is.null(out)) {
    needs$stdout_binary <- TRUE
    con <- ".stdout_binary()"
  }
  write <- code(
    "readr::write_delim(result, <<con>>, delim = <<lit(delim)>>, col_names = <<lit(header)>>)"
  )
  if (!is.null(out) || !identical(flags$resolved_output_format, "delim")) {
    return(write)
  }
  needs$tty <- TRUE
  n <- if (!is.null(flags$height)) paste0(", n = ", lit(flags$height))
  code_chain(list(
    branch(
      ".has_tty",
      # With the default result type, `rush run` sets the width for all
      # printing, at the top of the block
      if (
        identical(flags$command, "run") &&
          identical(flags$result_type %||% "auto", "auto")
      ) {
        code("options(tibble.width = <<width_code(flags)>>)")
      } else {
        code(
          "options(width = <<width_code(flags)>>, tibble.width = <<width_code(flags)>>)"
        )
      },
      code("print(tibble::as_tibble(result)<<n>>)")
    ),
    branch(NULL, write)
  ))
}

database_lines <- function(connect, disconnect) {
  c(
    paste0(".con <- ", connect),
    code(
      '
      if (is.data.frame(result)) {
        DBI::dbWriteTable(.con, "data", result)
      } else if (is.list(result)) {
        # A database read as input is itself a list of tables
        for (.name in names(result)) {
          .tables <- if (is.data.frame(result[[.name]])) result[.name] else result[[.name]]
          for (.tbl_name in names(.tables)) {
            DBI::dbWriteTable(.con, .tbl_name, .tables[[.tbl_name]])
          }
        }
      }'
    ),
    disconnect
  )
}

# FASTA and FASTQ. microseq needs a file and the columns Header, Sequence, and
# Quality, which clean names (as when reading a FASTA file) turns into
# lowercase.
sequence_lines <- function(fn, o, to_stdout, needs) {
  lines <- code(
    '
    .cols <- c("Header", "Sequence", "Quality")
    .match <- match(tolower(names(result)), tolower(.cols))
    names(result)[!is.na(.match)] <- .cols[.match[!is.na(.match)]]'
  )
  if (!to_stdout) {
    return(c(lines, code("microseq::<<fn>>(result, <<o>>)")))
  }
  needs$stdout_binary <- TRUE
  c(
    lines,
    code(
      "
      .file <- tempfile()
      microseq::<<fn>>(result, .file)
      writeBin(readBin(.file, raw(), n = file.size(.file)), .stdout_binary())
      unlink(.file)"
    )
  )
}

# Write every data frame in `result` to the file that the output template
# expands to, e.g. `%(file_name)s.csv`
template_lines <- function(template) {
  code(
    '
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
      pattern <- "%[(]([^)]+)[)]([-+ 0#]*[0-9]*[.]?[0-9]*[a-zA-Z])"
      while (grepl(pattern, tmpl)) {
        m <- regmatches(tmpl, regexec(pattern, tmpl))[[1]]
        value <- switch(m[2],
          file_name = file_name, file_index = file_index,
          table_name = table_name, table_index = table_index,
          "")
        tmpl <- sub(pattern, .expand_field(value, m[3]), tmpl)
      }
      tmpl
    }
    .file_index <- 0L
    for (.fname in names(result)) {
      .file_index <- .file_index + 1L
      .item <- result[[.fname]]
      if (is.data.frame(.item)) {
        .write_result(.item, .expand_template(<<lit(template)>>, .fname, .file_index, "", 0L))
      } else if (is.list(.item)) {
        .table_index <- 0L
        for (.tname in names(.item)) {
          .table_index <- .table_index + 1L
          .path <- .expand_template(<<lit(template)>>, .fname, .file_index, .tname, .table_index)
          .write_result(.item[[.tname]], .path)
        }
      }
    }'
  )
}
