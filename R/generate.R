build_flags <- function(
  command,
  expr = NULL,
  query = NULL,
  file = character(),
  output = NULL,
  output_format = "auto",
  input_format = "auto",
  delimiter = ",",
  input_delimiter = NULL,
  output_delimiter = NULL,
  header = TRUE,
  clean_names = TRUE,
  library = NULL,
  tidyverse = FALSE,
  head = NULL,
  seed = NULL,
  dry_run = FALSE,
  no_ir = FALSE,
  no_rush = FALSE,
  verbose = FALSE,
  x = NULL,
  y = NULL,
  z = NULL,
  color = NULL,
  fill = NULL,
  alpha = NULL,
  size = NULL,
  shape = NULL,
  group = NULL,
  aes = NULL,
  geom = "auto",
  facets = NULL,
  log = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  margins = FALSE,
  pre = NULL,
  post = NULL,
  width = NULL,
  height = NULL,
  units = "in",
  dpi = 300,
  output_root = "root",
  output_record = "record",
  output_sheet = NULL,
  output_indent = 2L,
  input_sheet = NULL
) {
  expression <- if (!is.null(expr)) {
    if (is.character(expr)) {
      rlang::parse_exprs(expr)
    } else if (is.language(expr)) {
      list(expr)
    } else if (is.list(expr)) {
      expr
    } else {
      cli::cli_abort(
        "{.arg expr} must be a character string or language object."
      )
    }
  }

  query_val <- if (!is.null(query)) {
    if (!is.character(query) || length(query) != 1) {
      cli::cli_abort("{.arg query} must be a single character string.")
    }
    query
  }

  lib_syms <- if (!is.null(library)) {
    purrr::map(library, rlang::sym)
  }

  seed_val <- if (!is.null(seed)) as.integer(seed)
  head_val <- if (!is.null(head)) as.integer(head)

  aes_sym <- function(val) {
    if (is.null(val)) {
      return(NULL)
    }
    if (is.character(val)) {
      return(rlang::sym(val))
    }
    val
  }

  facets_val <- if (!is.null(facets)) {
    if (is.character(facets)) {
      rlang::parse_expr(facets)
    } else {
      facets
    }
  }

  pre_val <- if (!is.null(pre)) {
    if (is.character(pre)) rlang::parse_exprs(pre) else pre
  }

  post_val <- if (!is.null(post)) {
    if (is.character(post)) rlang::parse_exprs(post) else post
  }

  list(
    command = command,
    expression = expression,
    query = query_val,
    file = file,
    output = output,
    output_format = output_format,
    input_format = input_format,
    delimiter = delimiter,
    input_delimiter = input_delimiter,
    output_delimiter = output_delimiter,
    no_header = !header,
    no_clean_names = !clean_names,
    library = lib_syms,
    tidyverse = tidyverse,
    head = head_val,
    input_sheet = input_sheet,
    seed = seed_val,
    dry_run = dry_run,
    no_ir = no_ir,
    no_rush = no_rush,
    verbose = verbose,
    x = aes_sym(x),
    y = aes_sym(y),
    z = aes_sym(z),
    color = aes_sym(color),
    fill = aes_sym(fill),
    alpha = aes_sym(alpha),
    size = aes_sym(size),
    shape = aes_sym(shape),
    group = aes_sym(group),
    aes = aes,
    geom = geom,
    facets = facets_val,
    log = log,
    title = title,
    xlab = xlab,
    ylab = ylab,
    margins = margins,
    pre = pre_val,
    post = post_val,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    output_root = output_root,
    output_record = output_record,
    output_sheet = output_sheet,
    output_indent = output_indent,
    input_sheet = input_sheet
  )
}

resolve_flags <- function(flags) {
  flags$resolved_input_delimiter <- flags$input_delimiter %||% flags$delimiter
  flags$resolved_output_delimiter <- flags$output_delimiter %||% flags$delimiter
  if (identical(flags$input_format, "tsv") && is.null(flags$input_delimiter)) {
    flags$resolved_input_delimiter <- "\t"
  }
  if (
    identical(flags$output_format, "tsv") && is.null(flags$output_delimiter)
  ) {
    flags$resolved_output_delimiter <- "\t"
  }
  flags$resolved_output_format <- resolve_output_format(
    flags$output,
    flags$output_format
  )
  if (
    !is.null(flags$output) &&
      identical(flags$resolved_output_format, "delim") &&
      tolower(tools::file_ext(flags$output)) == "tsv" &&
      is.null(flags$output_delimiter)
  ) {
    flags$resolved_output_delimiter <- "\t"
  }

  if (!is.null(flags$output) && grepl("%\\(", flags$output)) {
    flags$output_template <- flags$output
    flags$output <- NULL
  } else {
    flags$output_template <- NULL
  }

  flags
}

generate_script <- function(command, flags) {
  if (flags$verbose) {
    cli::cat_rule("Arguments", file = stderr())
    cli::cat_bullet(
      purrr::map2(names(flags), flags, format_flag),
      bullet_col = "yellow",
      file = stderr()
    )
    cli::cat_rule(file = stderr())
  }

  if (flags$dry_run && !flags$no_rush) {
    generate_script_compact(command, flags)
  } else {
    generate_script_inline(command, flags)
  }
}

generate_script_compact <- function(command, flags) {
  body_file <- tempfile()
  body <- file(body_file, open = "w")
  on.exit(unlink(body_file), add = TRUE)

  pkgs <- "rush"

  writeLines(compact_init_call(flags), body)
  writeLines("", body)

  if (is.integer(flags$seed)) {
    code_expression(body, set.seed(!!flags$seed))
  }

  switch(
    command,
    run = build_run_body_compact(body, flags),
    sql = build_sql_body_compact(body, flags),
    plot = build_plot_body_compact(body, flags),
    convert = build_convert_body_compact(body, flags)
  )

  writeLines("", body)
  writeLines("rush::write(result)", body)

  close(body)

  filename <- tempfile(fileext = ".R")
  on.exit(unlink(filename), add = TRUE)

  writeLines(
    c(
      frontmatter(pkgs),
      "",
      readLines(body_file)
    ),
    filename
  )

  if (flags$dry_run) {
    print_dry_run(filename)
    return(invisible(NULL))
  }

  run_generated_script(filename, flags)
}

generate_script_inline <- function(command, flags) {
  body_file <- tempfile()
  body <- file(body_file, open = "w")
  on.exit(unlink(body_file), add = TRUE)

  pkgs <- c("rlang", "cli", "tibble", "readr")

  if (is.integer(flags$seed)) {
    code_expression(body, set.seed(!!flags$seed))
  }

  result <- switch(
    command,
    run = build_run_body(body, flags),
    sql = build_sql_body(body, flags),
    plot = build_plot_body(body, flags),
    convert = build_convert_body(body, flags)
  )
  pkgs <- c(pkgs, result)

  if (identical(flags$resolved_output_format, "parquet")) {
    pkgs <- c(pkgs, "nanoparquet")
  } else if (flags$resolved_output_format %in% c("json", "jsonl")) {
    pkgs <- c(pkgs, "jsonlite")
  } else if (identical(flags$resolved_output_format, "arrow")) {
    pkgs <- c(pkgs, "arrow")
  } else if (identical(flags$resolved_output_format, "xlsx")) {
    pkgs <- c(pkgs, "writexl")
  } else if (
    flags$resolved_output_format %in% c("sav", "zsav", "dta", "sas7bdat", "xpt")
  ) {
    pkgs <- c(pkgs, "haven")
  } else if (identical(flags$resolved_output_format, "duckdb")) {
    pkgs <- c(pkgs, "duckdb", "DBI")
  } else if (identical(flags$resolved_output_format, "sqlite")) {
    pkgs <- c(pkgs, "RSQLite", "DBI")
  } else if (identical(flags$resolved_output_format, "ods")) {
    pkgs <- c(pkgs, "readODS")
  } else if (flags$resolved_output_format %in% c("fasta", "fastq")) {
    pkgs <- c(pkgs, "microseq")
  } else if (identical(flags$resolved_output_format, "yaml")) {
    pkgs <- c(pkgs, "yaml")
  } else if (identical(flags$resolved_output_format, "xml")) {
    pkgs <- c(pkgs, "xml2")
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
      dispatch_block(command)
    ),
    filename
  )

  if (flags$dry_run) {
    print_dry_run(filename)
    return(invisible(NULL))
  }

  run_generated_script(filename, flags)
}

# --- Compact mode helpers ---------------------------------------------------

compact_init_call <- function(flags) {
  defaults <- list(
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
  )

  args <- list(
    output = flags$output,
    output_format = flags$resolved_output_format,
    delimiter = flags$resolved_output_delimiter,
    head = flags$head,
    width = flags$width,
    height = flags$height,
    units = flags$units,
    dpi = flags$dpi,
    output_root = flags$output_root,
    output_record = flags$output_record,
    output_sheet = flags$output_sheet,
    output_indent = flags$output_indent,
    output_template = flags$output_template
  )

  non_default <- purrr::imap(args, function(val, nm) {
    if (identical(val, defaults[[nm]])) NULL else val
  })
  non_default <- purrr::compact(non_default)

  if (length(non_default) == 0) {
    return("rush::init()")
  }

  arg_strs <- purrr::imap_chr(non_default, function(val, nm) {
    paste0("  ", nm, " = ", deparse(val, width.cutoff = 500L))
  })
  c(
    "rush::init(",
    paste0(arg_strs, c(rep(",", length(arg_strs) - 1L), "")),
    ")"
  )
}

compact_read_call <- function(path, flags) {
  args <- list()
  kind <- file_kind(path, flags$input_format %||% "auto")

  if (!identical(flags$input_format %||% "auto", "auto")) {
    args$format <- flags$input_format
  }
  if (kind == "delim" && !is.null(flags$resolved_input_delimiter) &&
    flags$resolved_input_delimiter != ",") {
    args$delimiter <- flags$resolved_input_delimiter
  }
  if (flags$no_header) {
    args$col_names <- FALSE
  }
  if (flags$no_clean_names) {
    args$clean_names <- FALSE
  }
  if (!is.null(flags$input_sheet)) {
    args$sheet <- flags$input_sheet
  }

  if (length(args) == 0) {
    return(paste0("rush::read(", deparse(path), ")"))
  }

  arg_strs <- c(
    deparse(path),
    purrr::imap_chr(args, function(val, nm) {
      paste0(nm, " = ", deparse(val, width.cutoff = 500L))
    })
  )
  paste0("rush::read(", paste(arg_strs, collapse = ", "), ")")
}

build_run_body_compact <- function(con, flags) {
  if (is.null(flags$expression) && length(flags$file) == 0) {
    cli::cli_abort(c(
      "No expression to run.",
      i = "Provide an R expression, e.g. {.code rush run '1 + 1'}.",
      i = "See {.code rush run -h} for usage."
    ))
  }

  emit_compact_libraries(con, flags)

  if (length(flags$file) >= 1) {
    emit_compact_reads(con, flags$file, flags)
  }

  if (!is.null(flags$expression)) {
    emit_result_exprs(con, flags$expression)
  } else if (length(flags$file) == 1) {
    code_expression(con, result <- df)
  }
}

build_sql_body_compact <- function(con, flags) {
  if (is.null(flags$query)) {
    cli::cli_abort(c(
      "No query to run.",
      i = "Provide a SQL query, e.g. {.code rush sql 'SELECT 1'}.",
      i = "See {.code rush sql -h} for usage."
    ))
  }

  emit_compact_libraries(con, flags)
  emit_sql(con, flags$query, flags$file, flags)
}

build_plot_body_compact <- function(con, flags) {
  emit_compact_libraries(con, flags, default = "ggplot2")

  plot_files <- flags$file %||% "-"
  if (length(plot_files) == 0) plot_files <- "-"
  emit_compact_reads(con, plot_files, flags)

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
}

build_convert_body_compact <- function(con, flags) {
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

  emit_compact_libraries(con, flags)
  emit_compact_reads(con, flags$file, flags)

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
}

emit_compact_libraries <- function(con, flags, default = character()) {
  if (flags$tidyverse) {
    code_library(con, "tidyverse")
    code_library(con, "glue")
  } else if (length(default) > 0) {
    purrr::walk(default, function(e) code_library(con, e))
  }
  if (!is.null(flags$library)) {
    purrr::walk(flags$library, function(e) code_library(con, e))
  }
}

emit_compact_reads <- function(con, files, flags) {
  df_names <- input_names(files)
  code_expression(con, dfs <- list())

  for (i in seq_along(files)) {
    read_call <- compact_read_call(files[[i]], flags)
    writeLines(
      paste0("dfs[[\"", df_names[[i]], "\"]] <- ", read_call),
      con
    )
  }

  code_expression(con, df <- dfs[[1L]])
}
