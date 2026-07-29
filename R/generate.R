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
  sheet = NULL,
  seed = NULL,
  dry_run = FALSE,
  no_ir = FALSE,
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
  dpi = 300
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
    sheet = sheet,
    seed = seed_val,
    dry_run = dry_run,
    no_ir = no_ir,
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
    dpi = dpi
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
