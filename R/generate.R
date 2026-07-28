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
  } else if (identical(flags$resolved_output_format, "toml")) {
    pkgs <- c(pkgs, "RcppTOML")
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
