skip_if_no_ir <- function() {
  skip_if_not(nzchar(Sys.which("ir")), "ir not available")
}

rush_run_exec <- function(expr = NULL, file = character(), args = list(), stdin_data = NULL) {
  a <- c("run")
  if (!is.null(args[["output_format"]])) a <- c(a, "-O", args[["output_format"]])
  if (!is.null(args[["input_format"]])) a <- c(a, "-F", args[["input_format"]])
  if (!is.null(args[["output"]])) a <- c(a, "-o", args[["output"]])
  if (!is.null(args[["delimiter"]])) a <- c(a, "-d", args[["delimiter"]])
  if (!is.null(args[["input_delimiter"]])) a <- c(a, "--input-delimiter", args[["input_delimiter"]])
  if (!is.null(args[["output_delimiter"]])) a <- c(a, "-D", args[["output_delimiter"]])
  if (isTRUE(args[["no_header"]])) a <- c(a, "-H")
  if (isTRUE(args[["no_clean_names"]])) a <- c(a, "-C")
  if (!is.null(args[["head"]])) a <- c(a, "--head", as.character(args[["head"]]))
  if (!is.null(args[["seed"]])) a <- c(a, "--seed", as.character(args[["seed"]]))
  if (!is.null(args[["library"]])) a <- c(a, "-l", args[["library"]])
  if (isTRUE(args[["tidyverse"]])) a <- c(a, "-t")
  if (!is.null(args[["output_root"]])) a <- c(a, "--output-root", args[["output_root"]])
  if (!is.null(args[["output_record"]])) a <- c(a, "--output-record", args[["output_record"]])
  if (!is.null(args[["output_indent"]])) a <- c(a, "--output-indent", as.character(args[["output_indent"]]))
  if (!is.null(args[["output_sheet"]])) a <- c(a, "--output-sheet", args[["output_sheet"]])
  if (!is.null(args[["input_sheet"]])) a <- c(a, "--input-sheet", as.character(args[["input_sheet"]]))
  if (isTRUE(args[["no_ir"]])) a <- c(a, "--no-ir")
  if (!is.null(expr)) a <- c(a, expr)
  if (length(file) > 0) a <- c(a, "--", file)

  rush_raw(a, stdin_data = stdin_data)
}

rush_convert_exec <- function(file, args = list(), stdin_data = NULL) {
  a <- c("convert")
  if (!is.null(args[["output_format"]])) a <- c(a, "-O", args[["output_format"]])
  if (!is.null(args[["input_format"]])) a <- c(a, "-F", args[["input_format"]])
  if (!is.null(args[["output"]])) a <- c(a, "-o", args[["output"]])
  if (!is.null(args[["delimiter"]])) a <- c(a, "-d", args[["delimiter"]])
  if (!is.null(args[["input_delimiter"]])) a <- c(a, "--input-delimiter", args[["input_delimiter"]])
  if (!is.null(args[["output_delimiter"]])) a <- c(a, "-D", args[["output_delimiter"]])
  if (isTRUE(args[["no_header"]])) a <- c(a, "-H")
  if (isTRUE(args[["no_clean_names"]])) a <- c(a, "-C")
  if (!is.null(args[["head"]])) a <- c(a, "--head", as.character(args[["head"]]))
  if (!is.null(args[["output_root"]])) a <- c(a, "--output-root", args[["output_root"]])
  if (!is.null(args[["output_record"]])) a <- c(a, "--output-record", args[["output_record"]])
  if (!is.null(args[["output_indent"]])) a <- c(a, "--output-indent", as.character(args[["output_indent"]]))
  if (!is.null(args[["output_sheet"]])) a <- c(a, "--output-sheet", args[["output_sheet"]])
  if (!is.null(args[["input_sheet"]])) a <- c(a, "--input-sheet", as.character(args[["input_sheet"]]))
  a <- c(a, file)

  rush_raw(a, stdin_data = stdin_data)
}

rush_plot_exec <- function(file = character(), args = list(), stdin_data = NULL) {
  a <- c("plot")
  if (!is.null(args[["x"]])) a <- c(a, "-x", args[["x"]])
  if (!is.null(args[["y"]])) a <- c(a, "-y", args[["y"]])
  if (!is.null(args[["z"]])) a <- c(a, "-z", args[["z"]])
  if (!is.null(args[["color"]])) a <- c(a, "-c", args[["color"]])
  if (!is.null(args[["fill"]])) a <- c(a, "-f", args[["fill"]])
  if (!is.null(args[["alpha"]])) a <- c(a, "--alpha", args[["alpha"]])
  if (!is.null(args[["size"]])) a <- c(a, "--size", args[["size"]])
  if (!is.null(args[["shape"]])) a <- c(a, "--shape", args[["shape"]])
  if (!is.null(args[["group"]])) a <- c(a, "--group", args[["group"]])
  if (!is.null(args[["aes"]])) a <- c(a, "--aes", args[["aes"]])
  if (!is.null(args[["geom"]])) a <- c(a, "-g", args[["geom"]])
  if (!is.null(args[["facets"]])) a <- c(a, "--facets", args[["facets"]])
  if (!is.null(args[["log"]])) a <- c(a, "--log", args[["log"]])
  if (!is.null(args[["title"]])) a <- c(a, "--title", args[["title"]])
  if (!is.null(args[["xlab"]])) a <- c(a, "--xlab", args[["xlab"]])
  if (!is.null(args[["ylab"]])) a <- c(a, "--ylab", args[["ylab"]])
  if (isTRUE(args[["margins"]])) a <- c(a, "--margins")
  if (!is.null(args[["pre"]])) a <- c(a, "--pre", args[["pre"]])
  if (!is.null(args[["post"]])) a <- c(a, "--post", args[["post"]])
  if (!is.null(args[["output"]])) a <- c(a, "-o", args[["output"]])
  if (!is.null(args[["width"]])) a <- c(a, "-w", as.character(args[["width"]]))
  if (!is.null(args[["height"]])) a <- c(a, "--height", as.character(args[["height"]]))
  if (!is.null(args[["units"]])) a <- c(a, "--units", args[["units"]])
  if (!is.null(args[["dpi"]])) a <- c(a, "--dpi", as.character(args[["dpi"]]))
  if (!is.null(args[["input_format"]])) a <- c(a, "-F", args[["input_format"]])
  if (!is.null(args[["delimiter"]])) a <- c(a, "-d", args[["delimiter"]])
  if (!is.null(args[["input_delimiter"]])) a <- c(a, "--input-delimiter", args[["input_delimiter"]])
  if (isTRUE(args[["no_header"]])) a <- c(a, "-H")
  if (isTRUE(args[["no_clean_names"]])) a <- c(a, "-C")
  if (!is.null(args[["library"]])) a <- c(a, "-l", args[["library"]])
  if (isTRUE(args[["tidyverse"]])) a <- c(a, "-t")
  if (!is.null(args[["seed"]])) a <- c(a, "--seed", as.character(args[["seed"]]))
  if (!is.null(args[["input_sheet"]])) a <- c(a, "--input-sheet", as.character(args[["input_sheet"]]))
  if (length(file) > 0) a <- c(a, "--", file)

  rush_raw(a, stdin_data = stdin_data)
}

rush_sql_exec <- function(query, file = character(), args = list(), stdin_data = NULL) {
  a <- c("sql")
  if (!is.null(args[["output_format"]])) a <- c(a, "-O", args[["output_format"]])
  if (!is.null(args[["output"]])) a <- c(a, "-o", args[["output"]])
  if (!is.null(args[["delimiter"]])) a <- c(a, "-d", args[["delimiter"]])
  if (!is.null(args[["output_delimiter"]])) a <- c(a, "-D", args[["output_delimiter"]])
  if (!is.null(args[["head"]])) a <- c(a, "--head", as.character(args[["head"]]))
  if (!is.null(args[["library"]])) a <- c(a, "-l", args[["library"]])
  if (isTRUE(args[["tidyverse"]])) a <- c(a, "-t")
  if (!is.null(args[["seed"]])) a <- c(a, "--seed", as.character(args[["seed"]]))
  a <- c(a, query)
  if (length(file) > 0) a <- c(a, "--", file)

  rush_raw(a, stdin_data = stdin_data)
}

rush_raw <- function(argv, stdin_data = NULL) {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "library(rush)",
    paste0("rush(", paste(deparse(argv), collapse = ""), ")")
  ), script)

  stdin_file <- if (!is.null(stdin_data)) {
    f <- withr::local_tempfile()
    writeLines(stdin_data, f)
    f
  }

  result <- processx::run(
    "Rscript",
    script,
    stdin = stdin_file,
    stdout = "|",
    stderr = "|",
    error_on_status = FALSE,
    env = c("current", RUSH_NO_IR = "")
  )
  result
}

make_csv <- function(df, dir, name = "data.csv") {
  path <- file.path(dir, name)
  readr::write_csv(df, path)
  path
}

make_tsv <- function(df, dir, name = "data.tsv") {
  path <- file.path(dir, name)
  readr::write_tsv(df, path)
  path
}

make_json <- function(df, dir, name = "data.json") {
  path <- file.path(dir, name)
  writeLines(jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE), path)
  path
}

make_jsonl <- function(df, dir, name = "data.jsonl") {
  path <- file.path(dir, name)
  con <- file(path, "w")
  jsonlite::stream_out(df, con, verbose = FALSE)
  close(con)
  path
}

make_yaml <- function(df, dir, name = "data.yaml") {
  path <- file.path(dir, name)
  rows <- lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  writeLines(yaml::as.yaml(rows), path)
  path
}

make_toml <- function(df, dir, name = "data.toml", record = "record") {
  path <- file.path(dir, name)
  dq <- '"'
  lines <- character(0)
  for (i in seq_len(nrow(df))) {
    lines <- c(lines, paste0("[[", record, "]]"))
    for (col in names(df)) {
      val <- df[[col]][i]
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
  writeLines(lines, path)
  path
}

make_xml <- function(df, dir, name = "data.xml", root = "root", record = "record") {
  path <- file.path(dir, name)
  root_node <- xml2::xml_new_root(root)
  for (i in seq_len(nrow(df))) {
    row_node <- xml2::xml_add_child(root_node, record)
    for (col in names(df)) {
      xml2::xml_add_child(row_node, col, as.character(df[[col]][i]))
    }
  }
  xml2::write_xml(root_node, path)
  path
}

make_parquet <- function(df, dir, name = "data.parquet") {
  path <- file.path(dir, name)
  nanoparquet::write_parquet(df, path)
  path
}

make_rds <- function(obj, dir, name = "data.rds") {
  path <- file.path(dir, name)
  saveRDS(obj, path)
  path
}

make_xlsx <- function(df_or_list, dir, name = "data.xlsx") {
  path <- file.path(dir, name)
  writexl::write_xlsx(df_or_list, path)
  path
}

make_duckdb <- function(tables, dir, name = "data.duckdb") {
  path <- file.path(dir, name)
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (tname in names(tables)) {
    DBI::dbWriteTable(con, tname, tables[[tname]])
  }
  path
}

make_sqlite <- function(tables, dir, name = "data.sqlite") {
  path <- file.path(dir, name)
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  for (tname in names(tables)) {
    DBI::dbWriteTable(con, tname, tables[[tname]])
  }
  path
}

stdout_lines <- function(result) {
  lines <- strsplit(result$stdout, "\n")[[1]]
  lines[nzchar(lines)]
}

test_df <- function() {
  data.frame(
    name = c("Alice", "Bob", "Carol"),
    score = c(95, 82, 91),
    pass = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

numeric_df <- function() {
  data.frame(
    x = c(1.5, 2.7, 3.14),
    y = c(10L, 20L, 30L),
    z = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
}
