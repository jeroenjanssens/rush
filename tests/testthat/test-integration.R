# Integration tests that actually execute rush commands via ir/Rscript
# and verify real output. Every test is gated on ir being available.

# Section 1: Scalar and vector output -------------------------------------------

test_that("scalar numeric prints without [1] prefix", {
  skip_if_no_ir()
  result <- rush_run_exec("6 * 7")
  expect_equal(stdout_lines(result), "42")
})

test_that("scalar float prints correctly", {
  skip_if_no_ir()
  result <- rush_run_exec("pi")
  expect_match(stdout_lines(result), "^3\\.14159")
})

test_that("scalar string prints without quotes", {
  skip_if_no_ir()
  result <- rush_run_exec('paste("hello")')
  expect_equal(stdout_lines(result), "hello")
})

test_that("integer vector prints one value per line", {
  skip_if_no_ir()
  result <- rush_run_exec("seq(5)")
  expect_equal(stdout_lines(result), as.character(1:5))
})

test_that("character vector prints one value per line", {

  skip_if_no_ir()
  result <- rush_run_exec("LETTERS[1:4]")
  expect_equal(stdout_lines(result), c("A", "B", "C", "D"))
})

test_that("logical vector prints one value per line", {
  skip_if_no_ir()
  result <- rush_run_exec("c(TRUE, FALSE, TRUE)")
  expect_equal(stdout_lines(result), c("TRUE", "FALSE", "TRUE"))
})

test_that("named vector prints values only", {
  skip_if_no_ir()
  result <- rush_run_exec("c(a = 1, b = 2, c = 3)")
  lines <- stdout_lines(result)
  expect_true(all(c("1", "2", "3") %in% lines))
})

test_that("invisible NULL produces no output", {
  skip_if_no_ir()
  result <- rush_run_exec("invisible(NULL)")
  expect_equal(nchar(trimws(result$stdout)), 0)
})

test_that("long vector prints without index prefixes", {
  skip_if_no_ir()
  result <- rush_run_exec("1:100")
  lines <- stdout_lines(result)
  expect_equal(length(lines), 100)
  expect_false(any(grepl("^\\[", lines)))
})

test_that("paste result prints correctly", {
  skip_if_no_ir()
  result <- rush_run_exec('paste("a", "b")')
  expect_equal(stdout_lines(result), "a b")
})

# Section 2: Scalar/vector to file ---------------------------------------------

test_that("vector to CSV creates one-column file", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("seq(5)", args = list(output = out))
  expect_equal(result$status, 0)
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 5)
  expect_equal(names(df), "x")
  expect_equal(df$x, 1:5)
})

test_that("scalar to CSV creates one-row file", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("6 * 7", args = list(output = out))
  expect_equal(result$status, 0)
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 1)
  expect_equal(df$x, 42)
})

test_that("vector to JSON creates array", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("LETTERS[1:3]", args = list(output = out))
  expect_equal(result$status, 0)
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 3)
})

test_that("vector to Parquet is readable", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out.parquet")
  result <- rush_run_exec("seq(3)", args = list(output = out))
  expect_equal(result$status, 0)
  df <- nanoparquet::read_parquet(out)
  expect_equal(nrow(df), 3)
  expect_equal(df$x, 1:3)
})

test_that("logical vector to CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("c(TRUE, FALSE)", args = list(output = out))
  expect_equal(result$status, 0)
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$x, c(TRUE, FALSE))
})

# Section 3: Expression features ------------------------------------------------

test_that("multiple semicolons work, last value returned", {
  skip_if_no_ir()
  result <- rush_run_exec("x <- 5; x * 2")
  expect_equal(stdout_lines(result), "10")
})

test_that("sum of sequence via intermediate", {
  skip_if_no_ir()
  result <- rush_run_exec("x <- 1:5; sum(x)")
  expect_equal(stdout_lines(result), "15")
})

test_that("data.frame expression to CSV stdout", {
  skip_if_no_ir()
  result <- rush_run_exec(
    'data.frame(a = 1:3, b = c("x","y","z"))',
    args = list(output_format = "csv")
  )
  lines <- stdout_lines(result)
  expect_equal(lines[1], "a,b")
  expect_equal(length(lines), 4)
})

test_that("three expressions, last is result", {
  skip_if_no_ir()
  result <- rush_run_exec("a <- 1; b <- 2; a + b")
  expect_equal(stdout_lines(result), "3")
})

# Section 4: Reading flat formats -----------------------------------------------

test_that("read CSV: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read CSV: column names are cleaned", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df <- data.frame(`First Name` = "Alice", `Last Name` = "Smith", check.names = FALSE)
  f <- make_csv(df, dir)
  result <- rush_run_exec("names(df)", file = f)
  lines <- stdout_lines(result)
  expect_true("first_name" %in% lines)
  expect_true("last_name" %in% lines)
})

test_that("read TSV: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_tsv(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read TSV: values are correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_tsv(test_df(), dir)
  result <- rush_run_exec("df$name[1]", file = f)
  expect_equal(stdout_lines(result), "Alice")
})

test_that("read with semicolon delimiter", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.csv")
  writeLines(c("a;b", "1;2", "3;4"), path)
  result <- rush_run_exec("nrow(df)", file = path, args = list(delimiter = ";"))
  expect_equal(stdout_lines(result), "2")
})

test_that("read with --input-delimiter override", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.csv")
  writeLines(c("a\tb", "1\t2", "3\t4"), path)
  result <- rush_run_exec("nrow(df)", file = path, args = list(input_delimiter = "\t"))
  expect_equal(stdout_lines(result), "2")
})

test_that("read with -H (no header): columns named x1, x2", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.csv")
  writeLines(c("1,2", "3,4", "5,6"), path)
  result <- rush_run_exec("names(df)", file = path, args = list(no_header = TRUE))
  lines <- stdout_lines(result)
  expect_true("x1" %in% lines)
  expect_true("x2" %in% lines)
})

test_that("read with -H: first row is data not header", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.csv")
  writeLines(c("10,20", "30,40"), path)
  result <- rush_run_exec("df$x1[1]", file = path, args = list(no_header = TRUE))
  expect_equal(stdout_lines(result), "10")
})

test_that("read with -C preserves original names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df <- data.frame(`First Name` = "Alice", `Score (%)` = 95, check.names = FALSE)
  f <- make_csv(df, dir)
  result <- rush_run_exec("names(df)", file = f, args = list(no_clean_names = TRUE))
  lines <- stdout_lines(result)
  expect_true("First Name" %in% lines)
})

test_that("read default cleans names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df <- data.frame(`First Name` = "Alice", check.names = FALSE)
  f <- make_csv(df, dir)
  result <- rush_run_exec("names(df)", file = f)
  expect_equal(stdout_lines(result), "first_name")
})

# Section 5: Reading nested formats ---------------------------------------------

test_that("read JSON: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read JSON: column values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  result <- rush_run_exec("df$name[2]", file = f)
  expect_equal(stdout_lines(result), "Bob")
})

test_that("read JSONL: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_jsonl(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read JSONL: values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_jsonl(test_df(), dir)
  result <- rush_run_exec("df$score[1]", file = f)
  expect_equal(stdout_lines(result), "95")
})

test_that("read YAML: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read YAML: column names correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  result <- rush_run_exec("names(df)", file = f)
  lines <- stdout_lines(result)
  expect_true("name" %in% lines)
  expect_true("score" %in% lines)
})

test_that("read TOML: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read TOML: values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  result <- rush_run_exec("df$name[1]", file = f)
  expect_equal(stdout_lines(result), "Alice")
})

test_that("read XML: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read XML: column names and values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  result <- rush_run_exec("df$name[3]", file = f)
  expect_equal(stdout_lines(result), "Carol")
})

test_that("read XML with non-standard root/record names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir, root = "people", record = "person")
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

# Section 6: Reading binary/special formats -------------------------------------

test_that("read Parquet: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_parquet(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read Parquet: values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_parquet(test_df(), dir)
  result <- rush_run_exec("df$score[2]", file = f)
  expect_equal(stdout_lines(result), "82")
})

test_that("read RDS: data frame class", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_rds(test_df(), dir)
  result <- rush_run_exec("class(df)[1]", file = f)
  expect_equal(stdout_lines(result), "data.frame")
})

test_that("read RDS: non-data-frame object", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_rds(1:10, dir)
  result <- rush_run_exec("sum(df)", file = f, args = list(no_clean_names = TRUE))
  expect_equal(stdout_lines(result), "55")
})

test_that("read Excel: correct row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xlsx(test_df(), dir)
  result <- rush_run_exec("nrow(df)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read Excel: values correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xlsx(test_df(), dir)
  result <- rush_run_exec("df$name[1]", file = f)
  expect_equal(stdout_lines(result), "Alice")
})

test_that("read Excel with --input-sheet by name", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  sheets <- list(Sales = data.frame(a = 1:2), Costs = data.frame(b = 3:4))
  f <- make_xlsx(sheets, dir)
  result <- rush_run_exec("names(df)", file = f, args = list(input_sheet = "Costs"))
  expect_equal(stdout_lines(result), "b")
})

test_that("read Excel with --input-sheet by index", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  sheets <- list(Sales = data.frame(a = 1:2), Costs = data.frame(b = 3:4))
  f <- make_xlsx(sheets, dir)
  result <- rush_run_exec("names(df)", file = f, args = list(input_sheet = 2))
  expect_equal(stdout_lines(result), "b")
})

test_that("read DuckDB: table names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(t1 = data.frame(x = 1:3), t2 = data.frame(y = 4:6))
  f <- make_duckdb(tables, dir)
  result <- rush_run_exec("names(dfs$data)", file = f)
  lines <- stdout_lines(result)
  expect_true("t1" %in% lines)
  expect_true("t2" %in% lines)
})

test_that("read DuckDB: access table values", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(t1 = data.frame(x = 1:3))
  f <- make_duckdb(tables, dir)
  result <- rush_run_exec("nrow(dfs$data$t1)", file = f)
  expect_equal(stdout_lines(result), "3")
})

test_that("read SQLite: table names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(t1 = data.frame(x = 1:3), t2 = data.frame(y = 4:6))
  f <- make_sqlite(tables, dir)
  result <- rush_run_exec("names(dfs$data)", file = f)
  lines <- stdout_lines(result)
  expect_true("t1" %in% lines)
  expect_true("t2" %in% lines)
})

test_that("read SQLite: access table values", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(t1 = data.frame(x = 10:12))
  f <- make_sqlite(tables, dir)
  result <- rush_run_exec("sum(dfs$data$t1$x)", file = f)
  expect_equal(stdout_lines(result), "33")
})

# Section 7: Writing flat formats -----------------------------------------------

test_that("write CSV: header and values match", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("write TSV: tab-separated output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.tsv")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  lines <- readLines(out)
  expect_true(grepl("\t", lines[1]))
})

test_that("write with -D semicolon delimiter", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_delimiter = ";"))
  expect_equal(result$status, 0)
  lines <- readLines(out)
  expect_true(grepl(";", lines[1]))
})

test_that("-O csv to stdout produces CSV header + rows", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "csv"))
  lines <- stdout_lines(result)
  expect_match(lines[1], "name,score,pass")
  expect_equal(length(lines), 4)
})

test_that("-O tsv to stdout produces tab-separated", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "tsv"))
  lines <- stdout_lines(result)
  expect_true(grepl("\t", lines[1]))
})

test_that("default stdout on non-TTY produces delimited output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f)
  lines <- stdout_lines(result)
  expect_true(grepl(",", lines[1]))
})

# Section 8: Writing nested formats to files ------------------------------------

test_that("write JSON file: valid and correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 3)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("write JSONL file: one object per line", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.jsonl")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  lines <- readLines(out)
  expect_equal(length(lines), 3)
  first <- jsonlite::fromJSON(lines[1])
  expect_equal(first$name, "Alice")
})

test_that("write YAML file: valid", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  parsed <- yaml::read_yaml(out)
  expect_equal(length(parsed), 3)
  expect_equal(parsed[[1]]$name, "Alice")
})

test_that("write TOML file: valid with [[record]] headers", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  content <- readLines(out)
  expect_true(any(grepl("^\\[\\[record\\]\\]$", content)))
  expect_true(any(grepl('"name"', content)))
})

test_that("write XML file: valid structure", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "root")
  children <- xml2::xml_children(doc)
  expect_equal(length(children), 3)
  expect_equal(xml2::xml_name(children[[1]]), "record")
})

test_that("write Parquet file: readable", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.parquet")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- nanoparquet::read_parquet(out)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("write RDS file: identical round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.rds")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- readRDS(out)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("write Excel file: readable", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- readxl::read_excel(out)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("write DuckDB file: table created", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.duckdb")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = out, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_true("data" %in% DBI::dbListTables(con))
  df <- DBI::dbReadTable(con, "data")
  expect_equal(nrow(df), 3)
})

test_that("write SQLite file: table created", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.sqlite")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  expect_equal(result$status, 0)
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  expect_true("data" %in% DBI::dbListTables(con))
  df <- DBI::dbReadTable(con, "data")
  expect_equal(nrow(df), 3)
})

# Section 9: Writing nested formats to stdout -----------------------------------

test_that("-O json to stdout: valid JSON", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "json"))
  parsed <- jsonlite::fromJSON(result$stdout)
  expect_equal(nrow(parsed), 3)
})

test_that("-O jsonl to stdout: one object per line", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "jsonl"))
  lines <- stdout_lines(result)
  expect_equal(length(lines), 3)
  expect_no_error(jsonlite::fromJSON(lines[1]))
})

test_that("-O yaml to stdout: valid YAML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "yaml"))
  parsed <- yaml::yaml.load(result$stdout)
  expect_equal(length(parsed), 3)
})

test_that("-O toml to stdout: has [[record]] headers", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "toml"))
  expect_true(grepl("\\[\\[record\\]\\]", result$stdout))
})

test_that("-O xml to stdout: valid XML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "xml"))
  doc <- xml2::read_xml(result$stdout)
  expect_equal(xml2::xml_name(doc), "root")
  expect_equal(length(xml2::xml_children(doc)), 3)
})

# Section 10: --output-root and --output-record (XML) ---------------------------

test_that("XML default: root='root', record='record'", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "root")
  expect_equal(xml2::xml_name(xml2::xml_children(doc)[[1]]), "record")
})

test_that("XML custom --output-root and --output-record", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(
    output = out, output_root = "plants", output_record = "observation"
  ))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "plants")
  expect_equal(xml2::xml_name(xml2::xml_children(doc)[[1]]), "observation")
})

test_that("XML custom root only", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(
    output = out, output_root = "items"
  ))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "items")
  expect_equal(xml2::xml_name(xml2::xml_children(doc)[[1]]), "record")
})

test_that("XML custom record only", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(
    output = out, output_record = "entry"
  ))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "root")
  expect_equal(xml2::xml_name(xml2::xml_children(doc)[[1]]), "entry")
})

# Section 11: --output-record (TOML) -------------------------------------------

test_that("TOML default: uses [[record]]", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  content <- readLines(out)
  expect_true(any(grepl("^\\[\\[record\\]\\]$", content)))
})

test_that("TOML custom --output-record", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_run_exec("df", file = f, args = list(
    output = out, output_record = "item"
  ))
  content <- readLines(out)
  expect_true(any(grepl("^\\[\\[item\\]\\]$", content)))
  expect_false(any(grepl("^\\[\\[record\\]\\]$", content)))
})

test_that("TOML custom --output-record 'measurement'", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_run_exec("df", file = f, args = list(
    output = out, output_record = "measurement"
  ))
  content <- readLines(out)
  expect_true(any(grepl("^\\[\\[measurement\\]\\]$", content)))
})

# Section 12: --output-indent (JSON) -------------------------------------------

test_that("JSON default indent 2: pretty with 2 spaces", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  content <- readLines(out)
  indented <- content[grepl("^  ", content)]
  expect_true(length(indented) > 0)
  four_indented <- content[grepl("^    ", content)]
  expect_true(length(four_indented) > 0)
})

test_that("JSON --output-indent 4: pretty-printed", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_indent = 4))
  content <- readLines(out)
  expect_true(length(content) > 1)
  indented <- content[grepl("^  ", content)]
  expect_true(length(indented) > 0)
})

test_that("JSON --output-indent 0: compact single line", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_indent = 0))
  content <- readLines(out)
  expect_equal(length(content), 1)
})

# Section 13: --output-indent (YAML) -------------------------------------------

test_that("YAML default indent 2", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  content <- readLines(out)
  two_indented <- content[grepl("^  \\w", content)]
  expect_true(length(two_indented) > 0)
})

test_that("YAML --output-indent 4", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_indent = 4))
  content <- readLines(out)
  four_indented <- content[grepl("^    \\w", content)]
  expect_true(length(four_indented) > 0)
})

# Section 14: --output-sheet (Excel) -------------------------------------------

test_that("Excel default sheet name", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_run_exec("df", file = f, args = list(output = out))
  sheets <- readxl::excel_sheets(out)
  expect_equal(length(sheets), 1)
})

test_that("Excel --output-sheet 'Results'", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_sheet = "Results"))
  sheets <- readxl::excel_sheets(out)
  expect_equal(sheets, "Results")
})

test_that("Excel --output-sheet with spaces", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_run_exec("df", file = f, args = list(output = out, output_sheet = "My Data"))
  sheets <- readxl::excel_sheets(out)
  expect_equal(sheets, "My Data")
})

# Section 15: --head (row limiting) ---------------------------------------------

test_that("--head 2 limits CSV output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 2))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 2)
})

test_that("--head 2 limits JSON output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 2))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 2)
})

test_that("--head 2 limits YAML output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 2))
  parsed <- yaml::read_yaml(out)
  expect_equal(length(parsed), 2)
})

test_that("--head 2 limits XML output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 2))
  doc <- xml2::read_xml(out)
  expect_equal(length(xml2::xml_children(doc)), 2)
})

test_that("--head 2 limits TOML output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 2))
  content <- readLines(out)
  record_lines <- grep("^\\[\\[record\\]\\]$", content)
  expect_equal(length(record_lines), 2)
})

test_that("--head larger than data returns all rows", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_run_exec("df", file = f, args = list(output = out, head = 100))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("--head with rush convert", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_convert_exec(f, args = list(output = out, head = 1))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 1)
})

# Section 16: --seed ------------------------------------------------------------

test_that("same seed produces same output", {
  skip_if_no_ir()
  r1 <- rush_run_exec("sample(1:100, 5)", args = list(seed = 42))
  r2 <- rush_run_exec("sample(1:100, 5)", args = list(seed = 42))
  expect_equal(stdout_lines(r1), stdout_lines(r2))
})

test_that("different seed produces different output", {
  skip_if_no_ir()
  r1 <- rush_run_exec("sample(1:100, 5)", args = list(seed = 42))
  r2 <- rush_run_exec("sample(1:100, 5)", args = list(seed = 99))
  expect_false(identical(stdout_lines(r1), stdout_lines(r2)))
})

# Section 17: --library and --tidyverse -----------------------------------------

test_that("-l stringr loads package", {
  skip_if_no_ir()
  result <- rush_run_exec(
    'stringr::str_to_upper("hi")',
    args = list(library = "stringr")
  )
  expect_equal(stdout_lines(result), "HI")
})

test_that("-t loads tidyverse (filter works unqualified)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec(
    "nrow(filter(df, score > 90))",
    file = f,
    args = list(tidyverse = TRUE)
  )
  expect_equal(stdout_lines(result), "2")
})

test_that("-t loads glue", {
  skip_if_no_ir()
  result <- rush_run_exec(
    'glue("x is {1+1}")',
    args = list(tidyverse = TRUE)
  )
  expect_equal(stdout_lines(result), "x is 2")
})

# Section 18: Stdin reading -----------------------------------------------------

test_that("stdin CSV: correct row count", {
  skip_if_no_ir()
  csv_data <- c("a,b", "1,2", "3,4", "5,6")
  result <- rush_run_exec("nrow(df)", file = "-", stdin_data = csv_data)
  expect_equal(stdout_lines(result), "3")
})

test_that("stdin CSV: column names", {
  skip_if_no_ir()
  csv_data <- c("name,score", "Alice,95")
  result <- rush_run_exec("names(df)", file = "-", stdin_data = csv_data)
  lines <- stdout_lines(result)
  expect_true("name" %in% lines)
  expect_true("score" %in% lines)
})

test_that("stdin JSON with -F json", {
  skip_if_no_ir()
  df <- test_df()
  json_data <- jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE)
  result <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "json"),
    stdin_data = as.character(json_data)
  )
  expect_equal(stdout_lines(result), "3")
})

test_that("stdin YAML with -F yaml", {
  skip_if_no_ir()
  df <- test_df()
  rows <- lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  yaml_data <- strsplit(yaml::as.yaml(rows), "\n")[[1]]
  result <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "yaml"),
    stdin_data = yaml_data
  )
  expect_equal(stdout_lines(result), "3")
})

test_that("stdin TOML with -F toml", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  toml_data <- readLines(f)
  result <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "toml"),
    stdin_data = toml_data
  )
  expect_equal(stdout_lines(result), "3")
})

test_that("stdin XML with -F xml", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  xml_data <- readLines(f)
  result <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "xml"),
    stdin_data = xml_data
  )
  expect_equal(stdout_lines(result), "3")
})

test_that("stdin TSV with -F tsv", {
  skip_if_no_ir()
  tsv_data <- c("a\tb", "1\t2", "3\t4")
  result <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "tsv"),
    stdin_data = tsv_data
  )
  expect_equal(stdout_lines(result), "2")
})

test_that("stdin with -H (no header)", {
  skip_if_no_ir()
  csv_data <- c("1,2", "3,4")
  result <- rush_run_exec(
    "names(df)", file = "-",
    args = list(no_header = TRUE),
    stdin_data = csv_data
  )
  lines <- stdout_lines(result)
  expect_true("x1" %in% lines)
})

# Section 19: Pipeline chaining -------------------------------------------------

test_that("CSV stdout -> CSV stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "csv"))
  r2 <- rush_run_exec("nrow(df)", file = "-", stdin_data = strsplit(r1$stdout, "\n")[[1]])
  expect_equal(stdout_lines(r2), "2")
})

test_that("JSON stdout -> JSON stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "json"))
  r2 <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "json"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  expect_equal(stdout_lines(r2), "2")
})

test_that("YAML stdout -> YAML stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "yaml"))
  r2 <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "yaml"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  expect_equal(stdout_lines(r2), "2")
})

test_that("TOML stdout -> TOML stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "toml"))
  r2 <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "toml"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  expect_equal(stdout_lines(r2), "2")
})

test_that("XML stdout -> XML stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "xml"))
  r2 <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "xml"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  expect_equal(stdout_lines(r2), "2")
})

test_that("JSONL stdout -> JSONL stdin round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("head(df, 2)", file = f, args = list(output_format = "jsonl"))
  r2 <- rush_run_exec(
    "nrow(df)", file = "-",
    args = list(input_format = "jsonl"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  expect_equal(stdout_lines(r2), "2")
})

test_that("JSON pipe preserves column names", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  r1 <- rush_run_exec("df", file = f, args = list(output_format = "json"))
  r2 <- rush_run_exec(
    "names(df)", file = "-",
    args = list(input_format = "json"),
    stdin_data = strsplit(r1$stdout, "\n")[[1]]
  )
  lines <- stdout_lines(r2)
  expect_true("name" %in% lines)
  expect_true("score" %in% lines)
})

# Section 20: Flat -> nested conversions ----------------------------------------

test_that("CSV -> JSON: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
  expect_equal(parsed$score, c(95, 82, 91))
})

test_that("CSV -> YAML: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- yaml::read_yaml(out)
  expect_equal(parsed[[1]]$name, "Alice")
  expect_equal(parsed[[2]]$score, 82)
})

test_that("CSV -> TOML: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- RcppTOML::parseTOML(out)
  expect_equal(parsed$record[[1]]$name, "Alice")
  expect_equal(parsed$record[[2]]$score, 82)
})

test_that("CSV -> XML: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  rows <- xml2::xml_children(doc)
  first_row <- xml2::xml_children(rows[[1]])
  expect_equal(xml2::xml_text(first_row[[1]]), "Alice")
})

test_that("TSV -> JSON: columns preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_tsv(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("CSV -> JSONL: each line is valid", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.jsonl")
  rush_run_exec("df", file = f, args = list(output = out))
  lines <- readLines(out)
  expect_equal(length(lines), 3)
  first <- jsonlite::fromJSON(lines[1])
  expect_equal(first$name, "Alice")
})

# Section 21: Nested -> flat conversions ----------------------------------------

test_that("JSON -> CSV: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("YAML -> CSV: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("TOML -> CSV: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("XML -> CSV: columns and values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("JSONL -> CSV: all rows preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_jsonl(test_df(), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("JSON -> TSV: tab delimiter used", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.tsv")
  rush_run_exec("df", file = f, args = list(output = out))
  lines <- readLines(out)
  expect_true(grepl("\t", lines[1]))
})

# Section 22: Nested -> nested conversions --------------------------------------

test_that("JSON -> YAML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- yaml::read_yaml(out)
  expect_equal(parsed[[1]]$name, "Alice")
})

test_that("JSON -> XML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.xml")
  rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  rows <- xml2::xml_children(doc)
  expect_equal(length(rows), 3)
})

test_that("JSON -> TOML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.toml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- RcppTOML::parseTOML(out)
  expect_equal(parsed$record[[1]]$name, "Alice")
})

test_that("YAML -> JSON: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("YAML -> XML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  out <- file.path(dir, "out.xml")
  rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  expect_equal(length(xml2::xml_children(doc)), 3)
})

test_that("YAML -> TOML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  out <- file.path(dir, "out.toml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- RcppTOML::parseTOML(out)
  expect_equal(parsed$record[[1]]$name, "Alice")
})

test_that("XML -> JSON: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("XML -> YAML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- yaml::read_yaml(out)
  expect_equal(length(parsed), 3)
})

test_that("XML -> TOML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  out <- file.path(dir, "out.toml")
  rush_run_exec("df", file = f, args = list(output = out))
  content <- readLines(out)
  expect_true(any(grepl("Alice", content)))
})

test_that("TOML -> JSON: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("TOML -> YAML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- yaml::read_yaml(out)
  expect_equal(parsed[[1]]$name, "Alice")
})

test_that("TOML -> XML: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  out <- file.path(dir, "out.xml")
  rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  expect_equal(length(xml2::xml_children(doc)), 3)
})

test_that("JSONL -> JSON: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_jsonl(test_df(), dir)
  out <- file.path(dir, "out.json")
  rush_run_exec("df", file = f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 3)
})

test_that("JSON -> JSONL: values preserved", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.jsonl")
  rush_run_exec("df", file = f, args = list(output = out))
  lines <- readLines(out)
  expect_equal(length(lines), 3)
})

# Section 23: Full round-trip tests ---------------------------------------------

test_that("round-trip: CSV -> Parquet -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(numeric_df(), dir)
  pq <- file.path(dir, "mid.parquet")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = pq))
  rush_run_exec("df", file = pq, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$x, c(1.5, 2.7, 3.14))
  expect_equal(df$y, c(10, 20, 30))
})

test_that("round-trip: CSV -> JSON -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(numeric_df(), dir)
  mid <- file.path(dir, "mid.json")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$x, c(1.5, 2.7, 3.14))
})

test_that("round-trip: CSV -> YAML -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(numeric_df(), dir)
  mid <- file.path(dir, "mid.yaml")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$y, c(10, 20, 30))
})

test_that("round-trip: CSV -> TOML -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(numeric_df(), dir)
  mid <- file.path(dir, "mid.toml")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("round-trip: CSV -> XML -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(numeric_df(), dir)
  mid <- file.path(dir, "mid.xml")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("round-trip: CSV -> RDS -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  mid <- file.path(dir, "mid.rds")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("round-trip: CSV -> Excel -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  mid <- file.path(dir, "mid.xlsx")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("round-trip: CSV -> DuckDB -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  mid <- file.path(dir, "mid.duckdb")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("dfs$mid$data", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("round-trip: CSV -> SQLite -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  mid <- file.path(dir, "mid.sqlite")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("dfs$mid$data", file = mid, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

# Section 24: Multiple input files ----------------------------------------------

test_that("two CSVs: names(dfs) correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:2), dir, "a.csv")
  f2 <- make_csv(data.frame(y = 3:4), dir, "b.csv")
  result <- rush_run_exec("names(dfs)", file = c(f1, f2))
  lines <- stdout_lines(result)
  expect_true("a" %in% lines)
  expect_true("b" %in% lines)
})

test_that("two CSVs: nrow(dfs$a) correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:5), dir, "a.csv")
  f2 <- make_csv(data.frame(y = 1:3), dir, "b.csv")
  result <- rush_run_exec("nrow(dfs$a)", file = c(f1, f2))
  expect_equal(stdout_lines(result), "5")
})

test_that("two CSVs: nrow(dfs$b) correct", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:5), dir, "a.csv")
  f2 <- make_csv(data.frame(y = 1:3), dir, "b.csv")
  result <- rush_run_exec("nrow(dfs$b)", file = c(f1, f2))
  expect_equal(stdout_lines(result), "3")
})

test_that("CSV + TSV: mixed flat formats", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(a = 1:2), dir, "one.csv")
  f2 <- make_tsv(data.frame(b = 3:4), dir, "two.tsv")
  result <- rush_run_exec("names(dfs)", file = c(f1, f2))
  lines <- stdout_lines(result)
  expect_true("one" %in% lines)
  expect_true("two" %in% lines)
})

test_that("CSV + JSON: mixed flat + nested", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(a = 1:2), dir, "flat.csv")
  f2 <- make_json(data.frame(b = 3:4), dir, "nested.json")
  result <- rush_run_exec("nrow(dfs$nested)", file = c(f1, f2))
  expect_equal(stdout_lines(result), "2")
})

test_that("CSV + Parquet: mixed flat + binary", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(a = 1:2), dir, "text.csv")
  f2 <- make_parquet(data.frame(b = 5:7), dir, "bin.parquet")
  result <- rush_run_exec("nrow(dfs$bin)", file = c(f1, f2))
  expect_equal(stdout_lines(result), "3")
})

test_that("three files: all names present", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1), dir, "a.csv")
  f2 <- make_csv(data.frame(x = 1), dir, "b.csv")
  f3 <- make_csv(data.frame(x = 1), dir, "c.csv")
  result <- rush_run_exec("names(dfs)", file = c(f1, f2, f3))
  lines <- stdout_lines(result)
  expect_true(all(c("a", "b", "c") %in% lines))
})

test_that("name collision: files with same basename get suffixed", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  d1 <- file.path(dir, "a")
  d2 <- file.path(dir, "b")
  dir.create(d1)
  dir.create(d2)
  f1 <- make_csv(data.frame(x = 1:2), d1, "data.csv")
  f2 <- make_csv(data.frame(x = 3:5), d2, "data.csv")
  result <- rush_run_exec("names(dfs)", file = c(f1, f2))
  lines <- stdout_lines(result)
  expect_true("data" %in% lines)
  expect_true("data_1" %in% lines)
})

test_that("name collision: both data frames accessible", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  d1 <- file.path(dir, "a")
  d2 <- file.path(dir, "b")
  dir.create(d1)
  dir.create(d2)
  f1 <- make_csv(data.frame(x = 1:2), d1, "data.csv")
  f2 <- make_csv(data.frame(x = 3:5), d2, "data.csv")
  result <- rush_run_exec("nrow(dfs$data)", file = c(f1, f2))
  expect_equal(stdout_lines(result), "2")
  result2 <- rush_run_exec("nrow(dfs$data_1)", file = c(f1, f2))
  expect_equal(stdout_lines(result2), "3")
})

test_that("name collision: three files with same basename", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  d1 <- file.path(dir, "a")
  d2 <- file.path(dir, "b")
  d3 <- file.path(dir, "c")
  dir.create(d1)
  dir.create(d2)
  dir.create(d3)
  f1 <- make_csv(data.frame(x = 1:2), d1, "data.csv")
  f2 <- make_csv(data.frame(x = 3:5), d2, "data.csv")
  f3 <- make_csv(data.frame(x = 6:9), d3, "data.csv")
  result <- rush_run_exec("names(dfs)", file = c(f1, f2, f3))
  lines <- stdout_lines(result)
  expect_true("data" %in% lines)
  expect_true("data_1" %in% lines)
  expect_true("data_2" %in% lines)
  r1 <- rush_run_exec("nrow(dfs$data)", file = c(f1, f2, f3))
  expect_equal(stdout_lines(r1), "2")
  r2 <- rush_run_exec("nrow(dfs$data_1)", file = c(f1, f2, f3))
  expect_equal(stdout_lines(r2), "3")
  r3 <- rush_run_exec("nrow(dfs$data_2)", file = c(f1, f2, f3))
  expect_equal(stdout_lines(r3), "4")
})

test_that("digit-prefix file: dfs$x2024", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(val = 42), dir, "2024.csv")
  result <- rush_run_exec("dfs$x2024$val", file = f)
  expect_equal(stdout_lines(result), "42")
})

test_that("DuckDB input: table names via dfs", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(orders = data.frame(id = 1:3), items = data.frame(sku = c("a", "b")))
  f <- make_duckdb(tables, dir)
  result <- rush_run_exec("sort(names(dfs$data))", file = f)
  lines <- stdout_lines(result)
  expect_true("items" %in% lines)
  expect_true("orders" %in% lines)
})

test_that("DuckDB input: access table row count", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(orders = data.frame(id = 1:5))
  f <- make_duckdb(tables, dir)
  result <- rush_run_exec("nrow(dfs$data$orders)", file = f)
  expect_equal(stdout_lines(result), "5")
})

test_that("SQLite input: table names via dfs", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  tables <- list(users = data.frame(name = c("A", "B")))
  f <- make_sqlite(tables, dir)
  result <- rush_run_exec("names(dfs$data)", file = f)
  expect_equal(stdout_lines(result), "users")
})

# Section 25: rush convert end-to-end -------------------------------------------

test_that("convert CSV -> Parquet", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.parquet")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- nanoparquet::read_parquet(out)
  expect_equal(nrow(df), 3)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("convert CSV -> JSON", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("convert CSV -> YAML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  parsed <- yaml::read_yaml(out)
  expect_equal(parsed[[1]]$name, "Alice")
})

test_that("convert CSV -> TOML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  content <- readLines(out)
  expect_true(any(grepl("Alice", content)))
})

test_that("convert CSV -> XML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  doc <- xml2::read_xml(out)
  expect_equal(length(xml2::xml_children(doc)), 3)
})

test_that("convert CSV -> Excel", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- readxl::read_excel(out)
  expect_equal(nrow(df), 3)
})

test_that("convert CSV -> RDS", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.rds")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  df <- readRDS(out)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("convert CSV -> DuckDB", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.duckdb")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = out, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_true("data" %in% DBI::dbListTables(con))
})

test_that("convert CSV -> SQLite", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.sqlite")
  result <- rush_convert_exec(f, args = list(output = out))
  expect_equal(result$status, 0)
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  expect_true("data" %in% DBI::dbListTables(con))
})

test_that("convert JSON -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_convert_exec(f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("convert Parquet -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_parquet(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_convert_exec(f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 3)
})

test_that("convert Excel -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xlsx(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_convert_exec(f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("convert YAML -> JSON", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_yaml(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_convert_exec(f, args = list(output = out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$name, c("Alice", "Bob", "Carol"))
})

test_that("convert XML -> YAML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_xml(test_df(), dir)
  out <- file.path(dir, "out.yaml")
  result <- rush_convert_exec(f, args = list(output = out))
  parsed <- yaml::read_yaml(out)
  expect_equal(length(parsed), 3)
})

test_that("convert TOML -> XML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_toml(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_convert_exec(f, args = list(output = out))
  doc <- xml2::read_xml(out)
  expect_equal(length(xml2::xml_children(doc)), 3)
})

test_that("convert with --head 2", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_convert_exec(f, args = list(output = out, head = 2))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(nrow(parsed), 2)
})

test_that("convert with --output-root and --output-record (XML)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xml")
  result <- rush_convert_exec(f, args = list(
    output = out, output_root = "people", output_record = "person"
  ))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "people")
  expect_equal(xml2::xml_name(xml2::xml_children(doc)[[1]]), "person")
})

test_that("convert with --output-record (TOML)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.toml")
  result <- rush_convert_exec(f, args = list(output = out, output_record = "entry"))
  content <- readLines(out)
  expect_true(any(grepl("^\\[\\[entry\\]\\]$", content)))
})

test_that("convert with --output-indent 4 (JSON)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.json")
  result <- rush_convert_exec(f, args = list(output = out, output_indent = 4))
  content <- readLines(out)
  indented <- content[grepl("^  ", content)]
  expect_true(length(indented) > 0)
})

test_that("convert with --output-sheet (Excel)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.xlsx")
  result <- rush_convert_exec(f, args = list(output = out, output_sheet = "MySheet"))
  sheets <- readxl::excel_sheets(out)
  expect_equal(sheets, "MySheet")
})

test_that("convert with --input-sheet (Excel -> CSV)", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  sheets <- list(Sales = data.frame(a = 1:2), Costs = data.frame(b = 3:4))
  f <- make_xlsx(sheets, dir)
  out <- file.path(dir, "out.csv")
  result <- rush_convert_exec(f, args = list(output = out, input_sheet = "Costs"))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(names(df), "b")
})

test_that("convert with -F override: .txt treated as JSON", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.txt")
  writeLines(jsonlite::toJSON(test_df(), dataframe = "rows", auto_unbox = TRUE), path)
  out <- file.path(dir, "out.csv")
  result <- rush_convert_exec(path, args = list(output = out, input_format = "json"))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(df$name, c("Alice", "Bob", "Carol"))
})

test_that("convert with -O override: output to .txt as YAML", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.txt")
  result <- rush_convert_exec(f, args = list(output = out, output_format = "yaml"))
  parsed <- yaml::read_yaml(out)
  expect_equal(length(parsed), 3)
})

test_that("convert multiple CSVs -> DuckDB: tables named after files", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:2), dir, "orders.csv")
  f2 <- make_csv(data.frame(y = 3:4), dir, "items.csv")
  out <- file.path(dir, "out.duckdb")
  result <- rush_convert_exec(c(f1, f2), args = list(output = out))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = out, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  tables <- DBI::dbListTables(con)
  expect_true("orders" %in% tables)
  expect_true("items" %in% tables)
})

test_that("convert multiple CSVs -> SQLite: tables named after files", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:2), dir, "orders.csv")
  f2 <- make_csv(data.frame(y = 3:4), dir, "items.csv")
  out <- file.path(dir, "out.sqlite")
  result <- rush_convert_exec(c(f1, f2), args = list(output = out))
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  tables <- DBI::dbListTables(con)
  expect_true("orders" %in% tables)
  expect_true("items" %in% tables)
})

test_that("convert output template %(file_name)s.parquet", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(x = 1:2), dir, "a.csv")
  f2 <- make_csv(data.frame(x = 3:4), dir, "b.csv")
  out_tmpl <- file.path(dir, "%(file_name)s.parquet")
  result <- rush_convert_exec(c(f1, f2), args = list(output = out_tmpl))
  expect_true(file.exists(file.path(dir, "a.parquet")))
  expect_true(file.exists(file.path(dir, "b.parquet")))
})

# Section 26: rush sql end-to-end -----------------------------------------------

test_that("sql SELECT 1", {
  skip_if_no_ir()
  result <- rush_sql_exec("SELECT 1 AS x")
  lines <- stdout_lines(result)
  expect_true(any(grepl("1", lines)))
})

test_that("sql COUNT from CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_sql_exec("SELECT COUNT(*) AS n FROM data", file = f)
  lines <- stdout_lines(result)
  expect_true(any(grepl("3", lines)))
})

test_that("sql WHERE filter", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_sql_exec("SELECT name FROM data WHERE score > 90", file = f)
  lines <- stdout_lines(result)
  expect_true(any(grepl("Alice", lines)))
  expect_true(any(grepl("Carol", lines)))
  expect_false(any(grepl("Bob", lines)))
})

test_that("sql JOIN two CSVs", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f1 <- make_csv(data.frame(id = 1:3, name = c("A", "B", "C")), dir, "left.csv")
  f2 <- make_csv(data.frame(id = c(1, 3), val = c(10, 30)), dir, "right.csv")
  result <- rush_sql_exec(
    'SELECT "left".name, "right".val FROM "left" JOIN "right" USING(id)',
    file = c(f1, f2)
  )
  lines <- stdout_lines(result)
  expect_true(any(grepl("A", lines)))
  expect_true(any(grepl("30", lines)))
})

test_that("sql from Parquet", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_parquet(test_df(), dir)
  result <- rush_sql_exec("SELECT name FROM data LIMIT 1", file = f)
  lines <- stdout_lines(result)
  expect_true(any(grepl("Alice", lines)))
})

test_that("sql with -O json output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_sql_exec(
    "SELECT name FROM data",
    file = f,
    args = list(output_format = "json")
  )
  parsed <- jsonlite::fromJSON(result$stdout)
  expect_equal(nrow(parsed), 3)
})

test_that("sql with -o parquet output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.parquet")
  result <- rush_sql_exec(
    "SELECT * FROM data",
    file = f,
    args = list(output = out)
  )
  expect_equal(result$status, 0)
  df <- nanoparquet::read_parquet(out)
  expect_equal(nrow(df), 3)
})

test_that("sql with --head 2", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  out <- file.path(dir, "out.csv")
  result <- rush_sql_exec(
    "SELECT * FROM data",
    file = f,
    args = list(output = out, head = 2)
  )
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 2)
})

test_that("sql from stdin", {
  skip_if_no_ir()
  csv_data <- c("id,val", "1,10", "2,20", "3,30")
  result <- rush_sql_exec(
    "SELECT SUM(val) AS total FROM stdin",
    file = "-",
    stdin_data = csv_data
  )
  lines <- stdout_lines(result)
  expect_true(any(grepl("60", lines)))
})

# Section 27: rush plot end-to-end ----------------------------------------------

test_that("plot scatter to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(wt = runif(20), mpg = runif(20)), dir, "mtcars.csv")
  out <- file.path(dir, "plot.png")
  result <- rush_plot_exec(f, args = list(x = "wt", y = "mpg", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("plot histogram to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(mpg = rnorm(50)), dir, "data.csv")
  out <- file.path(dir, "hist.png")
  result <- rush_plot_exec(f, args = list(x = "mpg", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot line geom to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:10, y = cumsum(rnorm(10))), dir, "data.csv")
  out <- file.path(dir, "line.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", geom = "line", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot bar geom to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(cat = c("A", "B", "C", "A", "B")), dir, "data.csv")
  out <- file.path(dir, "bar.png")
  result <- rush_plot_exec(f, args = list(x = "cat", geom = "bar", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot boxplot geom to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(group = rep(c("A", "B"), each = 10), val = rnorm(20)),
    dir, "data.csv"
  )
  out <- file.path(dir, "box.png")
  result <- rush_plot_exec(f, args = list(x = "group", y = "val", geom = "boxplot", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot density geom to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = rnorm(50)), dir, "data.csv")
  out <- file.path(dir, "dens.png")
  result <- rush_plot_exec(f, args = list(x = "x", geom = "density", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot violin geom to PNG", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(group = rep(c("A", "B"), each = 20), val = rnorm(40)),
    dir, "data.csv"
  )
  out <- file.path(dir, "violin.png")
  result <- rush_plot_exec(f, args = list(
    x = "group", y = "val", geom = "violin", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot color aesthetic", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(x = 1:10, y = 1:10, g = rep(c("a", "b"), 5)),
    dir, "data.csv"
  )
  out <- file.path(dir, "color.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", color = "g", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot fill aesthetic", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(cat = c("A", "B", "C", "A"), grp = c("x", "x", "y", "y")),
    dir, "data.csv"
  )
  out <- file.path(dir, "fill.png")
  result <- rush_plot_exec(f, args = list(x = "cat", fill = "grp", geom = "bar", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot alpha aesthetic", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:10, y = 1:10, a = runif(10)), dir, "data.csv")
  out <- file.path(dir, "alpha.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", alpha = "a", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot size aesthetic", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:10, y = 1:10, s = 1:10), dir, "data.csv")
  out <- file.path(dir, "size.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", size = "s", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot shape aesthetic", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(x = 1:6, y = 1:6, sh = rep(c("a", "b"), 3)),
    dir, "data.csv"
  )
  out <- file.path(dir, "shape.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", shape = "sh", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot group aesthetic with line", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(x = rep(1:5, 2), y = rnorm(10), g = rep(c("a", "b"), each = 5)),
    dir, "data.csv"
  )
  out <- file.path(dir, "group.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", group = "g", geom = "line", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot facet_wrap", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(x = 1:12, y = rnorm(12), g = rep(c("A", "B", "C"), 4)),
    dir, "data.csv"
  )
  out <- file.path(dir, "fw.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", facets = "~ g", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot facet_grid", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(
      x = 1:12, y = rnorm(12),
      r = rep(c("R1", "R2"), 6), c = rep(c("C1", "C2", "C3"), 4)
    ),
    dir, "data.csv"
  )
  out <- file.path(dir, "fg.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", facets = "r ~ c", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot facet_grid with margins", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(
    data.frame(x = 1:8, y = rnorm(8), g1 = rep(c("A", "B"), 4), g2 = rep(c("X", "Y"), each = 4)),
    dir, "data.csv"
  )
  out <- file.path(dir, "margins.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", facets = "g1 ~ g2", margins = TRUE, output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot log x", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = c(1, 10, 100), y = c(1, 2, 3)), dir, "data.csv")
  out <- file.path(dir, "logx.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", log = "x", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot log y", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:3, y = c(1, 10, 100)), dir, "data.csv")
  out <- file.path(dir, "logy.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", log = "y", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot log xy", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = c(1, 10, 100), y = c(1, 10, 100)), dir, "data.csv")
  out <- file.path(dir, "logxy.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", log = "xy", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot title", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "title.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", title = "My Plot", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot xlab and ylab", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "labels.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", xlab = "Weight", ylab = "Mileage", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot title + xlab + ylab combined", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "all_labs.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", title = "T", xlab = "X", ylab = "Y", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot width and height", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "wh.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", width = 10, height = 8, output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000)
})

test_that("plot units cm", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "cm.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", width = 15, height = 10, units = "cm", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot dpi 72 produces smaller file", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out_lo <- file.path(dir, "lo.png")
  out_hi <- file.path(dir, "hi.png")
  rush_plot_exec(f, args = list(x = "x", y = "y", dpi = 72, output = out_lo))
  rush_plot_exec(f, args = list(x = "x", y = "y", dpi = 300, output = out_hi))
  expect_lt(file.size(out_lo), file.size(out_hi))
})

test_that("plot --pre transforms data", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:20, y = 1:20), dir, "data.csv")
  out <- file.path(dir, "pre.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", pre = "df <- head(df, 5)", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot --post adds layer", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "post.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", post = "p + ggplot2::theme_minimal()", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot SVG output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "plot.svg")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
  content <- readLines(out, n = 2)
  expect_true(any(grepl("svg|xml", content)))
})

test_that("plot PDF output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "plot.pdf")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("plot from stdin", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "stdin.png")
  csv_data <- c("a,b", "1,2", "3,4", "5,6", "7,8")
  result <- rush_plot_exec(
    file = "-",
    args = list(x = "a", y = "b", output = out),
    stdin_data = csv_data
  )
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot from TSV input", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_tsv(data.frame(a = 1:5, b = 1:5), dir, "data.tsv")
  out <- file.path(dir, "tsv.png")
  result <- rush_plot_exec(f, args = list(x = "a", y = "b", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot from Parquet input", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_parquet(data.frame(x = 1:5, y = 1:5), dir)
  out <- file.path(dir, "pq.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot from JSON input", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_json(data.frame(x = 1:5, y = 1:5), dir)
  out <- file.path(dir, "json.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", input_format = "json", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot from DuckDB input", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_duckdb(list(points = data.frame(x = 1:10, y = rnorm(10))), dir)
  out <- file.path(dir, "duckdb.png")
  result <- rush_plot_exec(f, args = list(x = "x", y = "y", output = out))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot with -l library", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "lib.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", library = "ggplot2", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot with -t tidyverse", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out <- file.path(dir, "tv.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", tidyverse = TRUE, output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

test_that("plot with --seed for reproducibility", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5, y = 1:5), dir, "data.csv")
  out1 <- file.path(dir, "s1.png")
  out2 <- file.path(dir, "s2.png")
  rush_plot_exec(f, args = list(x = "x", y = "y", seed = 42, output = out1))
  rush_plot_exec(f, args = list(x = "x", y = "y", seed = 42, output = out2))
  expect_equal(file.size(out1), file.size(out2))
})

test_that("plot from Excel with --input-sheet", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  sheets <- list(
    Sheet1 = data.frame(a = 1:3, b = 4:6),
    Sheet2 = data.frame(x = 1:5, y = 1:5)
  )
  f <- make_xlsx(sheets, dir)
  out <- file.path(dir, "sheet.png")
  result <- rush_plot_exec(f, args = list(
    x = "x", y = "y", input_sheet = "Sheet2", output = out
  ))
  expect_equal(result$status, 0)
  expect_true(file.exists(out))
})

# Section 28: Edge cases --------------------------------------------------------

test_that("single-row data frame -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1, y = "a"), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(df), 1)
})

test_that("single-column data frame -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(data.frame(x = 1:5), dir)
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = out))
  df <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(ncol(df), 1)
  expect_equal(names(df), "x")
})

test_that("NA values in CSV -> JSON -> CSV round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df_na <- data.frame(a = c(1, NA, 3), b = c("x", NA, "z"), stringsAsFactors = FALSE)
  f <- make_csv(df_na, dir)
  mid <- file.path(dir, "mid.json")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  result <- readr::read_csv(out, show_col_types = FALSE)
  expect_true(is.na(result$a[2]))
  expect_true(is.na(result$b[2]))
})

test_that("columns with dots in TOML output are quoted", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df_dots <- data.frame(`Sepal.Length` = 5.1, `Petal.Width` = 0.2, check.names = FALSE)
  f <- make_csv(df_dots, dir)
  out <- file.path(dir, "out.toml")
  rush_run_exec("df", file = f, args = list(output = out, no_clean_names = TRUE))
  content <- paste(readLines(out), collapse = "\n")
  expect_true(grepl('"Sepal.Length"', content))
  expect_true(grepl('"Petal.Width"', content))
})

test_that("columns with dots in XML output become valid elements", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df_dots <- data.frame(sepal_length = 5.1, petal_width = 0.2)
  f <- make_csv(df_dots, dir)
  out <- file.path(dir, "out.xml")
  rush_run_exec("df", file = f, args = list(output = out))
  doc <- xml2::read_xml(out)
  row <- xml2::xml_children(doc)[[1]]
  col_names <- xml2::xml_name(xml2::xml_children(row))
  expect_true("sepal_length" %in% col_names)
})

test_that("numeric precision survives JSON round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df_prec <- data.frame(x = c(3.14159265358979, 1e-10, 1e10))
  f <- make_csv(df_prec, dir)
  mid <- file.path(dir, "mid.json")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  result <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(result$x[1], 3.14159265358979, tolerance = 1e-3)
  expect_equal(result$x[3], 1e10, tolerance = 1)
})

test_that("wide data frame (50 columns) survives JSON round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  wide <- as.data.frame(matrix(1:100, nrow = 2, ncol = 50))
  f <- make_csv(wide, dir)
  mid <- file.path(dir, "mid.json")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  result <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(ncol(result), 50)
})

test_that("many rows (500) survive Parquet round-trip", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  big <- data.frame(x = seq_len(500), y = runif(500))
  f <- make_csv(big, dir)
  mid <- file.path(dir, "mid.parquet")
  out <- file.path(dir, "out.csv")
  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  result <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(nrow(result), 500)
})

test_that("unicode characters survive CSV -> JSON -> CSV", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  df_uni <- data.frame(name = c("café", "über", "ñoño"), stringsAsFactors = FALSE)
  f <- make_csv(df_uni, dir)
  mid <- file.path(dir, "mid.json")
  out <- file.path(dir, "out.csv")

  rush_run_exec("df", file = f, args = list(output = mid))
  rush_run_exec("df", file = mid, args = list(output = out))
  result <- readr::read_csv(out, show_col_types = FALSE)
  expect_equal(result$name[1], "café")
})

# Section 29: Error cases -------------------------------------------------------

test_that("parquet without -o gives error or no output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "parquet"))
  expect_true(result$status != 0 || nchar(result$stdout) == 0)
})

test_that("arrow without -o gives error or no output", {
  skip_if_no_ir()
  dir <- withr::local_tempdir()
  f <- make_csv(test_df(), dir)
  result <- rush_run_exec("df", file = f, args = list(output_format = "arrow"))
  expect_true(result$status != 0 || nchar(result$stdout) == 0)
})

test_that("invalid expression gives non-zero exit", {
  skip_if_no_ir()
  result <- rush_run_exec("this is not valid R code !!!")
  expect_false(result$status == 0)
})

test_that("missing input file gives error or no meaningful output", {
  skip_if_no_ir()
  result <- rush_run_exec("nrow(df)", file = "/nonexistent/file.csv")
  expect_true(result$status != 0 || nchar(trimws(result$stdout)) == 0 || grepl("Error", result$stderr))
})

# Section 30: --no-ir flag ------------------------------------------------------

test_that("--no-ir still produces correct output", {
  skip_if_no_ir()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "library(rush)",
    'rush("run", "--no-ir", "1 + 1")'
  ), script)
  result <- processx::run("Rscript", script, stdout = "|", stderr = "|", error_on_status = FALSE)
  expect_equal(result$status, 0)
  expect_equal(trimws(result$stdout), "2")
})
