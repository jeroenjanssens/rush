test_that("rush_convert without input errors", {
  expect_error(rush_convert(character()), "No input file to convert")
})

test_that("rush_convert without output or format errors", {
  expect_error(rush_convert("data.csv"), "No output format specified")
})

test_that("rush_convert csv to parquet", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.parquet")
  expect_true(any(grepl("read_delim\\(\"data.csv\"", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
})

test_that("rush_convert json to csv", {
  script <- capture_script(rush_convert, file = "data.json",
                           output = "out.csv")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl("readr::write_delim", script)))
})

test_that("rush_convert to stdout with output_format", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output_format = "json")
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("result <- df", script)))
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
})

test_that("rush_convert with multiple input files requires a template", {
  expect_error(
    rush_convert(c("a.csv", "b.csv"), output = "out.parquet", dry_run = TRUE),
    "template"
  )
})

test_that("rush_convert with output template emits result <- dfs", {
  script <- capture_script(rush_convert, file = c("a.csv", "b.csv"),
                           output = "%(file_name)s.parquet")
  expect_true(any(grepl("dfs <- list", script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("output_template", script)))
})

test_that("rush_convert csv to sav", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.sav")
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("haven::write_sav", script)))
})

test_that("rush_convert sav to csv", {
  script <- capture_script(rush_convert, file = "data.sav",
                           output = "out.csv")
  expect_true(any(grepl("haven::read_sav", script)))
  expect_true(any(grepl("write_delim", script)))
})

test_that("rush_convert csv to dta", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.dta")
  expect_true(any(grepl("haven::write_dta", script)))
})

test_that("rush_convert csv to sqlite", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.sqlite")
  expect_true(any(grepl("RSQLite::SQLite", script)))
  expect_true(any(grepl("dbWriteTable", script)))
})

test_that("rush_convert csv to rds", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.rds")
  expect_true(any(grepl("saveRDS", script)))
})

test_that("rush_convert csv to ods", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.ods")
  expect_true(any(grepl("readODS::write_ods", script)))
})

test_that("rush_convert csv to fasta", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.fasta")
  expect_true(any(grepl("microseq::writeFasta", script)))
})

test_that("rush_convert csv to duckdb", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output = "out.duckdb")
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("duckdb::duckdb\\(\\)", script)))
  expect_true(any(grepl("dbWriteTable", script)))
})

test_that("rush_convert multiple csvs to duckdb", {
  script <- capture_script(rush_convert, file = c("a.csv", "b.csv"),
                           output = "combined.duckdb")
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("for \\(.tbl_name in names\\(result\\)\\)", script)))
  expect_true(any(grepl("dbWriteTable\\(.con, .tbl_name", script)))
})

test_that("rush_convert multiple csvs to sqlite", {
  script <- capture_script(rush_convert, file = c("a.csv", "b.csv"),
                           output = "combined.sqlite")
  expect_true(any(grepl('output_format = "sqlite"', script)))
  expect_true(any(grepl("result <- dfs", script)))
  expect_true(any(grepl("for \\(.tbl_name in names\\(result\\)\\)", script)))
})

test_that("rush_convert respects input_format override", {
  script <- capture_script(rush_convert, file = "data.txt",
                           input_format = "json", output = "out.csv")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
})

test_that("rush_convert respects output_format override", {
  script <- capture_script(rush_convert, file = "data.csv",
                           output_format = "jsonl", output = "out.txt")
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
})

test_that("rush_convert applies head to limit rows", {
  script <- capture_script(rush_convert, file = "data.parquet",
                           output = "out.csv", head = 10)
  expect_true(any(grepl("head = 10L", script)))
})

test_that("rush_convert parquet to xlsx", {
  script <- capture_script(rush_convert, file = "data.parquet",
                           output = "out.xlsx")
  expect_true(any(grepl("nanoparquet::read_parquet", script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
})

test_that("rush_convert json to yaml preserves nesting", {
  script <- capture_script(rush_convert, file = "data.json",
                           output = "out.yaml")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_false(any(grepl("jsonlite::flatten", script)))
  expect_true(any(grepl("yaml::as.yaml", script)))
})
