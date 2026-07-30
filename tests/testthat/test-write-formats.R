# Parquet --------------------------------------------------------------------

test_that("writes Parquet when output ends in .parquet", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.parquet"
  )
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
})

test_that("output_format = 'parquet' works with explicit output", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "parquet",
    output = "out.parquet"
  )
  expect_true(any(grepl('output_format = "parquet"', script)))
  expect_true(any(grepl("nanoparquet::write_parquet", script)))
})

# JSON -----------------------------------------------------------------------

test_that("output_format = 'json' emits JSON output", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "json"
  )
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("output_format = 'jsonl' emits JSONL output", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "jsonl"
  )
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("output = 'data.json' infers JSON format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "data.json"
  )
  expect_true(any(grepl('output_format = "json"', script)))
  expect_true(any(grepl("jsonlite::toJSON", script)))
})

test_that("output = 'data.jsonl' infers JSONL format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "data.jsonl"
  )
  expect_true(any(grepl('output_format = "jsonl"', script)))
  expect_true(any(grepl("jsonlite::stream_out", script)))
})

test_that("JSON uses default output_indent of 2", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "json"
  )
  expect_true(any(grepl("output_indent = 2L", script)))
})

test_that("JSON uses custom output_indent", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "json",
    output_indent = 4L
  )
  expect_true(any(grepl("output_indent = 4L", script)))
})

test_that("JSON output_indent = 0 means compact", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "json",
    output_indent = 0L
  )
  expect_true(any(grepl("output_indent = 0L", script)))
})

# Arrow ----------------------------------------------------------------------

test_that("output_format = 'arrow' emits Arrow IPC output", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "arrow",
    output = "out.arrow"
  )
  expect_true(any(grepl('output_format = "arrow"', script)))
  expect_true(any(grepl("arrow::write_ipc_file", script)))
  expect_true(any(grepl("^#\\|   - arrow$", script)))
})

test_that("output = 'out.feather' infers Arrow format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.feather"
  )
  expect_true(any(grepl('output_format = "arrow"', script)))
  expect_true(any(grepl("arrow::write_ipc_file", script)))
})

# Excel ----------------------------------------------------------------------

test_that("output_format = 'xlsx' emits Excel output", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "xlsx",
    output = "out.xlsx"
  )
  expect_true(any(grepl('output_format = "xlsx"', script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
  expect_true(any(grepl("^#\\|   - writexl$", script)))
})

test_that("output = 'out.xlsx' infers Excel format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.xlsx"
  )
  expect_true(any(grepl('output_format = "xlsx"', script)))
  expect_true(any(grepl("writexl::write_xlsx", script)))
})

test_that("Excel uses custom output_sheet", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.xlsx",
    output_sheet = "Results"
  )
  expect_true(any(grepl('output_sheet = "Results"', script)))
})

# Haven: SPSS ----------------------------------------------------------------

test_that("output_format = 'sav' emits haven::write_sav", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "sav",
    output = "out.sav"
  )
  expect_true(any(grepl('output_format = "sav"', script)))
  expect_true(any(grepl("haven::write_sav", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

test_that("output = 'out.zsav' emits haven::write_sav with compress", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.zsav"
  )
  expect_true(any(grepl('output_format = "zsav"', script)))
  expect_true(any(grepl('compress = "zsav"', script)))
})

# Haven: Stata ----------------------------------------------------------------

test_that("output_format = 'dta' emits haven::write_dta", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "dta",
    output = "out.dta"
  )
  expect_true(any(grepl('output_format = "dta"', script)))
  expect_true(any(grepl("haven::write_dta", script)))
})

# Haven: SAS ------------------------------------------------------------------

test_that("output_format = 'sas7bdat' emits haven::write_sas", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "sas7bdat",
    output = "out.sas7bdat"
  )
  expect_true(any(grepl('output_format = "sas7bdat"', script)))
  expect_true(any(grepl("haven::write_sas", script)))
})

test_that("output_format = 'xpt' emits haven::write_xpt", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "xpt",
    output = "out.xpt"
  )
  expect_true(any(grepl('output_format = "xpt"', script)))
  expect_true(any(grepl("haven::write_xpt", script)))
})

# DuckDB ---------------------------------------------------------------------

test_that("output_format = 'duckdb' emits DuckDB write", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "duckdb",
    output = "out.duckdb"
  )
  expect_true(any(grepl('output_format = "duckdb"', script)))
  expect_true(any(grepl("duckdb::duckdb\\(\\)", script)))
  expect_true(any(grepl("dbWriteTable", script)))
  expect_true(any(grepl("^#\\|   - duckdb$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

test_that(".ddb extension resolves to duckdb format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output = "out.ddb"
  )
  expect_true(any(grepl('output_format = "duckdb"', script)))
})

# SQLite ---------------------------------------------------------------------

test_that("output_format = 'sqlite' emits SQLite write", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "sqlite",
    output = "out.sqlite"
  )
  expect_true(any(grepl('output_format = "sqlite"', script)))
  expect_true(any(grepl("RSQLite::SQLite", script)))
  expect_true(any(grepl("dbWriteTable", script)))
  expect_true(any(grepl("^#\\|   - RSQLite$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

# RDS -------------------------------------------------------------------------

test_that("output_format = 'rds' emits write_rds", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "rds",
    output = "out.rds"
  )
  expect_true(any(grepl('output_format = "rds"', script)))
  expect_true(any(grepl("write_rds", script)))
})

# ODS -------------------------------------------------------------------------

test_that("output_format = 'ods' emits readODS::write_ods", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "ods",
    output = "out.ods"
  )
  expect_true(any(grepl('output_format = "ods"', script)))
  expect_true(any(grepl("readODS::write_ods", script)))
  expect_true(any(grepl("^#\\|   - readODS$", script)))
})

# FASTA -----------------------------------------------------------------------

test_that("output_format = 'fasta' emits microseq::writeFasta", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "fasta",
    output = "out.fasta"
  )
  expect_true(any(grepl('output_format = "fasta"', script)))
  expect_true(any(grepl("microseq::writeFasta", script)))
  expect_true(any(grepl("^#\\|   - microseq$", script)))
})

# FASTQ -----------------------------------------------------------------------

test_that("output_format = 'fastq' emits microseq::writeFastq", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "fastq",
    output = "out.fastq"
  )
  expect_true(any(grepl('output_format = "fastq"', script)))
  expect_true(any(grepl("microseq::writeFastq", script)))
})

# YAML ------------------------------------------------------------------------

test_that("output_format = 'yaml' emits yaml::as.yaml", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "yaml",
    output = "out.yaml"
  )
  expect_true(any(grepl('output_format = "yaml"', script)))
  expect_true(any(grepl("yaml::as.yaml", script)))
  expect_true(any(grepl("^#\\|   - yaml$", script)))
})

test_that("YAML uses default output_indent of 2", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "yaml",
    output = "out.yaml"
  )
  expect_true(any(grepl("output_indent = 2L", script)))
})

test_that("YAML uses custom output_indent", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "yaml",
    output = "out.yaml",
    output_indent = 4L
  )
  expect_true(any(grepl("output_indent = 4L", script)))
})

# TOML ------------------------------------------------------------------------

test_that("output_format = 'toml' emits manual TOML writer", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "toml",
    output = "out.toml"
  )
  expect_true(any(grepl('output_format = "toml"', script)))
  expect_false(any(grepl("RcppTOML", script)))
})

test_that("TOML uses default record name 'record'", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "toml",
    output = "out.toml"
  )
  expect_true(any(grepl('output_record = "record"', script)))
})

test_that("TOML uses custom output_record", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "toml",
    output = "out.toml",
    output_record = "item"
  )
  expect_true(any(grepl('output_record = "item"', script)))
})

# XML -------------------------------------------------------------------------

test_that("output_format = 'xml' emits xml2::write_xml", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "xml",
    output = "out.xml"
  )
  expect_true(any(grepl('output_format = "xml"', script)))
  expect_true(any(grepl("xml2::write_xml", script)))
  expect_true(any(grepl("^#\\|   - xml2$", script)))
})

test_that("XML uses default root 'root' and record 'record'", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "xml",
    output = "out.xml"
  )
  expect_true(any(grepl('output_root = "root"', script)))
  expect_true(any(grepl('output_record = "record"', script)))
})

test_that("XML uses custom output_root and output_record", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.csv",
    output_format = "xml",
    output = "out.xml",
    output_root = "plants",
    output_record = "observation"
  )
  expect_true(any(grepl('output_root = "plants"', script)))
  expect_true(any(grepl('output_record = "observation"', script)))
})

# Nesting behavior ------------------------------------------------------------

test_that("json is flattened when output is a flat format", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.json",
    output = "out.parquet"
  )
  expect_true(any(grepl("jsonlite::flatten", script)))
})

test_that("json is NOT flattened when output is json", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.json",
    output_format = "json"
  )
  expect_false(any(grepl("jsonlite::flatten", script)))
})

test_that("json is NOT flattened when output is yaml", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.json",
    output = "out.yaml"
  )
  expect_false(any(grepl("jsonlite::flatten", script)))
})

test_that("json is NOT flattened when output is rds", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.json",
    output = "out.rds"
  )
  expect_false(any(grepl("jsonlite::flatten", script)))
})

test_that("jsonl is flattened when output is xlsx", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.jsonl",
    output = "out.xlsx"
  )
  expect_true(any(grepl("jsonlite::flatten", script)))
})

test_that("jsonl is NOT flattened when output is toml", {
  script <- capture_script(
    rush_run,
    expr = "df",
    file = "data.jsonl",
    output = "out.toml"
  )
  expect_false(any(grepl("jsonlite::flatten", script)))
})
