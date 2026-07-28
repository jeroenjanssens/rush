# Parquet --------------------------------------------------------------------

test_that("reads a Parquet file with nanoparquet", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.parquet")
  expect_true(any(grepl(
    "nanoparquet::read_parquet\\(\"data.parquet\"",
    script
  )))
  expect_true(any(grepl("^#\\|   - nanoparquet$", script)))
  expect_false(any(grepl("read_delim", script)))
})

# JSON -----------------------------------------------------------------------

test_that("reads a JSON file with jsonlite", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.json")
  expect_true(any(grepl("jsonlite::fromJSON\\(\"data.json\"\\)", script)))
  expect_true(any(grepl("jsonlite::flatten", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("reads a JSONL file with jsonlite stream_in", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.jsonl")
  expect_true(any(grepl(
    "jsonlite::stream_in\\(file\\(\"data.jsonl\"\\)",
    script
  )))
  expect_true(any(grepl("jsonlite::flatten", script)))
  expect_true(any(grepl("^#\\|   - jsonlite$", script)))
})

test_that("reads an .ndjson file as JSONL", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.ndjson")
  expect_true(any(grepl(
    "jsonlite::stream_in\\(file\\(\"data.ndjson\"\\)",
    script
  )))
})

test_that("JSON input does not flatten when output is JSON", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.json",
                           output_format = "json")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_false(any(grepl("jsonlite::flatten", script)))
})

test_that("input_format = 'json' forces JSON reading", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv",
                           input_format = "json")
  expect_true(any(grepl("jsonlite::fromJSON", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("input_format = 'jsonl' forces JSONL reading", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv",
                           input_format = "jsonl")
  expect_true(any(grepl("jsonlite::stream_in", script)))
})

test_that("JSON stdin uses file('stdin') connection", {
  script <- capture_script(rush_run, expr = "head(df)", file = "-",
                           input_format = "json")
  expect_true(any(grepl('fromJSON\\(file\\("stdin"\\)\\)', script)))
})

test_that("JSONL stdin uses file('stdin') connection", {
  script <- capture_script(rush_run, expr = "head(df)", file = "-",
                           input_format = "jsonl")
  expect_true(any(grepl('stream_in\\(file\\("stdin"\\)', script)))
})

# Excel ----------------------------------------------------------------------

test_that("reads an Excel file with readxl", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xlsx")
  expect_true(any(grepl("readxl::read_excel\\(\"data.xlsx\"\\)", script)))
  expect_true(any(grepl("^#\\|   - readxl$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("reads .xls files with readxl", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xls")
  expect_true(any(grepl("readxl::read_excel\\(\"data.xls\"\\)", script)))
  expect_true(any(grepl("^#\\|   - readxl$", script)))
})

test_that("sheet selects a specific Excel sheet", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xlsx",
                           sheet = "Sales")
  expect_true(any(grepl(
    'readxl::read_excel\\("data.xlsx", sheet = "Sales"\\)',
    script
  )))
})

test_that("sheet with numeric index works", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xlsx",
                           sheet = 2)
  expect_true(any(grepl(
    'readxl::read_excel\\("data.xlsx", sheet = 2\\)',
    script
  )))
})

test_that("input_format = 'xlsx' forces Excel reading", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv",
                           input_format = "xlsx")
  expect_true(any(grepl("readxl::read_excel", script)))
  expect_false(any(grepl("read_delim", script)))
})

# Arrow ----------------------------------------------------------------------

test_that("reads Arrow IPC file with arrow package", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.arrow")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.arrow\"\\)", script)))
  expect_true(any(grepl("^#\\|   - arrow$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("reads .feather files as Arrow IPC", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.feather")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.feather\"\\)", script)))
})

test_that("reads .ipc files as Arrow IPC", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.ipc")
  expect_true(any(grepl("arrow::read_ipc_file\\(\"data.ipc\"\\)", script)))
})

test_that("input_format = 'arrow' forces Arrow reading", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv",
                           input_format = "arrow")
  expect_true(any(grepl("arrow::read_ipc_file", script)))
  expect_false(any(grepl("read_delim", script)))
})

# Haven: SPSS ----------------------------------------------------------------

test_that("reads a .sav file with haven", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.sav")
  expect_true(any(grepl("haven::read_sav\\(\"data.sav\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("reads a .zsav file with haven::read_sav", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.zsav")
  expect_true(any(grepl("haven::read_sav\\(\"data.zsav\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

test_that("reads a .por file with haven", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.por")
  expect_true(any(grepl("haven::read_por\\(\"data.por\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

test_that("input_format = 'sav' overrides extension", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.txt",
                           input_format = "sav")
  expect_true(any(grepl("haven::read_sav\\(\"data.txt\"\\)", script)))
})

test_that("input_format = 'zsav' normalizes to sav for reading", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.txt",
                           input_format = "zsav")
  expect_true(any(grepl("haven::read_sav\\(\"data.txt\"\\)", script)))
})

# Haven: Stata ----------------------------------------------------------------

test_that("reads a .dta file with haven", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.dta")
  expect_true(any(grepl("haven::read_dta\\(\"data.dta\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

# Haven: SAS ------------------------------------------------------------------

test_that("reads a .sas7bdat file with haven", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.sas7bdat")
  expect_true(any(grepl("haven::read_sas\\(\"data.sas7bdat\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

test_that("reads a .xpt file with haven", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xpt")
  expect_true(any(grepl("haven::read_xpt\\(\"data.xpt\"\\)", script)))
  expect_true(any(grepl("^#\\|   - haven$", script)))
})

# DuckDB ---------------------------------------------------------------------

test_that("reads a DuckDB database into a dfs list", {
  script <- capture_script(rush_run, expr = "nrow(df)", file = "mydb.duckdb")
  expect_true(any(grepl(
    "dbConnect\\(duckdb::duckdb\\(\\), dbdir = \"mydb.duckdb\", read_only = TRUE\\)",
    script
  )))
  expect_true(any(grepl("dbListTables", script)))
  expect_true(any(grepl("dbReadTable", script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\] <- list\\(\\)', script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\]\\[\\[.t\\]\\]', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
  expect_true(any(grepl("^#\\|   - duckdb$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

# SQLite ---------------------------------------------------------------------

test_that("reads a SQLite database into a nested dfs entry", {
  script <- capture_script(rush_run, expr = "nrow(df)", file = "mydb.sqlite")
  expect_true(any(grepl("DBI::dbConnect\\(RSQLite::SQLite\\(\\)", script)))
  expect_true(any(grepl("dbListTables", script)))
  expect_true(any(grepl("dbReadTable", script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\] <- list\\(\\)', script)))
  expect_true(any(grepl('dfs\\[\\["mydb"\\]\\]\\[\\[.t\\]\\]', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
  expect_true(any(grepl("^#\\|   - RSQLite$", script)))
  expect_true(any(grepl("^#\\|   - DBI$", script)))
})

test_that("reads a .db file as SQLite", {
  script <- capture_script(rush_run, expr = "nrow(df)", file = "mydb.db")
  expect_true(any(grepl("RSQLite::SQLite\\(\\)", script)))
})

# FWF (read-only) -------------------------------------------------------------

test_that("reads a .fwf file with readr::read_fwf", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.fwf")
  expect_true(any(grepl("readr::read_fwf", script)))
  expect_true(any(grepl("readr::fwf_empty", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("input_format = 'fwf' overrides extension", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.txt",
                           input_format = "fwf")
  expect_true(any(grepl("readr::read_fwf\\(\"data.txt\"", script)))
})

# RDS -------------------------------------------------------------------------

test_that("reads a .rds file with readRDS", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.rds")
  expect_true(any(grepl("readRDS\\(\"data.rds\"\\)", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("rds reader does not add extra packages", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.rds",
                           clean_names = FALSE)
  expect_false(any(grepl("^#\\|   - haven$", script)))
  expect_false(any(grepl("^#\\|   - readODS$", script)))
})

# ODS -------------------------------------------------------------------------

test_that("reads a .ods file with readODS", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.ods")
  expect_true(any(grepl("readODS::read_ods\\(\"data.ods\"\\)", script)))
  expect_true(any(grepl("^#\\|   - readODS$", script)))
})

# FASTA -----------------------------------------------------------------------

test_that("reads a .fasta file with microseq", {
  script <- capture_script(rush_run, expr = "head(df)", file = "seqs.fasta")
  expect_true(any(grepl("microseq::readFasta\\(\"seqs.fasta\"\\)", script)))
  expect_true(any(grepl("^#\\|   - microseq$", script)))
})

test_that("reads .fa and .fna as FASTA", {
  script_fa <- capture_script(rush_run, expr = "head(df)", file = "seqs.fa")
  script_fna <- capture_script(rush_run, expr = "head(df)", file = "seqs.fna")
  expect_true(any(grepl("microseq::readFasta", script_fa)))
  expect_true(any(grepl("microseq::readFasta", script_fna)))
})

# FASTQ -----------------------------------------------------------------------

test_that("reads a .fastq file with microseq", {
  script <- capture_script(rush_run, expr = "head(df)", file = "reads.fastq")
  expect_true(any(grepl("microseq::readFastq\\(\"reads.fastq\"\\)", script)))
  expect_true(any(grepl("^#\\|   - microseq$", script)))
})

test_that("reads .fq as FASTQ", {
  script <- capture_script(rush_run, expr = "head(df)", file = "reads.fq")
  expect_true(any(grepl("microseq::readFastq", script)))
})

# YAML ------------------------------------------------------------------------

test_that("reads a .yaml file with yaml", {
  script <- capture_script(rush_run, expr = "head(df)", file = "config.yaml")
  expect_true(any(grepl("yaml::read_yaml\\(\"config.yaml\"\\)", script)))
  expect_true(any(grepl("^#\\|   - yaml$", script)))
  expect_false(any(grepl("read_delim", script)))
})

test_that("reads a .yml file with yaml", {
  script <- capture_script(rush_run, expr = "head(df)", file = "config.yml")
  expect_true(any(grepl("yaml::read_yaml\\(\"config.yml\"\\)", script)))
})

# TOML ------------------------------------------------------------------------

test_that("reads a .toml file with RcppTOML", {
  script <- capture_script(rush_run, expr = "head(df)", file = "config.toml")
  expect_true(any(grepl("RcppTOML::parseTOML\\(\"config.toml\"\\)", script)))
  expect_true(any(grepl("^#\\|   - RcppTOML$", script)))
  expect_false(any(grepl("read_delim", script)))
})

# XML -------------------------------------------------------------------------

test_that("reads a .xml file with xml2", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.xml")
  expect_true(any(grepl("xml2::read_xml\\(\"data.xml\"\\)", script)))
  expect_true(any(grepl("^#\\|   - xml2$", script)))
  expect_false(any(grepl("read_delim", script)))
})
