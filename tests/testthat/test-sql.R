test_that("rush_sql without a query errors gracefully", {
  expect_error(rush_sql(), "No query to run")
})

test_that("rush_sql generates a DuckDB connection", {
  script <- capture_script(rush_sql, query = "SELECT 1 AS x")
  expect_true(any(grepl("con <- DBI::dbConnect\\(duckdb::duckdb\\(\\)\\)", script)))
  expect_true(any(grepl("on\\.exit\\(DBI::dbDisconnect", script)))
})

test_that("rush_sql executes the query", {
  script <- capture_script(rush_sql, query = "SELECT 1 AS x")
  expect_true(any(grepl('result <- DBI::dbGetQuery\\(con, "SELECT 1 AS x"\\)', script)))
})

test_that("rush_sql registers CSV files as relations", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM a",
                           file = "a.csv")
  expect_true(any(grepl("CREATE VIEW.*a.*FROM read_csv_auto", script)))
})

test_that("rush_sql registers Parquet files", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM b",
                           file = "b.parquet")
  expect_true(any(grepl("CREATE VIEW.*b.*FROM read_parquet", script)))
})

test_that("rush_sql attaches DuckDB databases", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM w.t1",
                           file = "w.duckdb")
  expect_true(any(grepl("ATTACH .* AS ..w.. .READ_ONLY.", script)))
})

test_that("rush_sql registers JSON files with json extension", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM data",
                           file = "data.json")
  expect_true(any(grepl("INSTALL json; LOAD json", script)))
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("rush_sql registers JSONL files", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM events",
                           file = "events.jsonl")
  expect_true(any(grepl("INSTALL json; LOAD json", script)))
  expect_true(any(grepl("read_json_auto", script)))
})

test_that("rush_sql handles stdin", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM stdin",
                           file = "-")
  expect_true(any(grepl("\\.stdin_tmp <- tempfile", script)))
  expect_true(any(grepl(
    'CREATE VIEW .{1,2}stdin.{1,2} AS SELECT .* FROM read_csv_auto',
    script
  )))
})

test_that("rush_sql handles multiple files", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM a JOIN b USING (x)",
                           file = c("a.csv", "b.parquet"))
  expect_true(any(grepl("CREATE VIEW.*a.*FROM read_csv_auto", script)))
  expect_true(any(grepl("CREATE VIEW.*b.*FROM read_parquet", script)))
})

test_that("rush_sql without json files does not emit INSTALL json", {
  script <- capture_script(rush_sql,
                           query = "SELECT * FROM data",
                           file = "data.csv")
  expect_false(any(grepl("INSTALL json", script)))
})

test_that("rush_sql respects output_format", {
  script <- capture_script(rush_sql,
                           query = "SELECT 1",
                           output_format = "json")
  expect_true(any(grepl('output_format = "json"', script)))
})

test_that("rush_sql respects head", {
  script <- capture_script(rush_sql, query = "SELECT 1", head = 5)
  expect_true(any(grepl("head = 5L", script)))
})
