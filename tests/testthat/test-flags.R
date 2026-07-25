test_that("commands are recognized", {
  expect_equal(parse_arguments("run", "1 + 1")$command, "run")
  expect_equal(parse_arguments("plot", "mtcars.csv")$command, "plot")
})

test_that("logical flags default to FALSE and toggle on", {
  expect_false(parse_arguments("run", "1")$verbose)
  expect_true(parse_arguments("run", "-v", "1")$verbose)
  expect_true(parse_arguments("run", "-n", "1")$dry_run)
})

test_that("seed is coerced to integer", {
  seed <- parse_arguments("run", "--seed", "42", "1")$seed
  expect_type(seed, "integer")
  expect_equal(seed, 42L)
})

test_that("delimiter defaults to comma and is overridable", {
  expect_equal(parse_arguments("run", "1")$delimiter, ",")
  expect_equal(parse_arguments("run", "-d", "\t", "1")$delimiter, "\t")
})

test_that("expressions are parsed into a list of calls", {
  exprs <- parse_arguments("run", "nrow(df)")$expression
  expect_type(exprs, "list")
  expect_equal(exprs[[1]], quote(nrow(df)))
})

test_that("comma-separated libraries are parsed into symbols", {
  libs <- parse_arguments("run", "-l", "stringr,dplyr", "1")$library
  expect_equal(libs, list(rlang::sym("stringr"), rlang::sym("dplyr")))
})

test_that("plot aesthetics are parsed as symbols", {
  args <- parse_arguments("plot", "-x", "wt", "-y", "mpg", "mtcars.csv")
  expect_equal(args$x, rlang::sym("wt"))
  expect_equal(args$y, rlang::sym("mpg"))
})

test_that("--title parses to the title flag", {
  args <- parse_arguments("plot", "--title", "Cars", "mtcars.csv")
  expect_equal(args$title, "Cars")
})

test_that("positional file is captured", {
  expect_equal(parse_arguments("run", "1", "data.csv")$file, "data.csv")
})

test_that("convert_flag leaves logicals and NULL untouched", {
  expect_null(convert_flag(NULL, "seed"))
  expect_true(convert_flag(TRUE, "verbose"))
})
