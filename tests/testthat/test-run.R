test_that("rush_run without an expression or file errors gracefully", {
  expect_error(rush_run(), "No expression to run")
})

test_that("rush_run generates a script with shebang and frontmatter", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv")
  expect_equal(script[[1]], "#!/usr/bin/env -S ir run")
  expect_true(any(grepl("^#\\| packages:$", script)))
})

test_that("rush_run evaluates a simple expression", {
  script <- capture_script(rush_run, expr = "1 + 1")
  expect_true(any(grepl("result <- 1 \\+ 1", script)))
})

test_that("rush_run reads a file into df and dfs", {
  script <- capture_script(rush_run, expr = "head(df)", file = "data.csv")
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl('dfs\\[\\["data"\\]\\] <-', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("rush_run reads multiple files into a dfs list", {
  script <- capture_script(
    rush_run,
    expr = "nrow(dfs$a)",
    file = c("a.csv", "b.csv")
  )
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl(
    'dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"',
    script
  )))
})

test_that("rush_run with file but no expr passes through df", {
  script <- capture_script(rush_run, file = "data.csv")
  expect_true(any(grepl("result <- df", script)))
})

test_that("rush_run reads from stdin with '-'", {
  script <- capture_script(rush_run, expr = "head(df)", file = "-")
  expect_true(any(grepl("stdin", script)))
})

test_that("seed emits set.seed before other code", {
  script <- capture_script(
    rush_run,
    expr = "1 + 1",
    file = "data.csv",
    seed = 7
  )
  expect_true(any(grepl("set.seed\\(7\\)", script)))
  expect_lt(grep("set.seed", script), grep("read_delim", script))
})

test_that("library loads requested packages", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    library = "stringr"
  )
  expect_true(any(grepl("^library\\(stringr\\)$", script)))
})

test_that("library accepts a vector of packages", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    library = c("stringr", "dplyr")
  )
  expect_true(any(grepl("^library\\(stringr\\)$", script)))
  expect_true(any(grepl("^library\\(dplyr\\)$", script)))
})

test_that("tidyverse = TRUE loads tidyverse and glue", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    tidyverse = TRUE
  )
  expect_true(any(grepl("^library\\(tidyverse\\)$", script)))
  expect_true(any(grepl("^library\\(glue\\)$", script)))
})

test_that("header = FALSE reads without column names", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    header = FALSE
  )
  expect_true(any(grepl("col_names = FALSE", script)))
})

test_that("header = FALSE suppresses output header", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    header = FALSE
  )
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("input_header = FALSE only affects reading", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    input_header = FALSE
  )
  expect_true(any(grepl("col_names = FALSE", script)))
  expect_true(any(grepl("output_header = TRUE", script)))
})

test_that("output_header = FALSE only affects writing", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    output_header = FALSE
  )
  expect_true(any(grepl("col_names = TRUE", script)))
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("names provides column names and implies no input header", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    names = "a,b,c"
  )
  expect_true(any(grepl('col_names = c\\("a", "b", "c"\\)', script)))
  expect_true(any(grepl("output_header = TRUE", script)))
})

test_that("names with output_header = FALSE suppresses output header", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    names = "x,y",
    output_header = FALSE
  )
  expect_true(any(grepl('col_names = c\\("x", "y"\\)', script)))
  expect_true(any(grepl("output_header = FALSE", script)))
})

test_that("clean_names = FALSE omits the janitor call", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    clean_names = FALSE
  )
  expect_false(any(grepl("clean_names", script)))
})

test_that("head limits output rows", {
  script <- capture_script(rush_run, expr = "df", file = "data.csv", head = 3)
  expect_true(any(grepl("head = 3L", script)))
})

test_that("expr accepts a language object", {
  script <- capture_script(rush_run, expr = quote(head(df)), file = "data.csv")
  expect_true(any(grepl("result <- head\\(df\\)", script)))
})

test_that("expr accepts a list of expressions", {
  script <- capture_script(
    rush_run,
    expr = rlang::exprs(x <- 1, x + 1),
    file = "data.csv"
  )
  expect_true(any(grepl("x <- 1", script)))
  expect_true(any(grepl("result <- x \\+ 1", script)))
})

test_that("no_ir flag is propagated (script is still identical)", {
  script <- capture_script(rush_run, expr = "1 + 1", no_ir = TRUE)
  expect_equal(script[[1]], "#!/usr/bin/env -S ir run")
  expect_true(any(grepl("^#\\| packages:$", script)))
})

test_that("delimiter sets both input and output delimiters", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    delimiter = "\t"
  )
  expect_true(any(grepl('delim = "\\\\t"', script)))
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("output_delimiter overrides only the output delimiter", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    output_delimiter = "\t"
  )
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = ","', read_line)))
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("input_delimiter overrides only the input delimiter", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    input_delimiter = "\t"
  )
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = "\\\\t"', read_line)))
  expect_true(any(grepl('delimiter = ","', script)))
})

test_that("input_format = 'tsv' implies tab as input delimiter", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.tsv",
    input_format = "tsv"
  )
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = "\\\\t"', read_line)))
})

test_that(".tsv extension auto-detects tab as input delimiter", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "cities.tsv"
  )
  read_line <- script[grepl("read_delim", script)]
  expect_true(any(grepl('delim = "\\\\t"', read_line)))
})

test_that("output_format = 'tsv' implies tab as output delimiter", {
  script <- capture_script(
    rush_run,
    expr = "head(df)",
    file = "data.csv",
    output_format = "tsv"
  )
  expect_true(any(grepl('delimiter = "\\\\t"', script)))
})

test_that("build_flags errors on invalid expr type", {
  expect_error(rush_run(expr = 123), "must be a character string")
})

test_that("build_flags errors on invalid query type", {
  expect_error(
    rush:::build_flags("sql", query = 123),
    "must be a single character"
  )
})

test_that("build_flags accepts language object for expr", {
  script <- capture_script(rush_run, expr = quote(1 + 1))
  expect_true(any(grepl("result <- 1 \\+ 1", script)))
})

test_that("build_flags accepts pre/post as language objects", {
  script <- capture_script(
    rush:::rush_plot,
    file = "data.csv",
    x = "x",
    y = "y",
    pre = list(quote(df <- head(df)))
  )
  expect_true(any(grepl("df <- head\\(df\\)", script)))
})

test_that("build_flags accepts facets as language object", {
  script <- capture_script(
    rush:::rush_plot,
    file = "data.csv",
    x = "x",
    y = "y",
    facets = quote(~species)
  )
  expect_true(any(grepl("species", script)))
})

test_that("digit-prefixed file names generate parseable code", {
  script <- capture_script(rush_run, expr = "head(df)", file = "2024.csv")
  expect_silent(parse(text = script))
})

test_that("multi-file digit-prefixed names use valid dfs indexing", {
  script <- capture_script(
    rush_run,
    expr = "nrow(dfs)",
    file = c("2024.csv", "2025.csv")
  )
  expect_true(any(grepl('dfs\\[\\["x2024"\\]\\]', script)))
  expect_true(any(grepl('dfs\\[\\["x2025"\\]\\]', script)))
  expect_silent(parse(text = script))
})
