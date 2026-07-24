# The --dry-run flag prints the generated script instead of executing it,
# which makes it a convenient way to test script generation end to end.
dry_run <- function(...) {
  utils::capture.output(rush(...))
}

test_that("run generates a shebang and reads the file", {
  script <- dry_run("run", "-n", "head(df)", "data.csv")
  expect_equal(script[[1]], "#!/usr/bin/env Rscript")
  expect_true(any(grepl("read_delim", script)))
  expect_true(any(grepl("janitor::clean_names", script)))
  expect_true(any(grepl("^head\\(df\\)$", script)))
})

test_that("--no-clean-names omits the janitor call", {
  script <- dry_run("run", "-n", "-C", "head(df)", "data.csv")
  expect_false(any(grepl("clean_names", script)))
})

test_that("--no-header reads without column names", {
  script <- dry_run("run", "-n", "-H", "head(df)", "data.csv")
  expect_true(any(grepl("col_names = FALSE", script)))
})

test_that("--seed emits set.seed before other code", {
  script <- dry_run("run", "-n", "--seed", "7", "1 + 1", "data.csv")
  expect_true(any(grepl("set.seed\\(7\\)", script)))
  expect_lt(grep("set.seed", script), grep("read_delim", script))
})

test_that("libraries are loaded", {
  script <- dry_run("run", "-n", "-l", "stringr", "head(df)", "data.csv")
  expect_true(any(grepl("^library\\(stringr\\)$", script)))
})

test_that("--tidyverse loads tidyverse and glue", {
  script <- dry_run("run", "-n", "-t", "head(df)", "data.csv")
  expect_true(any(grepl("^library\\(tidyverse\\)$", script)))
  expect_true(any(grepl("^library\\(glue\\)$", script)))
})

test_that("stdin is read from a binary connection", {
  script <- dry_run("run", "-n", "head(df)", "-")
  expect_true(any(grepl("stdin", script)))
})

test_that("install generates an install.packages call", {
  script <- dry_run("install", "-n", "cli")
  expect_true(any(grepl("install.packages\\(\"cli\"\\)", script)))
})

test_that("plot generates a qplot call with aesthetics", {
  script <- dry_run("plot", "-n", "-x", "wt", "-y", "mpg", "mtcars.csv")
  expect_true(any(grepl("^library\\(ggplot2\\)$", script)))
  qplot_line <- script[grepl("qplot\\(", script)]
  expect_length(qplot_line, 1)
  expect_match(qplot_line, "x = wt")
  expect_match(qplot_line, "y = mpg")
  expect_match(qplot_line, "data = df")
})

test_that("plot --title becomes the main argument", {
  script <- dry_run("plot", "-n", "-x", "wt", "--title", "Cars", "mtcars.csv")
  expect_true(any(grepl('main = "Cars"', script)))
})

test_that("plot --pre and --post wrap the plot call", {
  script <- dry_run("plot", "-n", "-x", "wt",
                    "--pre", "df <- head(df)",
                    "--post", "p + theme_bw()", "mtcars.csv")
  expect_true(any(grepl("p <- qplot", script)))
  expect_true(any(grepl("theme_bw", script)))
  # pre runs before the plot, post after
  expect_lt(grep("head\\(df\\)", script)[[1]], grep("qplot", script))
  expect_gt(grep("theme_bw", script), grep("qplot", script))
})
