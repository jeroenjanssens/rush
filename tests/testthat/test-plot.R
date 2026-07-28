test_that("rush_plot reads a single file into df", {
  script <- capture_script(rush_plot, file = "a.csv", x = "wt")
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("rush_plot defaults to reading df from stdin", {
  script <- capture_script(rush_plot, x = "wt")
  expect_true(any(grepl('dfs\\[\\["stdin"\\]\\] <- .*stdin', script)))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("rush_plot reads multiple files into a dfs list", {
  script <- capture_script(
    rush_plot,
    file = c("a.csv", "b.csv"),
    x = "wt",
    pre = "df <- dplyr::bind_rows(dfs)"
  )
  expect_true(any(grepl("^dfs <- list\\(\\)$", script)))
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl(
    'dfs\\[\\["b"\\]\\] <- .*read_delim\\("b.csv"',
    script
  )))
})

test_that("rush_plot injects terminal-plotting GitHub packages", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt")
  expect_true(any(grepl("^#\\|   - github::coolbutuseless/devout$", script)))
  expect_true(any(grepl("^#\\|   - github::jeroenjanssens/miniansi$", script)))
  expect_true(any(grepl(
    "^#\\|   - github::coolbutuseless/devoutansi$",
    script
  )))
})

test_that("rush_plot generates a ggplot call with aesthetics", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt", y = "mpg")
  expect_true(any(grepl("^library\\(ggplot2\\)$", script)))
  ggplot_line <- script[grepl("ggplot\\(", script)]
  expect_length(ggplot_line, 1)
  expect_match(ggplot_line, "ggplot\\(df, aes\\(")
  expect_match(ggplot_line, "x = wt")
  expect_match(ggplot_line, "y = mpg")
})

test_that("rush_plot guesses geom_point when x and y given", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt", y = "mpg")
  expect_true(any(grepl("geom_point\\(\\)", script)))
})

test_that("rush_plot guesses geom_histogram when only x given", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt")
  expect_true(any(grepl("geom_histogram\\(\\)", script)))
})

test_that("rush_plot geom overrides the guessed geom", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    geom = "line"
  )
  expect_true(any(grepl("geom_line\\(\\)", script)))
  expect_false(any(grepl("geom_point", script)))
})

test_that("rush_plot log rejects invalid values", {
  expect_error(
    rush_plot("mtcars.csv", x = "wt", log = "z", dry_run = TRUE),
    "--log.*must be one of"
  )
})

test_that("rush_plot log adds log scales", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    log = "xy"
  )
  expect_true(any(grepl("scale_x_log10\\(\\)", script)))
  expect_true(any(grepl("scale_y_log10\\(\\)", script)))
})

test_that("rush_plot two-sided facets uses facet_grid", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    facets = "gear ~ cyl",
    margins = TRUE
  )
  expect_true(any(grepl("facet_grid\\(gear ~ cyl, margins = TRUE\\)", script)))
})

test_that("rush_plot one-sided facets uses facet_wrap", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    facets = "~ cyl"
  )
  expect_true(any(grepl("facet_wrap\\(~cyl\\)", script)))
})

test_that("rush_plot title, xlab, and ylab become labs()", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    title = "Cars",
    xlab = "Weight",
    ylab = "MPG"
  )
  labs_line <- script[grepl("labs\\(", script)]
  expect_length(labs_line, 1)
  expect_match(labs_line, 'title = "Cars"')
  expect_match(labs_line, 'x = "Weight"')
  expect_match(labs_line, 'y = "MPG"')
})

test_that("rush_plot pre and post wrap the plot call", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    pre = "df <- head(df)",
    post = "p + theme_bw()"
  )
  expect_true(any(grepl("p <- ggplot", script)))
  expect_true(any(grepl("theme_bw", script)))
  ggplot_line <- grep("p <- ggplot", script)
  expect_lt(grep("head\\(df\\)", script)[[1]], ggplot_line)
  expect_gt(grep("theme_bw", script), ggplot_line)
})

test_that("rush_plot with database input auto-selects first table", {
  script <- capture_script(rush_plot, file = "data.duckdb", x = "wt")
  expect_true(any(grepl("if \\(is.list\\(df\\)", script)))
  expect_true(any(grepl("df <- df\\[\\[1L\\]\\]", script)))
})

test_that("rush_plot color aesthetic works", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    color = "cyl"
  )
  ggplot_line <- script[grepl("ggplot\\(", script)]
  expect_match(ggplot_line, "color = cyl")
})

test_that("rush_plot fill aesthetic works", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    fill = "cyl",
    geom = "bar"
  )
  ggplot_line <- script[grepl("ggplot\\(", script)]
  expect_match(ggplot_line, "fill = cyl")
})
