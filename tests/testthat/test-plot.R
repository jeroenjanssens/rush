test_that("rush_plot reads a single file into df", {
  script <- capture_script(rush_plot, file = "a.csv", x = "wt")
  expect_true(any(grepl(
    'dfs\\[\\["a"\\]\\] <- .*read_delim\\("a.csv"',
    script
  )))
  expect_true(any(grepl("df <- dfs\\[\\[1(L)?\\]\\]", script)))
})

test_that("rush_plot without a file is an error", {
  expect_error(capture_script(rush_plot, x = "wt"), "No input file to plot")
})

test_that("rush_plot reads standard input with file = \"-\"", {
  script <- capture_script(rush_plot, file = "-", x = "wt")
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
  expect_true(all(paste0("#|   - ", terminal_plot_packages) %in% script))
})

test_that("terminal-plotting packages come from pinned jeroenjanssens forks", {
  expect_named(terminal_plot_packages, c("devout", "miniansi", "devoutansi"))
  expect_match(
    terminal_plot_packages,
    "^github::jeroenjanssens/(devout|miniansi|devoutansi)@[0-9a-f]{40}$"
  )
  repos <- sub(
    "^github::jeroenjanssens/([^@]+)@.*$",
    "\\1",
    terminal_plot_packages
  )
  expect_identical(unname(repos), names(terminal_plot_packages))
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

test_that("rush_plot chooses the geom when only x given, without rush", {
  # Inline scripts don't load rush, so they can't use rush::geom_auto()
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt")
  expect_false(any(grepl("rush::", script, fixed = TRUE)))
  expect_true(any(grepl("geom_histogram()", script, fixed = TRUE)))
  expect_true(any(grepl("geom_bar()", script, fixed = TRUE)))
})

test_that("the inline geom choice picks a bar chart for a discrete x", {
  skip_if_not_installed("ggplot2")
  flags <- list(x = quote(time), geom = "auto")
  geom_call <- build_plot_call(flags)[[3]]
  df <- data.frame(time = c("Dinner", "Lunch"), tip = c(1.5, 2))
  layer <- eval(geom_call, list(df = df), asNamespace("ggplot2"))
  expect_equal(class(layer$stat)[1], "StatCount")
  flags$x <- quote(tip)
  layer <- eval(
    build_plot_call(flags)[[3]],
    list(df = df),
    asNamespace("ggplot2")
  )
  expect_equal(class(layer$stat)[1], "StatBin")
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

test_that("rush_plot generates devoutansi dispatch when no output", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt")
  expect_true(any(grepl("devoutansi::ansi\\(", script)))
  expect_true("if (.has_tty) {" %in% script)
})

test_that("rush_plot asks devoutansi for plain text in ascii mode", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    output_format = "ascii"
  )
  expect_true(any(grepl("colour = FALSE", script, fixed = TRUE)))
  expect_false(any(grepl("ansi_strip", script, fixed = TRUE)))
})

test_that("rush_plot applies theme_minimal when no --post", {
  script <- capture_script(rush_plot, file = "mtcars.csv", x = "wt")
  expect_true(any(grepl("theme_minimal\\(\\)", script)))
  expect_true(any(grepl("panel.grid = .*element_blank", script)))
})

test_that("rush_plot skips default theme when --post is used", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    post = "p + theme_bw()"
  )
  expect_false(any(grepl("theme_minimal", script, fixed = TRUE)))
})

test_that("rush_plot passes width to ansi device", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    width = 40
  )
  expect_true(any(grepl("width = 40", script)))
})

test_that("rush_plot output_format ansi forces terminal art in script", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    output_format = "ansi"
  )
  expect_true(any(grepl("devoutansi::ansi\\(", script)))
  expect_false(any(grepl(".has_tty", script, fixed = TRUE)))
  expect_false(any(grepl("ggsave", script, fixed = TRUE)))
})

test_that("rush_plot output_format ascii forces terminal art in script", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    output_format = "ascii"
  )
  expect_true(any(grepl("plain_ascii = TRUE", script, fixed = TRUE)))
  expect_false(any(grepl(".has_tty", script, fixed = TRUE)))
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

# --geom-args ---------------------------------------------------------------

test_that("geom_args are passed to an explicit geom", {
  script <- capture_script(
    rush_plot,
    file = "wine.csv",
    x = "quality",
    fill = "type",
    geom = "density",
    geom_args = "adjust = 3, alpha = 0.5"
  )
  expect_true(any(grepl(
    "geom_density(adjust = 3, alpha = 0.5)",
    script,
    fixed = TRUE
  )))
})

test_that("geom_args can be a list", {
  script <- capture_script(
    rush_plot,
    file = "wine.csv",
    x = "quality",
    geom = "density",
    geom_args = list(adjust = 3, alpha = 0.5)
  )
  expect_true(any(grepl(
    "geom_density(adjust = 3, alpha = 0.5)",
    script,
    fixed = TRUE
  )))
})

test_that("geom_args are passed to the guessed geom_point", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    y = "mpg",
    geom_args = "size = 3"
  )
  expect_true(any(grepl("geom_point(size = 3)", script, fixed = TRUE)))
})

test_that("geom_args are passed to both branches of the inline geom choice", {
  script <- capture_script(
    rush_plot,
    file = "mtcars.csv",
    x = "wt",
    geom_args = "fill = \"steelblue\""
  )
  expect_true(any(grepl(
    "geom_histogram(fill = \"steelblue\")",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "geom_bar(fill = \"steelblue\")",
    script,
    fixed = TRUE
  )))
})

test_that("geom_args are passed to rush::geom_auto() in compact scripts", {
  flags <- list(x = quote(wt), geom = "auto", geom_args = list(bins = 10))
  geom_call <- build_plot_call(flags, compact = TRUE)[[3]]
  expect_equal(rlang::expr_deparse(geom_call), "rush::geom_auto(bins = 10)")
})

test_that("without geom_args, geoms get no arguments", {
  flags <- list(x = quote(wt), y = quote(mpg), geom = "density")
  expect_equal(
    rlang::expr_deparse(build_plot_call(flags)[[3]]),
    "geom_density()"
  )
  flags$geom_args <- list()
  expect_equal(
    rlang::expr_deparse(build_plot_call(flags)[[3]]),
    "geom_density()"
  )
})

test_that("geom_args end up in the layer's parameters", {
  skip_if_not_installed("ggplot2")
  flags <- list(
    x = quote(quality),
    fill = quote(type),
    geom = "density",
    geom_args = parse_geom_args("adjust = 3, alpha = 0.5")
  )
  df <- data.frame(
    quality = c(5, 6, 6, 7),
    type = c("red", "white", "red", "white")
  )
  plot <- eval(build_plot_call(flags), list(df = df), asNamespace("ggplot2"))
  layer <- plot$layers[[1]]
  expect_equal(layer$stat_params$adjust, 3)
  # A fixed alpha (not mapped, so no legend for it)
  expect_equal(layer$aes_params$alpha, 0.5)
  expect_null(plot$mapping$alpha)
})

test_that("geom_auto() passes geom_args on to the geom it picks", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(wt = c(1.5, 2.5, 3.5), time = c("Dinner", "Lunch", "Dinner"))
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = wt)) + geom_auto(bins = 7)
  expect_equal(plot$layers[[1]]$stat_params$bins, 7)
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = time)) + geom_auto(width = 0.5)
  expect_equal(plot$layers[[1]]$stat_params$width, 0.5)
})

test_that("parse_geom_args parses named and unnamed arguments", {
  args <- parse_geom_args("adjust = 3, alpha = 0.5")
  expect_equal(names(args), c("adjust", "alpha"))
  expect_equal(args$adjust, 3)
  args <- parse_geom_args("stat = \"identity\", position = position_dodge(0.9)")
  expect_equal(args$stat, "identity")
  expect_true(rlang::is_call(args$position, "position_dodge"))
})

test_that("invalid geom_args give a clear error", {
  expect_error(parse_geom_args("adjust = (3"), "--geom-args")
  expect_error(parse_geom_args("adjust ="), "--geom-args")
  expect_error(parse_geom_args("adjust = 3,, alpha = 1"), "--geom-args")
  expect_error(
    capture_script(rush_plot, file = "mtcars.csv", x = "wt", geom_args = 3),
    "geom_args"
  )
})
