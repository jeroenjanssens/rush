layer_stat <- function(plot) class(plot$layers[[1]]$stat)[1]

test_that("geom_auto uses a bar chart for text, factors, and logicals", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    time = c("Dinner", "Lunch", "Dinner"),
    size = c(2, 3, 2),
    ok = c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    layer_stat(ggplot2::ggplot(df, ggplot2::aes(x = time)) + geom_auto()),
    "StatCount"
  )
  expect_equal(
    layer_stat(
      ggplot2::ggplot(df, ggplot2::aes(x = factor(size))) + geom_auto()
    ),
    "StatCount"
  )
  expect_equal(
    layer_stat(ggplot2::ggplot(df, ggplot2::aes(x = ok)) + geom_auto()),
    "StatCount"
  )
})

test_that("geom_auto uses a histogram for numbers and dates", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(tip = c(1.5, 2, 3.5), day = as.Date("2021-12-14") + 0:2)
  expect_equal(
    layer_stat(ggplot2::ggplot(df, ggplot2::aes(x = tip)) + geom_auto()),
    "StatBin"
  )
  expect_equal(
    layer_stat(ggplot2::ggplot(df, ggplot2::aes(x = day)) + geom_auto()),
    "StatBin"
  )
})

test_that("geom_auto passes arguments to the chosen geom", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(tip = c(1.5, 2, 3.5))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = tip)) + geom_auto(bins = 5)
  expect_equal(p$layers[[1]]$stat_params$bins, 5)
})

test_that("geom_auto falls back to a histogram without data", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = tip)) + geom_auto()
  expect_equal(layer_stat(p), "StatBin")
})
