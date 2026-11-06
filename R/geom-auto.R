#' Choose a bar chart or histogram for a single variable
#'
#' Used by `rush plot` when `--geom` is `"auto"` and only `--x` is set. When
#' the layer is added to a plot, it looks at the `x` variable: numbers, dates,
#' and times get [ggplot2::geom_histogram()]; anything else (text, factors,
#' logicals) gets [ggplot2::geom_bar()], like `qplot()` used to do.
#'
#' @param ... Passed on to [ggplot2::geom_histogram()] or [ggplot2::geom_bar()].
#'
#' @return An object that adds the chosen layer when added to a ggplot.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(day = c("Sat", "Sun", "Sun"), tip = c(1.5, 2, 3.5))
#'   ggplot(df, aes(x = day)) + geom_auto()   # bar chart
#'   ggplot(df, aes(x = tip)) + geom_auto()   # histogram
#' }
geom_auto <- function(...) {
  structure(list(args = list(...)), class = "rush_geom_auto")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.rush_geom_auto <- function(object, plot, object_name, ...) {
  geom <- if (is_continuous(auto_x(plot))) {
    ggplot2::geom_histogram
  } else {
    ggplot2::geom_bar
  }
  plot + do.call(geom, object$args)
}

# The values of the plot's x aesthetic, or NULL if they can't be determined
auto_x <- function(plot) {
  x <- plot$mapping$x
  if (is.null(x) || !is.data.frame(plot$data)) {
    return(NULL)
  }
  tryCatch(rlang::eval_tidy(x, plot$data), error = function(e) NULL)
}

is_continuous <- function(x) {
  # Without values, keep the previous behavior (a histogram)
  is.null(x) ||
    (is.numeric(x) && !is.factor(x)) ||
    inherits(x, c("Date", "POSIXt", "difftime", "hms"))
}
