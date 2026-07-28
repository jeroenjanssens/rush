#' Create a quick plot
#'
#' Generates a self-contained R script that reads data and produces a ggplot2
#' visualization, then executes it with [ir](https://r-lib.github.io/ir/).
#' When writing to a terminal, the plot is rendered as ANSI art; otherwise it
#' is saved to a file or streamed as PNG.
#'
#' @param file Character vector of input file paths. The reader is chosen by
#'   extension (override with `input_format`). Use `"-"` to read from standard
#'   input. The first file is available as `df`.
#' @param x,y,z Column names for the x, y, and z aesthetics (as strings).
#' @param color,fill,alpha,size,shape,group Column names for additional
#'   aesthetics (as strings).
#' @param geom Geometry to use. One of `"auto"`, `"point"`, `"line"`,
#'   `"histogram"`, `"bar"`, `"boxplot"`, `"density"`, etc. When `"auto"`,
#'   `geom_point()` is used if both `x` and `y` are set, `geom_histogram()`
#'   otherwise.
#' @param facets A facet specification as a string. Use a two-sided formula
#'   (e.g. `"gear ~ cyl"`) for [ggplot2::facet_grid()] or a one-sided formula
#'   (e.g. `"~ cyl"`) for [ggplot2::facet_wrap()].
#' @param log Which axes to log-transform. One of `"x"`, `"y"`, or `"xy"`.
#' @param title Plot title.
#' @param xlab,ylab Axis labels.
#' @param margins If `TRUE`, display marginal facets (only with
#'   [ggplot2::facet_grid()]).
#' @param pre A character string of R code to run before plotting (e.g. to
#'   transform `df`).
#' @param post A character string of R code to run after plotting (e.g. to
#'   add layers to `p`).
#' @param output Path to save the plot. When `NULL` (the default), renders to
#'   the terminal as ANSI art if interactive, or streams PNG to stdout.
#' @param width,height Plot dimensions. In terminal mode, `width` is in
#'   characters; for file output, in `units`.
#' @param units Size units for file output. One of `"in"`, `"cm"`, `"mm"`, or
#'   `"px"`.
#' @param dpi Resolution for file output.
#' @param input_format Input format override. When `"auto"` (the default), the
#'   format is inferred from the file extension.
#' @param delimiter Input column delimiter. Defaults to `","`.
#' @param input_delimiter Input delimiter. Overrides `delimiter` for reading.
#' @param header If `FALSE`, the input file is read without column names.
#' @param clean_names If `TRUE` (the default), column names are cleaned with
#'   [janitor::clean_names()].
#' @param library Character vector of package names to load in the generated
#'   script.
#' @param tidyverse If `TRUE`, loads the tidyverse and glue packages.
#' @param sheet Sheet to read from an Excel file. Either a string (sheet name)
#'   or an integer (sheet index).
#' @param seed Integer seed for the random number generator.
#' @param dry_run If `TRUE`, print the generated script instead of executing
#'   it.
#' @param no_ir If `TRUE`, execute the script with `Rscript` instead of `ir`.
#' @param verbose If `TRUE`, print debugging information to stderr.
#'
#' @return Invisibly returns the exit status of the script (integer), or
#'   `NULL` when `dry_run = TRUE`.
#'
#' @examples
#' # Preview a scatter plot script
#' rush_plot("mtcars.csv", x = "wt", y = "mpg", dry_run = TRUE)
#'
#' # Histogram with color
#' rush_plot("mtcars.csv", x = "mpg", color = "cyl",
#'           geom = "histogram", dry_run = TRUE)
#'
#' # Faceted plot with title
#' rush_plot("mtcars.csv", x = "wt", y = "mpg",
#'           facets = "~ cyl", title = "Weight vs MPG", dry_run = TRUE)
#'
#' \dontrun{
#' # Actually execute (requires ir on PATH)
#' rush_plot("mtcars.csv", x = "wt", y = "mpg")
#' rush_plot("mtcars.csv", x = "wt", y = "mpg",
#'           output = "plot.png", width = 8, height = 6)
#' }
#'
#' @seealso [rush_run()], [rush_sql()], [rush_convert()]
#' @export
rush_plot <- function(
    file = "-",
    x = NULL,
    y = NULL,
    z = NULL,
    color = NULL,
    fill = NULL,
    alpha = NULL,
    size = NULL,
    shape = NULL,
    group = NULL,
    geom = "auto",
    facets = NULL,
    log = NULL,
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    margins = FALSE,
    pre = NULL,
    post = NULL,
    output = NULL,
    width = NULL,
    height = NULL,
    units = "in",
    dpi = 300,
    input_format = "auto",
    delimiter = ",",
    input_delimiter = NULL,
    header = TRUE,
    clean_names = TRUE,
    library = NULL,
    tidyverse = FALSE,
    sheet = NULL,
    seed = NULL,
    dry_run = FALSE,
    no_ir = FALSE,
    verbose = FALSE) {
  flags <- build_flags(
    command = "plot",
    file = file,
    output = output,
    input_format = input_format,
    delimiter = delimiter,
    input_delimiter = input_delimiter,
    header = header,
    clean_names = clean_names,
    library = library,
    tidyverse = tidyverse,
    sheet = sheet,
    seed = seed,
    dry_run = dry_run,
    no_ir = no_ir,
    verbose = verbose,
    x = x,
    y = y,
    z = z,
    color = color,
    fill = fill,
    alpha = alpha,
    size = size,
    shape = shape,
    group = group,
    geom = geom,
    facets = facets,
    log = log,
    title = title,
    xlab = xlab,
    ylab = ylab,
    margins = margins,
    pre = pre,
    post = post,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
  flags <- resolve_flags(flags)
  generate_script("plot", flags)
}
