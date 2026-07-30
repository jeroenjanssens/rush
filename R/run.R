#' Run an R expression
#'
#' Generates a self-contained R script from the given expression and input
#' files, then executes it with [ir](https://r-lib.github.io/ir/). The value
#' of the last expression is printed to stdout or written to `output`.
#'
#' @param expr A character string of R code to evaluate. The value of the last
#'   expression becomes the result. May also be a language object created with
#'   [quote()] or a list of language objects from [rlang::exprs()].
#' @param file Character vector of input file paths. The reader is chosen by
#'   extension (override with `input_format`). Use `"-"` to read from standard
#'   input. The first file is available as `df`; all files are in a named list
#'   `dfs`.
#' @param output Path to write the result. Format is inferred from the
#'   extension unless `output_format` is set. `NULL` (the default) prints to
#'   stdout.
#' @param output_format Output format override. When `"auto"` (the default),
#'   the format is inferred from `output`.
#' @param input_format Input format override. When `"auto"` (the default), the
#'   format is inferred from the file extension.
#' @param delimiter Character used as both input and output column delimiter.
#'   Defaults to `","`. Overridden by `input_delimiter` or `output_delimiter`
#'   when set.
#' @param input_delimiter Input delimiter. Overrides `delimiter` for reading.
#' @param output_delimiter Output delimiter. Overrides `delimiter` for writing.
#' @param header If `FALSE`, suppress headers on both input and output.
#'   Overridden by `input_header` or `output_header` when set.
#' @param input_header If `FALSE`, the input file is read without column
#'   names. Overrides `header` for reading.
#' @param output_header If `FALSE`, omit the header row when writing
#'   delimited output. Overrides `header` for writing.
#' @param names Comma-separated column names to use (e.g. `"a,b,c"`).
#'   Implies `input_header = FALSE`.
#' @param clean_names If `TRUE` (the default), column names are cleaned with
#'   [janitor::clean_names()].
#' @param library Character vector of package names to load in the generated
#'   script.
#' @param tidyverse If `TRUE`, loads the tidyverse and glue packages.
#' @param head Integer. Limit the output to this many rows.
#' @param input_sheet Sheet to read from an Excel file. Either a string (sheet
#'   name) or an integer (sheet index).
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
#' # Preview the generated script for a simple expression
#' rush_run("1 + 1", dry_run = TRUE)
#'
#' # Read a file and apply an expression
#' rush_run("head(df)", file = "data.csv", dry_run = TRUE)
#'
#' # Use a language object
#' rush_run(quote(nrow(df)), file = "data.csv", dry_run = TRUE)
#'
#' \dontrun{
#' # Actually execute (requires ir on PATH)
#' rush_run("1 + 1")
#' rush_run("dplyr::filter(df, x > 10)", file = "data.csv",
#'          output = "filtered.parquet")
#' }
#'
#' @seealso [rush_sql()], [rush_plot()], [rush_convert()]
#' @export
rush_run <- function(
  expr = NULL,
  file = character(),
  output = NULL,
  output_format = "auto",
  input_format = "auto",
  delimiter = ",",
  input_delimiter = NULL,
  output_delimiter = NULL,
  header = TRUE,
  input_header = NULL,
  output_header = NULL,
  names = NULL,
  clean_names = TRUE,
  library = NULL,
  tidyverse = FALSE,
  head = NULL,
  input_sheet = NULL,
  output_root = "root",
  output_record = "record",
  output_sheet = NULL,
  output_indent = 2L,
  seed = NULL,
  dry_run = FALSE,
  no_ir = FALSE,
  no_rush = FALSE,
  verbose = FALSE
) {
  flags <- build_flags(
    command = "run",
    expr = expr,
    file = file,
    output = output,
    output_format = output_format,
    input_format = input_format,
    delimiter = delimiter,
    input_delimiter = input_delimiter,
    output_delimiter = output_delimiter,
    header = header,
    input_header = input_header,
    output_header = output_header,
    names = names,
    clean_names = clean_names,
    library = library,
    tidyverse = tidyverse,
    head = head,
    input_sheet = input_sheet,
    output_root = output_root,
    output_record = output_record,
    output_sheet = output_sheet,
    output_indent = output_indent,
    seed = seed,
    dry_run = dry_run,
    no_ir = no_ir,
    no_rush = no_rush,
    verbose = verbose
  )
  flags <- resolve_flags(flags)
  generate_script("run", flags)
}
