#' Convert between file formats
#'
#' Generates a self-contained R script that reads one or more input files and
#' writes them in a different format, then executes it with
#' [ir](https://r-lib.github.io/ir/).
#'
#' @param file Character vector of input file paths. At least one file is
#'   required. The reader is chosen by extension (override with
#'   `input_format`). Use `"-"` to read from standard input.
#' @param output Output file path. The format is inferred from the extension
#'   unless `output_format` is set. Supports template placeholders for
#'   multi-file conversion: `%(file_name)s` and `%(file_index)d`.
#' @param output_format Output format override. When `"auto"` (the default),
#'   the format is inferred from `output`.
#' @param input_format Input format override. When `"auto"` (the default), the
#'   format is inferred from the file extension.
#' @param delimiter Character used as both input and output column delimiter.
#'   Defaults to `","`. Overridden by `input_delimiter` or `output_delimiter`
#'   when set.
#' @param input_delimiter Input delimiter. Overrides `delimiter` for reading.
#' @param output_delimiter Output delimiter. Overrides `delimiter` for writing.
#' @param header If `FALSE`, the input file is read without column names.
#' @param clean_names If `TRUE` (the default), column names are cleaned with
#'   [janitor::clean_names()].
#' @param head Integer. Limit the output to this many rows.
#' @param input_sheet Sheet to read from an Excel file. Either a string (sheet
#'   name) or an integer (sheet index).
#' @param dry_run If `TRUE`, print the generated script instead of executing
#'   it.
#' @param no_ir If `TRUE`, execute the script with `Rscript` instead of `ir`.
#' @param verbose If `TRUE`, print debugging information to stderr.
#'
#' @return Invisibly returns the exit status of the script (integer), or
#'   `NULL` when `dry_run = TRUE`.
#'
#' @examples
#' # Preview a CSV-to-Parquet conversion
#' rush_convert("data.csv", output = "data.parquet", dry_run = TRUE)
#'
#' # Multiple files with a template
#' rush_convert(c("a.csv", "b.csv"),
#'              output = "%(file_name)s.parquet", dry_run = TRUE)
#'
#' \dontrun{
#' # Actually execute (requires ir on PATH)
#' rush_convert("data.csv", output = "data.parquet")
#' rush_convert("data.json", output = "data.csv")
#' }
#'
#' @seealso [rush_run()], [rush_sql()], [rush_plot()]
#' @export
rush_convert <- function(
  file,
  output = NULL,
  output_format = "auto",
  input_format = "auto",
  delimiter = ",",
  input_delimiter = NULL,
  output_delimiter = NULL,
  header = TRUE,
  clean_names = TRUE,
  head = NULL,
  input_sheet = NULL,
  output_root = "root",
  output_record = "record",
  output_sheet = NULL,
  output_indent = 2L,
  dry_run = FALSE,
  no_ir = FALSE,
  verbose = FALSE
) {
  flags <- build_flags(
    command = "convert",
    file = file,
    output = output,
    output_format = output_format,
    input_format = input_format,
    delimiter = delimiter,
    input_delimiter = input_delimiter,
    output_delimiter = output_delimiter,
    header = header,
    clean_names = clean_names,
    head = head,
    input_sheet = input_sheet,
    output_root = output_root,
    output_record = output_record,
    output_sheet = output_sheet,
    output_indent = output_indent,
    dry_run = dry_run,
    no_ir = no_ir,
    verbose = verbose
  )
  flags <- resolve_flags(flags)
  generate_script("convert", flags)
}
