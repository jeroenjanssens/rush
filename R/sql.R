#' Query files with SQL
#'
#' Generates a self-contained R script that runs a SQL query via DuckDB, then
#' executes it with [ir](https://r-lib.github.io/ir/). Input files are
#' registered as relations that can be referenced in the query.
#'
#' @param query A character string containing a DuckDB SQL query. Required.
#' @param file Character vector of file paths to expose to the query. Each file
#'   becomes a DuckDB relation named after its base name (lowercased,
#'   non-alphanumeric characters become underscores). CSV files are read with
#'   `read_csv_auto`, Parquet with `read_parquet`, JSON with `read_json_auto`,
#'   and `.duckdb` databases are attached. Use `"-"` to read CSV from standard
#'   input (named `stdin`).
#' @param output Path to write the result. Format is inferred from the
#'   extension unless `output_format` is set. `NULL` (the default) prints to
#'   stdout.
#' @param output_format Output format override. When `"auto"` (the default),
#'   the format is inferred from `output`.
#' @param delimiter Output column delimiter. Defaults to `","`.
#' @param output_delimiter Output delimiter. Overrides `delimiter` for writing.
#' @param output_header If `FALSE`, omit the header row when writing
#'   delimited output.
#' @param library Character vector of package names to load in the generated
#'   script.
#' @param tidyverse If `TRUE`, loads the tidyverse and glue packages.
#' @param head Integer. Limit the output to this many rows.
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
#' # Preview the generated script
#' rush_sql("SELECT 1 AS x", dry_run = TRUE)
#'
#' # Query a CSV file
#' rush_sql("SELECT * FROM data WHERE x > 10",
#'          file = "data.csv", dry_run = TRUE)
#'
#' \dontrun{
#' # Actually execute (requires ir on PATH)
#' rush_sql("SELECT * FROM a JOIN b USING (id)",
#'          file = c("a.csv", "b.parquet"))
#' rush_sql("SELECT * FROM data", file = "data.csv",
#'          output = "result.parquet")
#' }
#'
#' @seealso [rush_run()], [rush_plot()], [rush_convert()]
#' @export
rush_sql <- function(
  query = NULL,
  file = character(),
  output = NULL,
  output_format = "auto",
  delimiter = ",",
  output_delimiter = NULL,
  output_header = NULL,
  library = NULL,
  tidyverse = FALSE,
  head = NULL,
  seed = NULL,
  dry_run = FALSE,
  no_ir = FALSE,
  no_rush = FALSE,
  verbose = FALSE
) {
  flags <- build_flags(
    command = "sql",
    query = query,
    file = file,
    output = output,
    output_format = output_format,
    delimiter = delimiter,
    output_delimiter = output_delimiter,
    output_header = output_header,
    library = library,
    tidyverse = tidyverse,
    head = head,
    seed = seed,
    dry_run = dry_run,
    no_ir = no_ir,
    no_rush = no_rush,
    verbose = verbose
  )
  flags <- resolve_flags(flags)
  generate_script("sql", flags)
}
