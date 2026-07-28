#' @keywords internal
"_PACKAGE"

#' @section Main functions:
#' \describe{
#'   \item{[rush_run()]}{Run an R expression on data files}
#'   \item{[rush_sql()]}{Query files with SQL via DuckDB}
#'   \item{[rush_plot()]}{Create a quick ggplot2 visualization}
#'   \item{[rush_convert()]}{Convert between file formats}
#' }
#'
#' @section How it works:
#' Each function assembles a self-contained R script with package dependencies
#' declared in frontmatter, then executes it with the
#' [ir](https://r-lib.github.io/ir/) command-line tool. This means packages
#' listed in Suggests (readr, nanoparquet, jsonlite, etc.) are resolved by ir
#' at runtime, not required at install time.
NULL
