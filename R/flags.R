str_column <- function(x) {
  stringr::str_pad(x, max(stringr::str_length(x)), side = "right")
}

parse_syms <- function(x) {
  parts <- unlist(strsplit(x, ","))
  purrr::map(parts, rlang::sym)
}

parse_named_exprs <- function(x) {
  rlang::call_args(rlang::parse_expr(paste0("list(", x, ")")))
}

# Guess whether a flag value is numeric (e.g. --dpi 300) or should stay a
# string; a lightweight stand-in for readr::parse_guess.
parse_guess <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (!anyNA(n)) n else x
}

flags_df <-
  tibble::tribble(
    ~short , ~long              , ~value      , ~description                      , ~default , ~arg               , ~apply             , ~category ,
    "h"    , "help"             , NA          , "Show this help"                  , NA       , NA                 , as.logical         , "general" ,
    NA     , "version"          , NA          , "Show version"                    , NA       , NA                 , as.logical         , "general" ,
    NA     , "seed"             , "int"       , "Seed random number generator"    , NA       , NA                 , as.integer         , "general" ,
    "v"    , "verbose"          , NA          , "Be verbose"                      , NA       , NA                 , as.logical         , "general" ,
    "n"    , "dry-run"          , NA          , "Only print generated script"     , NA       , "dry_run"          , as.logical         , "general" ,
    "I"    , "no-ir"            , NA          , "Run with Rscript, not ir"        , NA       , "no_ir"            , as.logical         , "general" ,
    "R"    , "no-rush"          , NA          , "Inline all code (no rush dep)"   , NA       , "no_rush"          , as.logical         , "general" ,
    "d"    , "delimiter"        , "str"       , "Delimiter (input and output)"    , ","      , NA                 , as.character       , "read"    ,
    NA     , "input-delimiter"  , "str"       , "Input delimiter (overrides -d)"  , NA       , "input_delimiter"  , as.character       , "read"    ,
    "F"    , "input-format"     , "format"    , "Input format"                    , "auto"   , "input_format"     , as.character       , "read"    ,
    NA     , "input-sheet"      , "name|int"  , "Excel sheet to read"             , NA       , "input_sheet"      , parse_guess        , "read"    ,
    "H"    , "no-header"        , NA          , "No header"                       , NA       , NA                 , as.logical         , "read"    ,
    "C"    , "no-clean-names"   , NA          , "No clean names"                  , NA       , NA                 , as.logical         , "read"    ,
    "t"    , "tidyverse"        , NA          , "Enter the Tidyverse"             , NA       , NA                 , as.logical         , "setup"   ,
    "l"    , "library"          , "name"      , "Libraries to load"               , NA       , NA                 , parse_syms         , "setup"   ,
    "x"    , "x"                , "name"      , "X column"                        , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    "y"    , "y"                , "name"      , "Y column"                        , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    "z"    , "z"                , "name"      , "Z column"                        , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    "c"    , "color"            , "name"      , "Color column"                    , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    "a"    , "alpha"            , "name"      , "Alpha column"                    , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    NA     , "aes"              , "key=value" , "Additional aesthetics"           , NA       , NA                 , parse_named_exprs  , "plot"    ,
    NA     , "shape"            , "name"      , "Shape column"                    , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    NA     , "group"            , "name"      , "Group column"                    , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    NA     , "size"             , "name"      , "Size column"                     , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    NA     , "expression"       , "code"      , "Code to run"                     , NA       , NA                 , rlang::parse_exprs , NA        ,
    NA     , "post"             , "code"      , "Code to run after plotting"      , NA       , NA                 , rlang::parse_exprs , "plot"    ,
    NA     , "pre"              , "code"      , "Code to run before plotting"     , NA       , NA                 , rlang::parse_exprs , "plot"    ,
    "f"    , "fill"             , "name"      , "Fill column"                     , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    "g"    , "geom"             , "geom"      , "Geometry"                        , "auto"   , NA                 , as.character       , "plot"    ,
    NA     , "facets"           , "formula"   , "Facet specification"             , NA       , NA                 , rlang::parse_expr  , "plot"    ,
    NA     , "log"              , "x|y|xy"    , "Variables to log transform"      , NA       , NA                 , as.character       , "plot"    ,
    NA     , "xlab"             , "str"       , "X axis label"                    , NA       , NA                 , as.character       , "plot"    ,
    NA     , "ylab"             , "str"       , "Y axis label"                    , NA       , NA                 , as.character       , "plot"    ,
    NA     , "title"            , "str"       , "Plot title"                      , NA       , NA                 , as.character       , "plot"    ,
    NA     , "margins"          , NA          , "Display marginal facets"         , NA       , NA                 , as.logical         , "plot"    ,
    "D"    , "output-delimiter" , "str"       , "Output delimiter (overrides -d)" , NA       , "output_delimiter" , as.character       , "save"    ,
    "O"    , "output-format"    , "format"    , "Output format"                   , "auto"   , "output_format"    , as.character       , "save"    ,
    NA     , "output-root"      , "str"       , "Root element name (XML)"         , "root"   , "output_root"      , as.character       , "save"    ,
    NA     , "output-record"    , "str"       , "Record element name (XML, TOML)" , "record" , "output_record"    , as.character       , "save"    ,
    NA     , "output-sheet"     , "str"       , "Excel sheet name to write"       , NA       , "output_sheet"     , as.character       , "save"    ,
    NA     , "output-indent"    , "int"       , "Indentation level (JSON, YAML)"  , "2"      , "output_indent"    , as.integer         , "save"    ,
    NA     , "head"             , "int"       , "Limit output rows"               , NA       , NA                 , as.integer         , "save"    ,
    "w"    , "width"            , "int"       , "Plot width"                      , NA       , NA                 , as.numeric         , "save"    ,
    NA     , "height"           , "int"       , "Plot height"                     , NA       , NA                 , as.numeric         , "save"    ,
    NA     , "units"            , "str"       , "Plot size units"                 , "in"     , NA                 , as.character       , "save"    ,
    NA     , "dpi"              , "str|int"   , "Plot resolution"                 , "300"    , NA                 , parse_guess        , "save"    ,
    "o"    , "output"           , "str"       , "Output file"                     , NA       , NA                 , as.character       , "save"    ,
  ) |>
  dplyr::mutate(arg = dplyr::if_else(is.na(arg), long, arg))

utils::globalVariables(c(
  names(flags_df),
  "text",
  "text_left",
  "text_right",
  "df",
  "dfs"
))

flags_section <- function(filter_exp = TRUE) {
  flags_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      text_left = glue::glue(
        "  ",
        dplyr::if_else(!is.na(short), "-{short}, ", "    "),
        "--{long}",
        dplyr::if_else(!is.na(value), " <{value}>", "")
      ),
      text_right = glue::glue(
        "{description}",
        dplyr::if_else(!is.na(default), " [default: {default}]", ""),
        "."
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      text = stringr::str_c(str_column(text_left), "  ", text_right)
    ) |>
    dplyr::filter({{ filter_exp }}) |>
    dplyr::arrange(long) |>
    dplyr::pull(text) |>
    stringr::str_c(collapse = "\n")
}


docs <- list(
  "rush: R Scripting at the Command Line

Usage:
  rush [options] <command> [<args>]

Options:
{flags_section(category == 'general')}

Commands:
  convert
  plot
  run
  sql",

  run = "rush: Run an R expression

Usage:
  rush run [options] [<expression>] [--] [<file>...]

Arguments:
  <expression>             R expression to evaluate. The value of the last
                           expression is printed or written out.
  <file>                   Data file(s) to read. Two variables are always set:
                           'df' (the first input) and 'dfs' (a named list of
                           all inputs, keyed by file stem). Database files
                           (.duckdb, .sqlite) are nested one level deeper:
                           dfs$dbname$tablename. The reader is chosen by
                           extension. Use '-' to read from standard input
                           (becomes dfs$stdin).

Note: -d/--delimiter sets both input and output delimiter. Use -F, -D, or -O
to override format and delimiter for each side independently.

Reading options:
{flags_section(category == 'read')}

Setup options:
{flags_section(category == 'setup')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}",

  plot = "rush: Quick plot

Usage:
  rush plot [options] [--] [<file>...]

Arguments:
  <file>                   Data file(s) to read before plotting. The reader is
                           chosen by extension: '.parquet'/'.pq' via
                           nanoparquet, '.duckdb'/'.ddb' via DuckDB, everything
                           else as delimited text. A single file is read into a
                           data frame named 'df'; use '-' or omit to read
                           delimited text from standard input. Multiple files
                           are each read into a named element of a list 'dfs';
                           combine them into 'df' yourself with the --pre
                           option, e.g. 'df <- dplyr::bind_rows(dfs)'.

Reading options:
{flags_section(category == 'read')}

Setup options:
{flags_section(category == 'setup')}

Plotting options:
{flags_section(category == 'plot')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}",

  convert = "rush: Convert between file formats

Usage:
  rush convert [options] [<file>...]

Arguments:
  <file>                   Input file(s) to convert. The reader is chosen by
                           extension (or overridden with -F). Use '-' to read
                           from standard input. Output file is specified with
                           --output (-o); its format is inferred from extension
                           (or overridden with -O).

Reading options:
{flags_section(category == 'read')}

Setup options:
{flags_section(category == 'setup')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}",

  sql = "rush: Query files with SQL (DuckDB)

Usage:
  rush sql [options] [<query>] [--] [<file>...]

Arguments:
  <query>                  DuckDB SQL query to run. Its result is printed or
                           written out.
  <file>                   File(s) to expose to the query, each as a DuckDB
                           relation named after the file's base name (lowercased,
                           non-alphanumeric chars become underscores; names
                           starting with a digit are prefixed with 'x'). The
                           relation is double-quoted in the generated SQL, so
                           refer to it as e.g. SELECT * FROM \"x2024\". CSV files
                           are read with read_csv_auto, Parquet with read_parquet,
                           and a '.duckdb' database is attached so its tables are
                           addressed as name.table. Use '-' to read CSV from
                           standard input as a relation named 'stdin'. Files with
                           the same base name in different directories collide.

Setup options:
{flags_section(category == 'setup')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}"
)

flags_apply <- tibble::deframe(dplyr::select(flags_df, arg, apply))

convert_flag <- function(value, name) {
  if (is.null(value) || is.logical(value)) {
    return(value)
  }

  if (name %in% names(flags_apply)) {
    flags_apply[[name]](value)
  } else {
    value
  }
}

parse_arguments <- function(...) {
  argv <- c(...)
  command <- NULL
  if (argv[1] %in% names(docs)) {
    command <- argv[1]
    doc <- docs[[command]]
  } else {
    doc <- docs[[1]]
  }

  args <- docopt::docopt(
    glue::glue(doc),
    args = argv,
    version = as.character(utils::packageVersion("rush")),
    strict = FALSE
  )

  args$command <- command
  args <- args[stringr::str_starts(names(args), "[:alnum:]")]
  args <- purrr::map2(args, names(args), convert_flag)
  args[order(names(args))]
}

format_flag <- function(name, value) {
  if (is.list(value)) {
    value_text <- rlang::expr_text(value)
    if (length(value) >= 1) value <- value[[1]]
  } else {
    value_text <- rlang::expr_text(value)
  }

  name <- pillar::align(name, 15)
  type <- pillar::type_sum(value)
  glue::glue(
    "{name} ",
    "{cli::style_italic(cli::col_blue('<', type, '>'))} ",
    "{ifelse(is.null(value), '', value_text)}"
  )
}
