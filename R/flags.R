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

flags_df <-
  tibble::tribble(
    ~short , ~long             , ~value      , ~description                  , ~default , ~arg      , ~apply              , ~category ,
    "h"    , "help"            , NA          , "Show this help"              , NA       , NA        , as.logical          , "general" ,
    NA     , "version"         , NA          , "Show version"                , NA       , NA        , as.logical          , "general" ,
    "v"    , "verbose"         , NA          , "Be verbose"                  , NA       , NA        , as.logical          , "general" ,
    "q"    , "quiet"           , NA          , "Be quiet"                    , NA       , NA        , as.logical          , "general" ,
    "n"    , "dry-run"         , NA          , "Only print generated script" , NA       , "dry_run" , as.logical          , "general" ,
    "u"    , "upgrade"         , NA          , "Upgrade packages"            , NA       , NA        , as.logical          , "install" ,
    "d"    , "delimiter"       , "str"       , "Delimiter"                   , ","      , NA        , as.character        , "read"    ,
    "H"    , "no-header"       , NA          , "No header"                   , NA       , NA        , as.logical          , "read"    ,
    "C"    , "no-clean-names"  , NA          , "No clean names"              , NA       , NA        , as.logical          , "read"    ,
    "t"    , "tidyverse"       , NA          , "Enter the Tidyverse"         , NA       , NA        , as.logical          , "setup"   ,
    "l"    , "library"         , "name"      , "Libraries to load"           , NA       , NA        , parse_syms          , "setup"   ,
    "x"    , "x"               , "name"      , "X column"                    , NA       , NA        , rlang::parse_expr   , "plot"   ,
    "y"    , "y"               , "name"      , "Y column"                    , NA       , NA        , rlang::parse_expr   , "plot"   ,
    "z"    , "z"               , "name"      , "Z column"                    , NA       , NA        , rlang::parse_expr   , "plot"   ,
    "c"    , "color"           , "name"      , "Color column"                , NA       , NA        , rlang::parse_expr   , "plot"   ,
    "a"    , "alpha"           , "name"      , "Alpha column"                , NA       , NA        , rlang::parse_expr   , "plot"   ,
    NA     , "aes"             , "key=value" , "Additional aesthetics"       , NA       , NA        , parse_named_exprs   , "plot"   ,
    NA     , "shape"           , "name"      , "Shape column"                , NA       , NA        , rlang::parse_expr   , "plot"   ,
    NA     , "group"           , "name"      , "Group column"                , NA       , NA        , rlang::parse_expr   , "plot"   ,
    NA     , "size"            , "name"      , "Size column"                 , NA       , NA        , rlang::parse_expr   , "plot"   ,
    NA    ,  "expression"      , "code"      , "Code to run"                 , NA       , NA        , rlang::parse_exprs  , NA        ,
    NA     , "post"            , "code"      , "Code to run after plotting"  , NA       , NA        , rlang::parse_exprs  , "plot"   ,
    NA     , "pre"             , "code"      , "Code to run before plotting" , NA       , NA        , rlang::parse_exprs  , "plot"   ,
    "f"    , "fill"            , "name"      , "Fill column"                 , NA       , NA        , rlang::parse_expr   , "plot"   ,
    "g"    , "geom"            , "geom"      , "Geometry"                    , "auto"   , NA        , as.character        , "plot"   ,
    NA     , "facets"          , "formula"   , "Facet specification"         , NA       , NA        , rlang::parse_expr   , "plot"   ,
    NA     , "log"             , "x|y|xy"    , "Variables to log transform"  , NA       , NA        , as.character        , "plot"   ,
    NA     , "xlab"            , "str"       , "X axis label"                , NA       , NA        , as.character        , "plot"   ,
    NA     , "ylab"            , "str"       , "Y axis label"                , NA       , NA        , as.character        , "plot"   ,
    NA     , "title"           , "str"       , "Plot title"                  , NA       , "main"    , as.character        , "plot"   ,
    NA     , "margins"         , NA          , "Display marginal facets"     , NA       , NA        , as.logical          , "plot"   ,
    "w"    , "width"           , "int"       , "Plot width"                  , NA       , NA        , as.numeric          , "save"    ,
    NA     , "height"          , "int"       , "Plot height"                 , NA       , NA        , as.numeric          , "save"    ,
    NA     , "units"           , "str"       , "Plot size units"             , "in"     , NA        , as.character        , "save"    ,
    NA     , "dpi"             , "str|int"   , "Plot resolution"             , "300"    , NA        , readr::parse_guess  , "save"    ,
    "o"    , "output"          , "str"       , "Output file"                 , NA       , NA        , as.character        , "save"    ,
  ) %>%
  dplyr::mutate(arg = dplyr::if_else(is.na(arg), long, arg))

utils::globalVariables(c(names(flags_df), "text", "text_left", "text_right"))

flags_section <- function(filter_exp = TRUE) {
  flags_df %>%
  dplyr::rowwise() %>%
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
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(text = stringr::str_c(str_column(text_left), "  ", text_right)) %>%
  dplyr::filter({{ filter_exp }}) %>%
  dplyr::arrange(long) %>%
  dplyr::pull(text) %>%
  stringr::str_c(collapse = "\n")
}


docs <- list(
"rush: R Scripting at the Command Line

Usage:
  rush [options] <command> [<args>]

Options:
{flags_section(category == 'general')}

Commands:
  plot
  run
  install",

run = "rush: Run an R expression

Usage:
  rush run [options] <expression> [--] [<file>...]

Reading options:
{flags_section(category == 'read')}

Setup options:
{flags_section(category == 'setup')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}",

install = "rush: Install a package

Usage:
  rush install [options] <package>...

Install options:
{flags_section(category == 'install')}

General options:
{flags_section(category == 'general')}",

plot = "rush: Quick plot

Usage:
  rush plot [options] [--] [<file>|-]

Reading options:
{flags_section(category == 'read')}

Setup options:
{flags_section(category == 'setup')}

Plotting options:
{flags_section(category == 'plot')}

Saving options:
{flags_section(category == 'save')}

General options:
{flags_section(category == 'general')}"
)

flags_apply <- tibble::deframe(dplyr::select(flags_df, arg, apply))

convert_flag <- function(value, name) {
  if (is.null(value) || is.logical(value)) return(value)

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

  args <- docopt::docopt(glue::glue(doc),
                         args = argv,
                         version = as.character(utils::packageVersion("rush")),
                         strict = FALSE)

  args$command <- command
  args <- args[stringr::str_starts(names(args), "[:alnum:]")]
  args <- purrr::map2(args, names(args), convert_flag)
  args[order(names(args))]
}

extra <- list(a = 1, b = 2)

format_flag <- function(name, value) {
  if (is.list(value)) {
    # value_text <- stringr::str_c(purrr::map_chr(value, rlang::expr_text), collapse = "; ")
    value_text <- rlang::expr_text(value)
    if (length(value) >= 1) value <- value[[1]]
  } else{
    value_text <- rlang::expr_text(value)
  }

  glue::glue("{pillar::align(name, 15)} ",
             "{cli::style_italic(cli::col_blue('<',pillar::type_sum(value),'>'))} ",
             "{ifelse(is.null(value), '', value_text)}")
}

format_flag("extra", extra)
