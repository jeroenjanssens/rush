#' Main entry point
#'
#' @param ... character vector of parameters
#'
#' @export
rush <- function(...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse flags
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  flags <- parse_arguments(...)
  has_tty <- isatty(stdout())

  if (has_tty) {
    options(width = flags$width %||% cli::console_width())
  }

  if (flags$verbose) {
    cli::cat_rule("Arguments", file = stderr())
    cli::cat_bullet(purrr::map2(names(flags), flags, format_flag),
                    bullet_col = "yellow", file = stderr())
    cli::cat_rule(file = stderr())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create script
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  filename <- tempfile()
  script <- file(filename, open = "w")
  on.exit(unlink(filename))

  code_expression(script, "#!/usr/bin/env Rscript")

  if (flags$command == "install") {
    code_expression(script, utils::install.packages(!!flags$package))
  }

  if (flags$command == "run") {
    # Load libraries
    if (flags$tidyverse) {
      code_library(script, "tidyverse")
      code_library(script, "glue")
    }
    if (!is.null(flags$library)) {
      purrr::walk(flags$library, function(e) code_library(script, e))
    }

    # Read files
    if (length(flags$file) == 1) {
      df_file <- flags$file
      if (df_file == "-") df_file <- expr(file("stdin", "rb", raw = TRUE))
      read_expr <- expr(readr::read_delim(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
      if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
      code_expression(script, `<-`(df, !!read_expr))
    } else if (length(flags$file) > 1) {
      df_names <-
        ifelse(flags$file == "-", "stdin",
               fs::path_ext_remove(basename(flags$file))) %>%
        janitor::make_clean_names()

      code_expression(script, dfs <- list())
      for (i in seq_along(df_names)) {
        df_name <- rlang::parse_expr(paste0("dfs$", df_names[[i]]))
        df_file <- flags$file[[i]]
        if (df_file == "-") df_file <- expr(file("stdin", "rb", raw = TRUE))

        read_expr <- expr(readr::read_delim(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
        if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
        code_expression(script, !!rlang::call2("<-", df_name, read_expr))
      }
    }

    # Add expressions
    if (!is.null(flags$expression)) {
      purrr::walk(flags$expression, function(e) code_expression(script, !!e))
    }

  }

  if (flags$command == "plot") {
    if (flags$tidyverse) {
      code_library(script, "tidyverse")
      code_library(script, "glue")
    } else {
      code_library(script, "ggplot2")
    }
    if (!is.null(flags$library)) {
      purrr::walk(flags$library, function(e) code_library(script, e))
    }

    if (rlang::is_null(flags$file) || flags$file == "-") {
      df_file <- expr(file("stdin", "rb", raw = TRUE))
    } else {
      df_file <- flags$file
    }

    read_expr <- expr(readr::read_delim(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
    if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
    code_expression(script, `<-`(df, !!read_expr))

    if (!is.null(flags$pre)) {
      purrr::walk(flags$pre, function(e) code_expression(script, !!e))
    }

    qplot_args <- purrr::compact(
      flags[union(methods::formalArgs(ggplot2::qplot),
                  c("adj", "alpha", "angle", "bg", "cex", "col", "color",
                    "colour", "fg", "fill", "group", "hjust", "label",
                    "linetype", "lower", "lty", "lwd", "max", "middle", "min",
                    "pch", "radius", "sample", "shape", "size", "srt", "upper",
                    "vjust", "weight", "x", "xend", "xmax", "xmin",
                    "xintercept", "y", "yend", "ymax", "ymin", "yintercept",
                    "z"))])
    qplot_args$data <- rlang::parse_expr("df")

    qplot_call <- rlang::call2("qplot", !!!qplot_args)
    qplot_call <- rlang::call_modify(qplot_call, !!!flags$aes, .homonyms = "last")

    if (!is.null(flags$post)) {
      qplot_call <- rlang::call2("<-", rlang::sym("p"), qplot_call)
    }

    code_expression(script, !!qplot_call)

    if (!is.null(flags$post)) {
      purrr::walk(flags$post, function(e) code_expression(script, !!e))
    }

  }

  close(script)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Output result
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (flags$dry_run) {
    code <- readLines(filename)
    if (has_tty) code <- prettycode::highlight(code)
    cat(code, sep = "\n")
  } else {

    if (!flags$verbose) {
      dev_null <- file(nullfile(), open = "w")
      sink(dev_null, type = "message")
      sink(dev_null, type = "output")
    } else {
      sink(stderr(), type = "message")
      sink(stderr(), type = "output")
    }

    # Run script and assign output to result
    e <- rlang::env()

    result <- source(filename, local = e, echo = flags$verbose,
                     spaced = FALSE, prompt.echo = "#> ",
                     continue.echo = "#+ ")$value
    sink()

    if (rlang::is_atomic(result)) {
      cli::cat_line(result)
    }

    if (rlang::is_bare_list(result)) {
      result <- tibble::enframe(result)
    }

    if (is.data.frame(result)) {
      if (has_tty && is.null(flags$output)) {
        options(tibble.width = flags$width %||% cli::console_width())
        print(tibble::as_tibble(result), n = flags$height)
      } else {
        con <- flags$output %||% stdout_binary()
        readr::write_delim(result, con, delim = flags$delim)
      }
    }

    if (ggplot2::is.ggplot(result)) {

      if (is.null(flags$output)) {
        flags$output <- ifelse(has_tty, "ansi", "png")
      }

      if (flags$output %in% c("ansi", "ascii")) {
        if (is.null(flags$width)) flags$width <- cli::console_width()
        if (requireNamespace("devoutansi", quietly = TRUE)) {
          devoutansi::ansi(width = flags$width,
                           height = flags$height,
                           plain_ascii = TRUE,
                           char_lookup_table = 2)
          p <- result +
            ggplot2::theme_minimal() +
            ggplot2::theme(panel.grid = ggplot2::element_blank())
          print(p)
          invisible(grDevices::dev.off())
        } else {
          cli::cat_line("Please specify --output, redirect to a file, or install {devoutansi}")
        }
      } else {
        if (fs::path_ext(flags$output) == "") {
          output_filename <- tempfile()
          on.exit(unlink(output_filename))
          device <- flags$output
          cat_output <- TRUE
        } else {
          output_filename  <- flags$output
          device <- NULL
          cat_output <- FALSE
        }

        if (is.null(flags$width)) flags$width <- 6
        if (is.null(flags$height)) flags$height <- 4

        ggplot2::ggsave(output_filename,
                        result,
                        device = device,
                        width = flags$width,
                        height = flags$height,
                        units = flags$units,
                        dpi = flags$dpi)

        if (cat_output) {
          contents <- readBin(output_filename, raw(), n = 1e8)
          writeBin(contents, stdout_binary())
        }
      }
    }
  }
}


