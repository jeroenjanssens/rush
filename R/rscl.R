
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @export
rscl <- function(...) {

  # parse flags -------------------------------------------------------------

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

  # create script -----------------------------------------------------------

  filename <- tempfile()
  script <- file(filename, open = "w")
  on.exit(unlink(filename))

  write_script(script, "#!/usr/bin/env Rscript")

  if (flags$command == "install") {
    write_script(script, pak::pkg_install(!!flags$package,
                                          upgrade = !!flags$upgrade))
  }

  if (flags$command == "run") {
    # Load libraries
    if (!is.null(flags$library)) {
      purrr::walk(flags$library, function(e) write_script(script, library(!!e)))
    }

    # Read files
    if (length(flags$file) == 1) {
      df_file <- flags$file
      if (df_file == "-") df_file <- expr(stdin())
      read_expr <- expr(vroom::vroom(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
      if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
      write_script(script, df <- !!read_expr)
    } else if (length(flags$file) > 1) {
      df_names <-
        ifelse(flags$file == "-", "stdin",
               fs::path_ext_remove(basename(flags$file))) %>%
        janitor::make_clean_names()

      write_script(script, dfs <- list())
      for (i in seq_along(df_names)) {
        df_name <- rlang::parse_expr(paste0("dfs$", df_names[[i]]))
        df_file <- flags$file[[i]]
        if (df_file == "-") df_file <- expr(stdin())

        read_expr <- expr(vroom::vroom(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
        if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
        write_script(script, !!df_name <- !!read_expr)
      }
    }

    # Add expressions
    if (!is.null(flags$expression)) {
      purrr::walk(flags$expression, function(e) write_script(script, !!e))
    }

  }

  if (flags$command == "qplot") {
    write_script(script, library(ggplot2))

    if (rlang::is_null(flags$file) || flags$file == "-") {
      df_file <- expr(stdin())
    } else {
      df_file <- flags$file
    }

    read_expr <- expr(vroom::vroom(!!df_file, delim = !!flags$delimiter, col_names = !!(!flags$no_header)))
    if (!flags$no_clean_names) read_expr <- expr(janitor::clean_names(!!read_expr))
    write_script(script, df <- !!read_expr)

    if (!is.null(flags$pre)) {
      purrr::walk(flags$pre, function(e) write_script(script, !!e))
    }

    qplot_args <- purrr::compact(
      flags[union(methods::formalArgs(ggplot2::qplot),
                  setdiff(ggplot2:::ggplot_global$all_aesthetics, c("width")))])
    qplot_args$data <- rlang:::expr(df)

    qplot_call <- rlang::call2("qplot", !!!qplot_args)
    qplot_call <- rlang::call_modify(qplot_call, !!!flags$aes, .homonyms = "last")



    if (!is.null(flags$post)) {
      qplot_call <- rlang::call2("<-", rlang::sym("p"), qplot_call)
    }

    write_script(script, !!qplot_call)

    if (!is.null(flags$post)) {
      purrr::walk(flags$post, function(e) write_script(script, !!e))
    }

  }

  close(script)

  # output ------------------------------------------------------------------

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

    if (rlang::is_list(result) &&
        !is.data.frame(result) &&
        !ggplot2::is.ggplot(result)) {
      result <- tibble::enframe(result)
    }

    if (is.data.frame(result)) {
      if (has_tty && is.null(flags$output)) {
        options(tibble.width = flags$width %||% cli::console_width())
        print(tibble::as_tibble(result), n = flags$height)
      } else {
        con <- flags$output %||% stdout()
        vroom::vroom_write(result, con, delim = ",")
      }
    }

    if (ggplot2::is.ggplot(result)) {

      if (is.null(flags$output)) {
        flags$output <- ifelse(has_tty, "ansi", "png")
      }

      if (flags$output %in% c("ansi", "ascii")) {
        if (is.null(flags$width)) flags$width <- cli::console_width()

        devoutansi::ansi(width = flags$width,
                         height = flags$height,
                         plain_ascii = flags$output == "ascii")
        p <- result +
          ggplot2::theme_minimal() +
          ggplot2::theme(panel.grid = ggplot2::element_blank())
        print(p)
        invisible(grDevices::dev.off())
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
          std_out <- file("/dev/stdout", "wb", raw = TRUE)
          writeBin(contents, std_out)
        }
      }
    }
  }
}


