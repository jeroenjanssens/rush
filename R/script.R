`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

code_expression <- function(con, ...) {
  writeLines(as.character(rlang::enexprs(...)), con)
}

code_library <- function(con, name) {
  writeLines(paste0("library(", name, ")"), con)
}

# Emit a list of expressions, assigning the value of the last one to `result`
# so the dispatch block at the end of the script knows what to print or save.
emit_result_exprs <- function(con, exprs) {
  n <- length(exprs)
  if (n > 1) {
    purrr::walk(exprs[-n], function(e) code_expression(con, !!e))
  }
  last <- rlang::call2("<-", rlang::sym("result"), exprs[[n]])
  code_expression(con, !!last)
}

# The `#| packages:` frontmatter that turns the generated script into a
# self-describing `ir` script. `ir run` resolves these refs with pak before
# executing the script.
frontmatter <- function(packages) {
  c(
    "#!/usr/bin/env -S ir run",
    "#| packages:",
    paste0("#|   - ", packages)
  )
}

# A minimal, dependency-free version of janitor::make_clean_names, used only
# to turn multiple input file names into distinct data-frame names.
clean_names_simple <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

# Bake the runtime context that the dispatch block needs into an R list
# literal, so the generated script stays self-contained.
script_preamble <- function(flags) {
  ctx <- list(
    output    = flags$output,
    width     = flags$width,
    height    = flags$height,
    units     = flags$units,
    dpi       = flags$dpi,
    delimiter = flags$delimiter,
    has_post  = !is.null(flags$post)
  )
  lines <- vapply(names(ctx), function(nm) {
    paste0("  ", nm, " = ", paste(deparse(ctx[[nm]]), collapse = ""))
  }, character(1))
  c(".rush <- list(",
    paste0(lines, c(rep(",", length(lines) - 1L), "")),
    ")")
}

# The output-dispatch code appended to every generated script. It runs inside
# the `ir run` subprocess, which inherits the parent's stdio, so TTY detection
# and binary output behave exactly as if rush had produced the output itself.
dispatch_block <- function(command) {
  if (command == "plot") dispatch_plot() else dispatch_run()
}

dispatch_run <- function() {
'#~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.has_tty <- isatty(stdout())
.stdout_binary <- function() {
  if (.Platform$OS.type == "windows") file("stdout", "wb", raw = TRUE)
  else file("/dev/stdout", "wb", raw = TRUE)
}
if (.has_tty) options(width = if (is.null(.rush$width)) cli::console_width() else .rush$width)

if (rlang::is_atomic(result)) {
  cli::cat_line(result)
} else if (rlang::is_bare_list(result)) {
  result <- tibble::enframe(result)
}

if (is.data.frame(result)) {
  if (.has_tty && is.null(.rush$output)) {
    options(tibble.width = if (is.null(.rush$width)) cli::console_width() else .rush$width)
    print(tibble::as_tibble(result), n = .rush$height)
  } else {
    con <- if (is.null(.rush$output)) .stdout_binary() else .rush$output
    readr::write_delim(result, con, delim = .rush$delimiter)
  }
}
'
}

dispatch_plot <- function() {
'#~~~ Output dispatch (added by rush) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.has_tty <- isatty(stdout())
.stdout_binary <- function() {
  if (.Platform$OS.type == "windows") file("stdout", "wb", raw = TRUE)
  else file("/dev/stdout", "wb", raw = TRUE)
}

out <- .rush$output
w <- .rush$width
h <- .rush$height
if (is.null(out)) out <- if (.has_tty) "ansi" else "png"

if (out %in% c("ansi", "ascii")) {
  if (is.null(w)) w <- cli::console_width()
  devoutansi::ansi(width = w, height = h, plain_ascii = TRUE, char_lookup_table = 2)
  if (!.rush$has_post) {
    result <- result +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  print(result)
  invisible(grDevices::dev.off())
} else {
  if (fs::path_ext(out) == "") {
    output_filename <- tempfile()
    device <- out
    cat_output <- TRUE
  } else {
    output_filename <- out
    device <- NULL
    cat_output <- FALSE
  }
  if (is.null(w)) w <- 6
  if (is.null(h)) h <- 4
  ggplot2::ggsave(output_filename, result, device = device,
                  width = w, height = h, units = .rush$units, dpi = .rush$dpi)
  if (cat_output) {
    contents <- readBin(output_filename, raw(), n = 1e8)
    writeBin(contents, .stdout_binary())
  }
}
'
}
