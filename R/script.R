`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

code_expression <- function(con, ...) {
  writeLines(as.character(rlang::enexprs(...)), con)
}

code_library <- function(con, name) {
  writeLines(paste0("library(", name, ")"), con)
}

stdout_binary <- function() {
  if (.Platform$OS.type == "windows") {
    file("stdout", "wb", raw = TRUE)
  } else {
    file("/dev/stdout", "wb", raw = TRUE)
  }
}

