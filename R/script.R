write_script <- function(con, ...) {
  writeLines(as.character(rlang::enexprs(...)), con)
}

filename <- function(con) {
  summary(con)$description
}

