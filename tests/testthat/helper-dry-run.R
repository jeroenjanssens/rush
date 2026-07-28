capture_script <- function(fn, ...) {
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = "")
  on.exit(Sys.setenv(PATH = old_path))
  utils::capture.output(fn(..., dry_run = TRUE))
}
