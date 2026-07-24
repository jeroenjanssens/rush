#!/usr/bin/env Rscript
#| launcher:
#|   name: rush
#|   default-packages: [base, utils, rush]
# Show rush's own usage errors as a plain message, without an R backtrace,
# the way a command-line tool should.
options(rlang_backtrace_on_error = "none")
status <- rush::rush(commandArgs(trailingOnly = TRUE))
quit(status = if (is.numeric(status)) status else 0L)
