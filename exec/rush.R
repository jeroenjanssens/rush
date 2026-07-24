#!/usr/bin/env Rscript
#| launcher:
#|   name: rush
#|   default-packages: [base, utils, rush]
status <- rush::rush(commandArgs(trailingOnly = TRUE))
quit(status = if (is.numeric(status)) status else 0L)
