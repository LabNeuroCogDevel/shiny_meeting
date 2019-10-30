#!/usr/bin/env Rscript

# windows L: is /Volumes/L elsewhere
bea_res <- function(...) normalizePath(
            file.path(
             ifelse(.Platform$OS.type == "windows", 'L:','/Volumes/L'),
             'bea_res',
             ...))
