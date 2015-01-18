#! /usr/bin/Rscript --vanilla

source('data-analysis-functions.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('Arguments: <modes file name> <language names rewrite prefix>')
}
modes.filename <- args[1]
names.rewrite.prefix <- args[2]

stopifnot(file.exists(modes.filename))

modes <- read.simulation(filename=modes.filename, names.rewrite.prefix=names.rewrite.prefix)

write.table(modes, file=modes.filename, row.names=FALSE, col.names=FALSE, sep=',', dec='.', qmethod='double')