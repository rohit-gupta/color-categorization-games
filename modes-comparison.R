#! /usr/bin/Rscript --vanilla

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop('Arguments: <first modes file name> <second modes file name> <imputate NA\'s (TRUE | FALSE)> <output file name>')
}
first.modes.filename <- args[1]
second.modes.filename <- args[2]
na.imputate <- as.logical(args[3])
output.filename <- args[4]

stopifnot(file.exists(first.modes.filename))
stopifnot(file.exists(second.modes.filename))
stopifnot(!is.na(na.imputate))

source('data-analysis-functions.R')

first.modes <- read.simulation(first.modes.filename)
second.modes <- read.simulation(second.modes.filename)

match <- match.all.languages(first.modes, second.modes, metric='ARI', na.imputate=na.imputate)

write.csv(match, file=output.filename)