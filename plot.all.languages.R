#! /usr/bin/Rscript --vanilla

source('data-analysis-functions.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2 || length(args) > 3) {
  stop('Arguments: <modes file name> [language names rewrite prefix] <output directory>')
}
modes.filename <- args[1]
if (length(args) == 2) {
  names.rewrite.prefix <- NA
  output.dir <- args[2]
} else {
  names.rewrite.prefix <- args[2]
  output.dir <- args[3]
}

stopifnot(file.exists(modes.filename))
dir.create(output.dir, recursive=TRUE, showWarnings=FALSE)

modes <- read.simulation(filename=modes.filename, names.rewrite.prefix=names.rewrite.prefix)

for (lang in unique(modes$lang.name)) {
  cat('Plotting', lang, '\n')
  p <- plot.mode(lang, modes)
  ggsave(file.path(output.dir, paste(sep='.', lang, 'png')), width=10, height=3)
}