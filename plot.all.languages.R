#! /usr/bin/Rscript --vanilla

source('data-analysis-functions.R')

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('Arguments: <modes file name> <output directory>')
}
modes.filename <- args[1]
output.dir <- args[2]

stopifnot(file.exists(modes.filename))
dir.create(output.dir, recursive=TRUE, showWarnings=FALSE)

modes <- read.simulation(filename=modes.filename)

for (lang in unique(modes$lang.name)) {
  cat('Plotting', lang, '\n')
  p <- plot.mode(lang, modes)
  ggsave(file.path(output.dir, paste(sep='.', lang, 'png')), width=10, height=3)
}