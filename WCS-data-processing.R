#! /usr/bin/Rscript --vanilla

source('data-analysis-functions.R')

wcs.data.dir <- 'WCS-Data-20110316'
wcs.selection.filename <- 'data/WCS_pruned.tsv'
agreement.level <- 0.5
output.filename <- 'WCS-modes.csv'

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
  if (length(args) != 4) {
    stop('Arguments: <WCS data dir> <WCS selection file name> <agreement level> <output file name>')
  }
  wcs.data.dir <- args[1]
  wcs.selection.filename <- args[2]
  agreement.level <- as.double(args[3])
  output.filename <- args[4]
}

stopifnot(file.exists(wcs.data.dir))
stopifnot(file.exists(wcs.selection.filename))
stopifnot(is.double(agreement.level))
stopifnot(file.exists(output.filename))

cat("Reading data from WCS files in directory", wcs.data.dir, "\n")
wcs.chip <- read.table(file.path(wcs.data.dir, "chip.txt"), stringsAsFactors=FALSE, sep="\t", na.strings="",
                        col.names=c("wcs.index","wcs.value","wcs.hue","wcs.valuehue"))

wcs.lang <- read.table(file.path(wcs.data.dir, "lang.txt"), stringsAsFactors=FALSE, sep="\t", na.strings="")
colnames(wcs.lang)[1:4] <- c("wcs.index","name","location","field.worker")

wcs.selection <- read.table(wcs.selection.filename, sep='\t',
                            col.names=c("wcs.lang.index","wcs.lang.name","include","comment"))

wcs <- read.table(file.path(wcs.data.dir, "term.txt"), stringsAsFactors=FALSE, sep="\t", na.strings="",
                   col.names=c("wcs.lang.index","speaker.index", "wcs.chip.index","term"))

cat("Read", nrow(wcs), "datapoints.\n")

cat("Excluding data for", length(which(wcs.selection$include==0)),
    "languages as specified in file", wcs.selection.filename, "\n")
wcs <- wcs[wcs$wcs.lang.index %in% wcs.selection$wcs.lang.index[wcs.selection$include == 1],]

cat("Marking dubious data points ('?' and '*') as <NA>\n")
wcs$term[wcs$term == "?"] <- NA
wcs$term[wcs$term == "*"] <- NA

cat("Calculating mode map for each language with agreement level", agreement.level, "(may take a while)...\n")
wcs.modes <- aggregate(wcs$term, by=list(wcs$wcs.chip.index, wcs$wcs.lang.index),
                        FUN=function(x) { agreement(x, min.level=agreement.level) }) 
colnames(wcs.modes) <- c("wcs.chip.index", "wcs.lang.index", "term")

wcs.modes$wcs.hue <- wcs.chip$wcs.hue[wcs.modes$wcs.chip.index]
wcs.modes$wcs.value <- wcs.chip$wcs.value[wcs.modes$wcs.chip.index]
wcs.modes$lang.name <- wcs.lang$name[wcs.modes$wcs.lang.index]

wcs.modes$wcs.hue <- factor(as.numeric(wcs.modes$wcs.hue))
wcs.modes$wcs.value <- factor(wcs.modes$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))

cat("Writing results to file", output.filename, "\n")
write.table(wcs.modes[c('lang.name','wcs.hue','wcs.value','term')], sep=',',
          file=output.filename, row.names=FALSE, col.names=FALSE)
