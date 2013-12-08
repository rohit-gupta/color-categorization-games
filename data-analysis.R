source('data-analysis-functions.R')

if (!file.exists("wcs-modes.csv")) {

    cat("Reading data from WCS files...\n")
    wcs.chip <- read.table("WCS-Data-20110316/chip.txt", stringsAsFactors=FALSE, sep="\t", na.strings="",
                    col.names=c("wcs.index","wcs.value","wcs.hue","wcs.valuehue"))
                        
    wcs.lang <- read.table("WCS-Data-20110316/lang.txt", stringsAsFactors=FALSE, sep="\t", na.strings="")
    colnames(wcs.lang)[1:4] <- c("wcs.index","name","location","field.worker")
    
    wcs <- read.table("WCS-Data-20110316/term.txt", stringsAsFactors=FALSE, sep="\t", na.strings="",
               col.names=c("wcs.lang.index","speaker.index", "wcs.chip.index","term"))
               
    cat("Read", nrow(wcs), "datapoints.\n")
               
    cat("Marking dubious data points (\"?\" and \"*\") as <NA>...\n")
    wcs$term[wcs$term == "?"] <- NA
    wcs$term[wcs$term == "*"] <- NA

    cat("Calculating mode map for each language (may take a while)...\n")
    wcs.modes <- aggregate(wcs$term, by=list(wcs$wcs.chip.index, wcs$wcs.lang.index), FUN=mode.statistic)
    colnames(wcs.modes) <- c("wcs.chip.index", "wcs.lang.index", "term")

    wcs.modes$wcs.hue <- wcs.chip$wcs.hue[wcs.modes$wcs.chip.index]
    wcs.modes$wcs.value <- wcs.chip$wcs.value[wcs.modes$wcs.chip.index]
    wcs.modes$lang.name <- wcs.lang$name[wcs.modes$wcs.lang.index]
    
    wcs.modes$wcs.hue <- factor(as.numeric(wcs.modes$wcs.hue))
    wcs.modes$wcs.value <- factor(wcs.modes$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))
    
    cat("Saving mode map to file \"wcs-modes.csv\"...\n")
    write.csv(wcs.modes, file="wcs-modes.csv", row.names=FALSE, na="<NA>")
    
} else {

    cat("Loading data from file \"wcs-modes.csv\"...\n")
    wcs.modes <- read.csv("wcs-modes.csv", stringsAsFactors=FALSE, na.strings="<NA>")
    
    wcs.modes$wcs.hue <- factor(as.numeric(wcs.modes$wcs.hue))
    wcs.modes$wcs.value <- factor(wcs.modes$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))
    
}

if (!file.exists("wcs-wcs-match.Rdata")) {
    cat("Matching WCS languages with WCS languages (may take a while)...\n")
    wcs.wcs.match <- match.all.languages(wcs.modes, wcs.modes)
    save(wcs.wcs.match, file="wcs-wcs-match.Rdata")
} else {
    cat("Loading match for WCS languages with WCS languages from file \"wcs-wcs-match.Rdata\"...\n")
    load("wcs-wcs-match.Rdata")
}

cat("Reading all simulation results...\n")
sim3.modes <- read.simulation("results/simulations-3.csv")
sim4.modes <- read.simulation("results/simulations-4.csv")
sim5.modes <- read.simulation("results/simulations-5.csv")
sim6.modes <- read.simulation("results/simulations-6.csv")
sim7.modes <- read.simulation("results/simulations-7.csv")
sim8.modes <- read.simulation("results/simulations-8.csv")

sph3.modes <- read.simulation("results/sphere-3.csv")
sph4.modes <- read.simulation("results/sphere-4.csv")
sph5.modes <- read.simulation("results/sphere-5.csv")
sph6.modes <- read.simulation("results/sphere-6.csv")
sph7.modes <- read.simulation("results/sphere-7.csv")
sph8.modes <- read.simulation("results/sphere-8.csv")

rand3.modes <- read.simulation("results/random-3.csv")
rand4.modes <- read.simulation("results/random-4.csv")
rand5.modes <- read.simulation("results/random-5.csv")
rand6.modes <- read.simulation("results/random-6.csv")
rand7.modes <- read.simulation("results/random-7.csv")
rand8.modes <- read.simulation("results/random-8.csv")

cat("Matching simulation results with WCS languages...\n")
sim3.wcs.match <- match.all.languages(sim3.modes, wcs.modes)
sim4.wcs.match <- match.all.languages(sim4.modes, wcs.modes)
sim5.wcs.match <- match.all.languages(sim5.modes, wcs.modes)
sim6.wcs.match <- match.all.languages(sim6.modes, wcs.modes)
sim7.wcs.match <- match.all.languages(sim7.modes, wcs.modes)
sim8.wcs.match <- match.all.languages(sim8.modes, wcs.modes)

cat("Matching simulation results with perfect sphere with WCS languages...\n")
sph3.wcs.match <- match.all.languages(sph3.modes, wcs.modes)
sph4.wcs.match <- match.all.languages(sph4.modes, wcs.modes)
sph5.wcs.match <- match.all.languages(sph5.modes, wcs.modes)
sph6.wcs.match <- match.all.languages(sph6.modes, wcs.modes)
sph7.wcs.match <- match.all.languages(sph7.modes, wcs.modes)
sph8.wcs.match <- match.all.languages(sph8.modes, wcs.modes)

cat("Matching random results with WCS languages...\n")
rand3.wcs.match <- match.all.languages(rand3.modes, wcs.modes)
rand4.wcs.match <- match.all.languages(rand4.modes, wcs.modes)
rand5.wcs.match <- match.all.languages(rand5.modes, wcs.modes)
rand6.wcs.match <- match.all.languages(rand6.modes, wcs.modes)
rand7.wcs.match <- match.all.languages(rand7.modes, wcs.modes)
rand8.wcs.match <- match.all.languages(rand8.modes, wcs.modes)

cat("Calculating aggregated summaries...\n")
sim.wcs.match.summ <-
    rbind(summary.match(sim3.wcs.match$accuracy.table)
         ,summary.match(sim4.wcs.match$accuracy.table)
         ,summary.match(sim5.wcs.match$accuracy.table)
         ,summary.match(sim6.wcs.match$accuracy.table)
         ,summary.match(sim7.wcs.match$accuracy.table)
         ,summary.match(sim8.wcs.match$accuracy.table)
         )
rownames(sim.wcs.match.summ) <- 3:8
sph.wcs.match.summ <-
    rbind(summary.match(sph3.wcs.match$accuracy.table)
         ,summary.match(sph4.wcs.match$accuracy.table)
         ,summary.match(sph5.wcs.match$accuracy.table)
         ,summary.match(sph6.wcs.match$accuracy.table)
         ,summary.match(sph7.wcs.match$accuracy.table)
         ,summary.match(sph8.wcs.match$accuracy.table)
         )
rownames(sph.wcs.match.summ) <- 3:8
rand.wcs.match.summ <-
    rbind(summary.match(rand3.wcs.match$accuracy.table)
         ,summary.match(rand4.wcs.match$accuracy.table)
         ,summary.match(rand5.wcs.match$accuracy.table)
         ,summary.match(rand6.wcs.match$accuracy.table)
         ,summary.match(rand7.wcs.match$accuracy.table)
         ,summary.match(rand8.wcs.match$accuracy.table)
         )
rownames(rand.wcs.match.summ) <- 3:8
wcs.wcs.match.summ <-
    rbind(summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 3))
         ,summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 4))
         ,summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 5))
         ,summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 6))
         ,summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 7))
         ,summary.match(filter.wcs.match(wcs.wcs.match$accuracy.table, 8))
         )
rownames(wcs.wcs.match.summ) <- 3:8
