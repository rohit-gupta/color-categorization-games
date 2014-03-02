source('data-analysis-functions.R')
library(apcluster)

agreement.level <- 0.5

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
wcs.modes <- aggregate(wcs$term, by=list(wcs$wcs.chip.index, wcs$wcs.lang.index),
                       FUN=function(x) { agreement(x, min.level=agreement.level) }) 
colnames(wcs.modes) <- c("wcs.chip.index", "wcs.lang.index", "term")

wcs.modes$wcs.hue <- wcs.chip$wcs.hue[wcs.modes$wcs.chip.index]
wcs.modes$wcs.value <- wcs.chip$wcs.value[wcs.modes$wcs.chip.index]
wcs.modes$lang.name <- wcs.lang$name[wcs.modes$wcs.lang.index]

wcs.modes$wcs.hue <- factor(as.numeric(wcs.modes$wcs.hue))
wcs.modes$wcs.value <- factor(wcs.modes$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))

cat("Saving mode map to file \"wcs-modes.csv\"...\n")
write.csv(wcs.modes, file="wcs-modes.csv", row.names=FALSE, na="<NA>")

cat("Matching WCS languages with WCS languages (may take a while)...\n")
wcs.wcs.match <- match.all.languages.m(wcs.modes, wcs.modes)
save(wcs.wcs.match, file="wcs-wcs-match.Rdata")

cat("Reading all simulation results...\n")
sim.modes <- data.frame()
sim.modes <- rbind(sim.modes, read.simulation("results/Regier3.csv", names.prefix='RKK3.'))
sim.modes <- rbind(sim.modes, read.simulation("results/Regier4.csv", names.prefix='RKK4.'))
sim.modes <- rbind(sim.modes, read.simulation("results/Regier5.csv", names.prefix='RKK5.'))
sim.modes <- rbind(sim.modes, read.simulation("results/Regier6.csv", names.prefix='RKK6.'))

sim.modes <- rbind(sim.modes, read.simulation("results/simulations-3.csv", names.prefix='CO3.'))
sim.modes <- rbind(sim.modes, read.simulation("results/simulations-4.csv", names.prefix='CO4.'))
sim.modes <- rbind(sim.modes, read.simulation("results/simulations-5.csv", names.prefix='CO5.'))
sim.modes <- rbind(sim.modes, read.simulation("results/simulations-6.csv", names.prefix='CO6.'))
sim.modes <- rbind(sim.modes, read.simulation("results/simulations-7.csv", names.prefix='CO7.'))
sim.modes <- rbind(sim.modes, read.simulation("results/simulations-8.csv", names.prefix='CO8.'))

cat("Matching simulation results with WCS languages (may take a while)...\n")
sim.wcs.match <- match.all.languages.m(sim.modes, wcs.modes)

cat("Calculating clustering of WCS languages...\n")
clustering <- apcluster(s=wcs.wcs.match)
clustering.eval <- evaluate.clustering(clustering=clustering, similarity.matrix=wcs.wcs.match)
clustering.summary <- summary.similarities(list(clustering.eval))

cat("Evaluating simulations against clustering...\n")
simulations.eval <- list()
for(sim in 1:nrow(sim.wcs.match)) {
  simulations.eval[[row.names(sim.wcs.match)[sim]]] <- evaluate.language(clustering=clustering, similarity.row=sim.wcs.match[sim,])
}

simulations.summary <- summary.similarities(simulations.eval)