library(ggplot2)

mode.statistic <- function(x) {
    z <- table(as.vector(x))
    if (length(z) == 0) {
        return(NA)
    } else {
        return(names(z)[z == max(z)][1])
    }
}

read.simulation <- function(filename, simplify.names=TRUE) {
    tmp <- read.csv(filename, header=FALSE, stringsAsFactors=FALSE,
               col.names=c("lang.name","wcs.hue","wcs.value","term"))
    tmp$wcs.hue <- factor(as.numeric(tmp$wcs.hue))
    tmp$wcs.value <- factor(tmp$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))
    
    if (simplify.names) {
        tmp$lang.name <- as.character(as.numeric(factor(tmp$lang.name)))
    }
    
    return(tmp)
}

plot.mode <- function(lang.name, modes=wcs.modes, ...) {
    tmp <- modes[modes$lang.name == lang.name,]
    tmp <- tmp[order(tmp$wcs.value, tmp$wcs.hue),]
    tmp$term <- factor(tmp$term, levels=unique(tmp$term))
    ggplot(tmp, aes(x=wcs.hue, y=wcs.value)) + geom_tile(aes(fill=term), colour="black") + xlab("") + ylab("") +
        scale_fill_grey(start=0.1,end=1) +
        theme_minimal() + coord_fixed() + 
        theme(legend.position = "none", panel.grid.major=element_blank(), axis.ticks=element_blank()) + xlim(c("0","",as.character(1:40)))
}

classify.languages <- function(modes=wcs.modes, chips.threshold=16) {
    chips.per.term <- aggregate(modes$term, by=list(modes$term, modes$lang.name), length)
    terms.per.lang <- aggregate(chips.per.term$x, by=list(chips.per.term[,2]), function(x){length(x[x>=chips.threshold])})
    colnames(terms.per.lang) <- c("lang.name", "num.terms")
    return(terms.per.lang)
}

match.languages <- function(lang1.name, lang2.name, modes1=wcs.modes, modes2=wcs.modes) {
    require(entropy)
    
    aligned <- merge(modes1[modes1$lang.name==lang1.name,], modes2[modes2$lang.name==lang2.name,], by=c("wcs.hue","wcs.value"))
    terms1 <- aligned$term.x
    terms2 <- aligned$term.y
    
    confusion.table <- table(terms1, terms2)

    similarity <- mi.plugin(confusion.table) / max(entropy(table(terms1)), entropy(table(terms2)))
        
    return(similarity)
}

match.all.languages <- function(modes1, modes2) {
    langs1 <- unique(modes1$lang.name)
    langs2 <- unique(modes2$lang.name)
    similarity.table <- matrix(ncol=length(langs2), nrow=length(langs1), dimnames=list(langs1, langs2))
    for (lang1 in langs1) {
        for (lang2 in langs2) {
            cat('Calculating similarity between', lang1, 'and', lang2, '\n')
            similarity.table[lang1, lang2] <- match.languages(lang1, lang2, modes1, modes2)
        }
    }
    
    return(similarity.table)
}

summary.match <- function(accuracy.table, per.row=FALSE) {
    accuracy.table <- as.data.frame(accuracy.table)
    filtered <- accuracy.table[,!sapply(accuracy.table, function(y){all(is.na(y))})]
    if (per.row) {
        summ <- as.data.frame(t(sapply(as.data.frame(t(filtered)), summary)))
    } else {
        summ <- as.data.frame(t(fivenum(t(filtered))))
        colnames(summ) <- c("Min.","1st Qu.","Median","3rd Qu.","Max.")
        summ$Mean <- mean(t(filtered), na.rm=TRUE)
        summ <- summ[,c(1:3,6,4:5)]
    }
    return(summ)
}

filter.wcs.match <- function(accuracy.table, num.terms) {
    terms.count <- classify.languages(wcs.modes)
    langs <- terms.count$lang.name[terms.count$num.terms == num.terms]
    filtered <- accuracy.table[langs, langs]
    for (i in 1:nrow(filtered)) {
        filtered[i,i] <- NA
    }
    return(filtered)
}
