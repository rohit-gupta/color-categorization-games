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
    aligned <- merge(modes1[modes1$lang.name==lang1.name,], modes2[modes2$lang.name==lang2.name,], by=c("wcs.hue","wcs.value"))
    terms1 <- aligned$term.x
    terms2 <- aligned$term.y
    
    confusion.table <- table(terms1, terms2)
    
    match <- list()
    match$terms <- data.frame()
    tmp <- as.data.frame(confusion.table)
    colnames(tmp) <- c(lang1.name, lang2.name, 'num.matches')
    while (nrow(tmp) > 0 && max(tmp$num.matches) > 0) {
        t1data <- tmp[tmp$num.matches == max(tmp$num.matches), ][1, ]
        
        t1 <- as.character(t1data[,lang1.name])
        t2 <- as.character(t1data[,lang2.name])
        t1data$precision <- t1data$num.matches / sum(confusion.table[t1, ])
        t1data$recall <- t1data$num.matches / sum(confusion.table[ ,t2])
        t1data$f.measure <- 2 * ((t1data$precision * t1data$recall) / (t1data$precision + t1data$recall))
        
        match$terms <- rbind(match$terms, t1data)
        
        tmp <- tmp[tmp[,lang1.name] != t1 & tmp[,lang2.name] != t2, ]
    }

    match$accuracy <- sum(match$terms$num.matches) / sum(confusion.table)

    match$unmatched.count <- 0
    unmatched1 <- rownames(confusion.table)[!(rownames(confusion.table) %in% match$terms[,lang1.name])]
    unmatched2 <- colnames(confusion.table)[!(colnames(confusion.table) %in% match$terms[,lang2.name])]
    if (length(unmatched1) != 0 || length(unmatched2) != 0) {
        match$unmatched <- data.frame()
        if (length(unmatched1) != 0) {
            for (t1 in unmatched1) {
                t1data <- list()
                t1data$term <- t1
                t1data$occurrences <- sum(confusion.table[t1,])

                match$unmatched <- rbind(match$unmatched, data.frame(t1data))
            }
        }
        if (length(unmatched2) != 0) {
            for (t2 in unmatched2) {
                t2data <- list()
                t2data$term <- t2
                t2data$occurrences <- sum(confusion.table[,t2])

                match$unmatched <- rbind(match$unmatched, data.frame(t2data))
            }
        }
        match$unmatched.count <- sum(match$unmatched$occurrences)
        warning(match$unmatched.count, " unmatched data points.")
    }
        
    return(match)
}

match.all.languages <- function(modes1, modes2, unmatched.threshold1=16, unmatched.threshold2=16) {
    match <- list()
    
    term.count1 <- classify.languages(modes1, unmatched.threshold1)
    term.count2 <- classify.languages(modes2, unmatched.threshold2)
    for (lang1 in term.count1$lang.name) {
        cat("Calculating matches for", lang1, "against:\n")
        nterms <- term.count1[term.count1$lang.name == lang1, "num.terms"]
        match[[lang1]] <- list()
        modes2.filtered <- modes2[modes2$lang.name %in% term.count2[term.count2$num.terms == nterms, "lang.name"],]
        for (lang2 in unique(modes2.filtered$lang.name)) {
            cat("\t", lang2)
            tmp <- match.languages(lang1, lang2, modes1, modes2)
            match[[lang1]][[lang2]] <- tmp
            cat(" -", tmp$accuracy, "\n")
        }
    }
    
    langs1 <- unique(modes1$lang.name)
    langs2 <- unique(modes2$lang.name)
    match$accuracy.table <- as.data.frame(replicate(length(langs2), replicate(length(langs1), NA)), row.names=langs1)
    #colnames(match$accuracy.table) <- langs2
    for (lang1 in langs1) {
        for (lang2 in langs2) {
            tmp <- match[[lang1]][[lang2]]
            if (!is.null(tmp)) {
                match$accuracy.table[lang1, lang2] <- tmp$accuracy
            }
        }
    }
    
    return(match)
}

summary.match <- function(accuracy.table, per.row=FALSE) {
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
