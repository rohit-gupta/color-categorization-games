library(ggplot2)

mode.statistic <- function(x) {
  z <- table(as.vector(x))
  if (length(z) == 0) {
    return(NA)
  } else {
    return(names(z)[z == max(z)][1])
  }
}

agreement <- function(x, min.level) {
  z <- table(as.vector(x))
  if (length(z) == 0) {
    return(NA)
  } else {
    mode <- names(z)[z == max(z)][1]
    if (z[mode]/sum(z) > min.level)
      return(mode)
    else
      return(NA)
  }
}

read.simulation <- function(filename, names.rewrite.prefix=NA) {
  tmp <- read.csv(filename, header=FALSE, stringsAsFactors=FALSE,
                  col.names=c("lang.name","wcs.hue","wcs.value","term"))
  tmp$wcs.hue <- factor(as.numeric(tmp$wcs.hue))
  tmp$wcs.value <- factor(tmp$wcs.value, levels=c("J","I","H","G","F","E","D","C","B","A"))
  
  if (!is.na(names.rewrite.prefix)) {
    lang.names <- factor(tmp$lang.name)
    if (length(unique(lang.names)) > 1) {
        tmp$lang.name <- paste(sep='', names.rewrite.prefix, as.character(as.numeric(lang.names)))
    } else {
        tmp$lang.name <- names.rewrite.prefix
    }
  }
  
  return(tmp)
}

get.mode <- function(lang.name, modes=wcs.modes) {
  return(modes[modes$lang.name == lang.name,])
}

plot.mode <- function(lang.name, modes=wcs.modes, ...) {
  tmp <- modes[modes$lang.name == lang.name,]
  tmp <- tmp[order(tmp$wcs.value, tmp$wcs.hue),]
  tmp$term <- factor(tmp$term, levels=unique(tmp$term))
  ggplot(na.omit(tmp), aes(x=wcs.hue, y=wcs.value)) + geom_tile(aes(fill=term), colour="black") +
    scale_fill_grey(start=0.1,end=0.9) +
    theme_minimal() + coord_fixed() + labs(title=lang.name, x='', y='') +
    theme(legend.position = "none", panel.grid.major=element_blank(), axis.ticks=element_blank()) + xlim(c("0","",as.character(1:40)))
}

plot.all.modes <- function(lang.names, modes=wcs.modes, ...) {
  require(gridExtra)
  plots <- list()
  for (i in 1:length(lang.names)) {
    plots[[i]] <- plot.mode(lang.names[i], modes)
  }
  do.call('grid.arrange', plots)
}

classify.languages <- function(modes=wcs.modes, chips.threshold=16) {
  chips.per.term <- aggregate(modes$term, by=list(modes$term, modes$lang.name), length)
  terms.per.lang <- aggregate(chips.per.term$x, by=list(chips.per.term[,2]), function(x){length(x[x>=chips.threshold])})
  colnames(terms.per.lang) <- c("lang.name", "num.terms")
  return(terms.per.lang)
}

normalized.mutual.information <- function(x, y, adjusted=FALSE) {
  require(entropy)
  require(gmp)
  
  expected <- 0
  
  if (adjusted) {
    n <- length(x)
    nr <- length(na.omit(unique(x)))
    nc <- length(na.omit(unique(y)))
    ct <- table(x, y)
    a <- rowSums(ct)
    b <- colSums(ct)
    for (i in 1:nr) {
      for (j in 1:nc) {
        for (nij in max(a[i]+b[j]-n,1):min(a[i],b[j])) {
           tmp <- 1
           tmp <- tmp * (nij/n)
           tmp <- tmp * (log(n*nij/(a[[i]]*b[[j]])))
           tmp <- tmp * ((factorialZ(a[[i]]) * factorialZ(b[[j]]))/factorialZ(n))
           tmp <- tmp * (1 / (factorialZ(nij) * factorialZ(a[[i]]-nij)))
           tmp <- tmp * (factorialZ(n-a[[i]]) / factorialZ(b[[j]]-nij))
           tmp <- tmp * (factorialZ(n-b[[j]]) / factorialZ(n-a[[i]]-b[[j]]+nij))
           expected <- expected + as.numeric(tmp)
        }
      }
    }
  }
  
  nmi <- (mi.plugin(table(x, y)) - expected) / (max(entropy(table(x)), entropy(table(y))) - expected)
  
  return(nmi)
}

imputate <- function(x, method='random') {
  values <- unique(na.omit(x))
  x[is.na(x)] <- sample(values, length(x[is.na(x)]), replace=TRUE)
  return(x)
}

match.languages <- function(lang1.name, lang2.name, modes1=wcs.modes, modes2=wcs.modes, metric=c('NMI','AMI','ARI'), na.imputate=FALSE) {
  require(entropy)
  require(mclust)

  metric <- match.arg(metric)
  
  aligned <- merge(modes1[modes1$lang.name==lang1.name,], modes2[modes2$lang.name==lang2.name,], by=c("wcs.hue","wcs.value"))
  terms1 <- aligned$term.x
  terms2 <- aligned$term.y
  
  metric.function <- switch(metric,
                            NMI = function(x,y) { normalized.mutual.information(x,y,adjusted=FALSE) },
                            AMI = function(x,y) { normalized.mutual.information(x,y,adjusted=TRUE) },
                            ARI = function(x,y) { adjustedRandIndex(x,y) })
  
  if (na.imputate) {
    similarities <- c()
    for (i in 1:100) {
      similarities <- c(similarities, metric.function(imputate(terms1), imputate(terms2)))
    }
    similarity <- median(similarities)        
  } else {
    similarity <- metric.function(terms1, terms2)
  }
  
  return(similarity)
}

match.all.languages <- function(modes1, modes2, metric=c('NMI','AMI','ARI'), na.imputate=TRUE) {
  langs1 <- unique(modes1$lang.name)
  langs2 <- unique(modes2$lang.name)
  
  similarity.table <- matrix(ncol=length(langs2), nrow=length(langs1), dimnames=list(langs1, langs2))
  for (lang1 in langs1) {
    for (lang2 in langs2) {
      cat('Calculating similarity between', lang1, 'and', lang2, '\n')
      similarity.table[lang1, lang2] <- match.languages(lang1, lang2, modes1, modes2, metric=metric, na.imputate=na.imputate)
    }
  }
  
  return(similarity.table)
}

library(memoise)
match.all.languages.m <- memoise(match.all.languages)

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

plot.scenario.similarities <- function(similarity.table, scenario.prefix, number.of.terms, additional.prefix='RKK') {
  tmppp <- as.list(as.data.frame(t(similarity.table)))
  tmp <- data.frame(Language.name=c(), Similarity=c())
  for(lang in names(tmppp)) {
    tmp <- rbind(tmp, data.frame(Language.name=replicate(n=length(tmppp[[lang]]),expr=lang)
                                 ,Similarity=tmppp[[lang]]))
  }
  p <- ggplot(tmp[grep(pattern=paste(sep='', scenario.prefix, number.of.terms, '|', additional.prefix, number.of.terms)
                       ,x=tmp$Language.name),]
              ,aes(x=Language.name, y=Similarity))
#   p <- p + geom_jitter(color='grey')
  p <- p + geom_boxplot()#outlier.colour=NA)
  p <- p + coord_flip() + theme_bw() + xlab('')
  return(p)
}

plot.sim.in.context <- function(wcs.match, wcs.names, sim.match, sim.names) {
  melted.wcs.match <- melt(wcs.match)
  melted.wcs.match <- melted.wcs.match[melted.wcs.match$Var1 %in% wcs.names,]
  melted.wcs.match$origin <- 'WCS'
  
  melted.sim.match <- melt(t(sim.match))
  melted.sim.match <- melted.sim.match[(melted.sim.match$Var1 %in% wcs.names) &
                                       (melted.sim.match$Var2 %in% sim.names),]
  melted.sim.match$origin <- melted.sim.match$Var2
  
  melted.all.match <- rbind(melted.sim.match, melted.wcs.match)
  melted.all.match$origin <- factor(melted.all.match$origin, levels=c('WCS', unique(as.character(melted.sim.match$origin))))
  
  p <- ggplot(melted.all.match, aes(Var1, value, alpha=origin, shape=origin))
  p <- p + geom_point() + coord_flip() + xlab('') + ylab('Similarity')
  p <- p + theme_bw()
  return(p)
  
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

evaluate.clustering <- function(clustering, similarity.matrix) {
  similarities.per.cluster <- list()
  for (cluster in 1:length(clustering@clusters)) {
    similarities.per.cluster[[cluster]] <- vector()
    for (l1 in clustering[[cluster]]) {
      for (l2 in clustering[[cluster]]) {
        if (l2 > l1) {
          similarities.per.cluster[[cluster]] <- c(similarities.per.cluster[[cluster]], similarity.matrix[l1,l2])
        }
      }
    }
  }
  return(similarities.per.cluster)
}

evaluate.language <- function(clustering, similarity.row) {
  similarities.per.cluster <- list()
  for (cluster in 1:length(clustering@clusters)) {
    similarities.per.cluster[[cluster]] <- similarity.row[clustering[[cluster]]]
  }
  return(similarities.per.cluster)
}

summary.similarities <- function(similarities.list, aggregation=median) {
  if(!all(sapply(similarities.list, length) == sapply(similarities.list, length)[1]))
    stop('All instances in the similarity list should have the same length.')
  summ <- t(sapply(similarities.list, function(x) { sapply(x, median)}))
  colnames(summ) <- names(similarities.list[[1]])
  return(summ)
}