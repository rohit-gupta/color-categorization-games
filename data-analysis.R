library(ggplot2)

source('data-analysis-functions.R')

nterms <- 4

digits.rounding <- 3
match.files <- c(paste(sep='-', 'results/Munsell', nterms, 'WCS.csv'),
                 paste(sep='-', 'results/spectrum-10-CIELAB-Daylight-6500K-flat', nterms, 'WCS.csv'),
                 paste(sep='-', 'results/spectrum-10-CIELAB-Daylight-6500K', nterms, 'WCS.csv'))
rkk.match.file <- paste(sep='', 'results/Regier', nterms, '-WCS.csv')

max.similarities <- data.frame(lang.name=character(), 
                               model.name=character(), 
                               max.name=character(),
                               max.value=numeric())
for (match.file in match.files) {
  match <- read.csv(match.file, row.names=1)
  max.cols <- max.col(match, ties.method='first')
  for (row in 1:nrow(match)) {
    lang.name <- row.names(match)[row]
    model.name <- strsplit(lang.name, '-')[[1]][1]
    max.name <- colnames(match)[max.cols[row]]
    max.value <- round(match[row, max.cols[row]], digits=digits.rounding)
    max.similarities <- rbind(max.similarities,
                              data.frame(lang.name, model.name, max.name, max.value))
  }
}

rkk.match <- read.csv(rkk.match.file, row.names=1)
rkk.max.cols <- max.col(rkk.match, ties.method='first')
rkk.max.value <- round(rkk.match[1, rkk.max.cols[1]], digits=digits.rounding)

theme_set(theme_minimal())

# ggplot(max.similarities, aes(x=max.value)) + geom_bar(position='dodge',aes(fill=model.name)) + geom_density(aes(linetype=model.name)) + scale_fill_grey(start = 0, end = .8)
print(ggplot(max.similarities, aes(x=max.value)) + 
        geom_bar() + geom_density() + facet_grid(model.name ~ .) +
        geom_vline(xintercept=rkk.max.value, linetype='longdash') +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        labs(title=paste(nterms, 'terms')) + xlim(0.1,0.55))
#ggplot(max.similarities, aes(x=model.name, y=max.value)) + geom_boxplot() + coord_flip()

print(wilcox.test(subset(max.similarities, model.name == 'COM1')$max.value,
                  subset(max.similarities, model.name == 'COM2')$max.value,
                  paired=FALSE, conf.int=TRUE, exact=FALSE))
print(wilcox.test(subset(max.similarities, model.name == 'COM1')$max.value,
                  subset(max.similarities, model.name == 'COM3')$max.value,
                  paired=FALSE, conf.int=TRUE, exact=FALSE))
print(wilcox.test(subset(max.similarities, model.name == 'COM2')$max.value,
                  subset(max.similarities, model.name == 'COM3')$max.value,
                  paired=FALSE, conf.int=TRUE, exact=FALSE))