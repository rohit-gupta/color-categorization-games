read.image <- function(filename) {
  require(tiff)
  require(colorspace)
  img <- readTIFF(filename)
  img.rgb <- RGB(as.vector(img[,,1]), as.vector(img[,,2]), as.vector(img[,,3]))
  img.lab <- as(img.rgb, 'LAB')
  colnames(img.lab@coords) <- c('L','a','b')
  return(img.lab@coords)
}

freqs.calculation <- function(space, Lab.coords) {
  options(warn=-1)
  L.breaks <- c(min(space$L) - 1, sort(unique(space$L)) + diff(sort(unique(space$L)))/2)
  L.binned <- cut(Lab.coords[,'L'], breaks=L.breaks, labels=sort(unique(space$L)))

  a.breaks <- c(min(space$a) - 1, sort(unique(space$a)) + diff(sort(unique(space$a)))/2)
  a.binned <- cut(Lab.coords[,'a'], breaks=a.breaks, labels=sort(unique(space$a)))

  b.breaks <- c(min(space$b) - 1, sort(unique(space$b)) + diff(sort(unique(space$b)))/2)
  b.binned <- cut(Lab.coords[,'b'], breaks=b.breaks, labels=sort(unique(space$b)))
  options(warn=0)
  
  freqs.table <- table(L.binned, a.binned, b.binned)
  
  freqs <- vector()
  for (point in 1:nrow(space)) {
    freqs <- c(freqs, freqs.table[as.character(space[point, 'L']), as.character(space[point, 'a']), as.character(space[point, 'b'])])
  }
  
  return(freqs)
}

freqs.blur <- function(space, freqs, factor=2) {
  space.dist <- as.matrix(dist(space[,c('L','a','b')]))
  space.sim <- (1 - space.dist / max(space.dist)) ^ factor
  new.freqs <- replicate(nrow(space), 0)
  for (p1 in 1:nrow(space)) {
    for (p2 in 1:nrow(space)) {
      new.freqs[p1] <- new.freqs[p1] + freqs[p2] * space.sim[p1,p2]
    }
  }
  return(new.freqs)
}

priors.calculation <- function(space, images.dir, blurring.factor=NA) {
  files <- list.files(images.dir, pattern='*.tif', recursive=T, full.names=T)
  
  total.freqs <- replicate(nrow(space), 0)
  for (file in files) {
    img.lab <- read.image(file)
    freqs <- freqs.calculation(space, img.lab[sample(nrow(img.lab), 1000),])
    total.freqs <- total.freqs + freqs
  }
  if (!is.na(blurring.factor)) {
    total.freqs <- freqs.blur(space, total.freqs, factor=blurring.factor)
  }
  priors <- total.freqs / max(total.freqs)
  return(priors)
}