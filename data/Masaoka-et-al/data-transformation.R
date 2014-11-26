inspect_by_layer <- function(original, selection, slice_dim, step=10) {
    extra_dims <- colnames(original)[colnames(original) != slice_dim]
    for (i in seq(min(selection[,slice_dim]), max(selection[,slice_dim]), by=step)) {
        plot(original[original[,slice_dim] == i, extra_dims], col='grey', main=paste(slice_dim,'=',i))
        points(selection[selection[,slice_dim] == i, extra_dims])
        Sys.sleep(5)
    }
}

ray_tracing_2d <- function(edges, dim1, dim2, step, inner_step) {
    y <- data.frame()
    for (i in seq(-200, 200, by=step)) {#seq(min(edges[, dim1]), max(edges[, dim1]), by=step)) {
        inside <- FALSE
        on_edge <- FALSE
        for (j in seq(-200, 200, by=inner_step)) {#seq(min(edges[, dim2]), max(edges[, dim2]), by=inner_step)) {
            #cat(dim1,":",i,dim2,":",j,"\n")
            if (any(edges[, dim1] == i & edges[, dim2] == j)) {
                if (!on_edge) {
                    inside <- !inside
                    on_edge <- TRUE
                }
            } else {
                on_edge <- FALSE
            }
            if (inside) {
                ytmp <- data.frame(i, j)
                colnames(ytmp) <- c(dim1,dim2)
                y <- rbind(y, ytmp)
            }
        }
    }
    return(y)
}

process_file <- function(file, step=10) {
    expected_columns <- c('a','b','L')

    x <- read.csv(file)

    stopifnot(colnames(x) == expected_columns)

    # round with 0.5 decimal places
    #x <- round(x*2)/2
    x$a <- round(x$a)
    x$b <- round(x$b)

    solid <- data.frame()
    for (k in seq(min(x$L), max(x$L), by=step)) {
        print(k)

        y <- ray_tracing_2d(x[x$L == k,], 'a', 'b', step, 1)
        z <- ray_tracing_2d(x[x$L == k,], 'b', 'a', step, 1)

        yz <- merge(y,z)
        yz$L <- k
        solid <- rbind(solid, yz)

        print(nrow(solid))
    }

    return(solid)
}

process_all <- function(files) {
    for (file in files) {
        process_file(file)
    }
}
