### PCA biplot function according to the Gower and Hand (1996) philosophy with prediction biplot axes
### Sugnet Lubbe, 2014, Department of Statistical Sciences, UCT
###
### For more information, see Gower, J.C., Lubbe, S. and le Roux, N.J. 2011. Understanding Biplots. Wiley: Chichester

PCAbiplot <- function (X, g = NULL, scaled.mat = FALSE, e.vects = 1:ncol(X), correlation.biplot = FALSE, samples = list(...), ax = list(...), Title = NULL, biplot.legend = FALSE, ...) 
{

indmat <- function (groep.vec) 
     { elements <- levels(factor(groep.vec))
       Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
       dimnames(Y) <- list(NULL, paste(elements))
       for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
       return(Y)
     }

biplot.check.X <- function (X, scaled.mat, centred.mat=TRUE) 
     { X <- as.matrix(X)
       unscaled.X <- X
       means <- apply(X, 2, mean)
       sd <- sqrt(apply(X, 2, var))
       if (!centred.mat) {  X <- X
                            means <- rep(0, ncol(X))
                            sd <- rep(1, ncol(X))    }      
       else { if (scaled.mat) X <- scale(X) else { X <- scale(X, scale = FALSE)
                                                   sd <- rep(1, ncol(X))        }    }  
       if (is.null(dimnames(X))) dimnames(X) <- list(paste(1:nrow(X)), paste("V", 1:ncol(X), sep = ""))
       if (length(dimnames(X)[[1]]) == 0) dimnames(X)[[1]] <- paste(1:nrow(X))
       if (length(dimnames(X)[[2]]) == 0) dimnames(X)[[2]] <- paste("V", 1:ncol(X), sep = "")
       list(X = X, unscaled.X = unscaled.X, means = means, sd = sd)
     }

biplot.check.G <- function (G, n) 
     { if (is.null(G)) { G <- matrix(indmat(rep(1, n)), ncol = 1)
                         dimnames(G) <- list(1:n, "AllData")      }
       if (nrow(G) != n) stop("number of rows of X and G differ")
       if (is.null(dimnames(G))) dimnames(G) <- list(NULL, paste("class", 1:ncol(G), sep = ""))
       if (length(dimnames(G)[[2]]) == 0) dimnames(G)[[2]] <- paste("class", 1:ncol(G), sep = "")
       if (ncol(G) == 1) class.vec <- rep(dimnames(G)[[2]], n) else class.vec <- apply(t(apply(G, 1, function(x) x == max(x))), 1, function(s, G) dimnames(G)[[2]][s], G = G)
       G
     }

biplot.ax.control <- function (p, X.names, which = 1:p, col = "seagreen", lwd = 1, lty = 1, label = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, 
                               ticks = 5, tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1, ax.names = X.names) 
     { if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
       which <- which[which <= p]
       which <- which[which > 0]
       ax.num <- length(which)
       while (length(col) < ax.num) col <- c(col, col) 
       col <- as.vector(col[1:ax.num])
       while (length(lwd) < ax.num) lwd <- c(lwd, lwd) 
       lwd <- as.vector(lwd[1:ax.num])
       while (length(lty) < ax.num) lty <- c(lty, lty) 
       lty <- as.vector(lty[1:ax.num])
       if (label != "Orthog" & label != "Hor" & label != "Paral") stop("Incorrect specification of axis label direction")
       while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
       label.col <- as.vector(label.col[1:ax.num])
       while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
       label.cex <- as.vector(label.cex[1:ax.num])
       while (length(label.dist) < ax.num) label.dist <- c(label.dist,label.dist)
       label.dist <- as.vector(label.dist[1:ax.num])
       while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
       ticks <- as.vector(ticks[1:ax.num])
       while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
       tick.col <- as.vector(tick.col[1:ax.num])
       while (length(tick.size) < ax.num) tick.size <- c(tick.size,tick.size)
       tick.size <- as.vector(tick.size[1:ax.num])
       while (length(tick.label) < ax.num) tick.label <- c(tick.label,tick.label)
       tick.label <- as.vector(tick.label[1:ax.num])
       while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col,tick.label.col)
       tick.label.col <- as.vector(tick.label.col[1:ax.num])
       while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex,tick.label.cex)
       tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
       while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side,tick.label.side)
       tick.label.side <- as.vector(tick.label.side[1:ax.num])
       while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
       tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
       while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
       tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
       ax.names <- ax.names[which]
       while (length(ax.names) < p) ax.names <- c(ax.names, "")
       ax.names <- as.vector(ax.names[1:ax.num])
       list(which = which, col = col, lwd = lwd, lty = lty, label = label, label.col = label.col, label.cex = label.cex, label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label, 
         tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, names = ax.names)
     }

calibrate.axis <- function (j, unscaled.X, means, sd, axes.rows, ax.which, ax.tickvec) 
     {  ax.num <- ax.which[j]
        tick <- ax.tickvec[j]
        ax.direction <- axes.rows[j, ]
        r <- ncol(axes.rows)
        number.points <- 100
        std.ax.tick.label <- pretty(unscaled.X[, ax.num], n = tick)
        std.range <- c(min(std.ax.tick.label), max(std.ax.tick.label))
        std.ax.tick.label.min <- std.ax.tick.label - (std.range[2] - std.range[1])
        std.ax.tick.label.max <- std.ax.tick.label + (std.range[2] - std.range[1])
        std.ax.tick.label <- c(std.ax.tick.label, std.ax.tick.label.min, std.ax.tick.label.max)
        interval <- (std.ax.tick.label - means[ax.num])/sd[ax.num]
        axis.vals <- seq(from = min(interval), to = max(interval), length = number.points)
        axis.vals <- sort(unique(c(axis.vals, interval)))
        number.points <- length(axis.vals)
        axis.points <- matrix(0, nrow = number.points, ncol = r)
        for (i in 1:r) axis.points[, i] <- axis.vals * ax.direction[i]
        axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num], 0)
        for (i in 1:number.points) if (any(zapsmall(axis.points[i, r + 1] - std.ax.tick.label) == 0)) 
        axis.points[i, r + 2] <- 1
        axis.points
     }

biplot.sample.control <- function (J, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), pch = 3, cex = 1, label = T, label.cex = 0.75, label.side = "bottom") 
     {  while (length(col) < J) col <- c(col, col)
        col <- as.vector(col[1:J])
        while (length(pch) < J) pch <- c(pch, pch)
        pch <- as.vector(pch[1:J])
        while (length(cex) < J) cex <- c(cex, cex)
        cex <- as.vector(cex[1:J])
        while (length(label) < J) label <- c(label, label)
        label <- as.vector(label[1:J])
        while (length(label.cex) < J) label.cex <- c(label.cex, label.cex)
        label.cex <- as.vector(label.cex[1:J])
        while (length(label.side) < J) label.side <- c(label.side, label.side)
        label.side <- as.vector(label.side[1:J])
        list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side)
}

.samples.plot <- function(Z, G, sample.style) 
     {  x.vals <- Z[, 1]
        y.vals <- Z[, 2]
        invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
        Z <- Z[invals, ]
        for (j in 1:ncol(G)) { class.num <- j
                               Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
                               text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
                               if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]], cex = sample.style$label.cex[j], pos = text.pos, col = sample.style$col[j])
                               for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = sample.style$pch[j], col = sample.style$col[j], cex = sample.style$cex[j])   }
     }

.marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex) 
     {  uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
        mm <- 1/(uin[1] * 25.4)
        d <- expand * mm
        if (grad == "v") { lines(rep(x, 2), c(y - d, y + d), col = col)
                           if (label.on.off == 1) text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)     }
        if (grad == "h") { lines(c(x - d, x + d), rep(y, 2), col = col)
                           if (label.on.off == 1) if (side == "right") text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                           else text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                                          }
        if (is.numeric(grad)) {  b <- d * sqrt(1/(1 + grad * grad))
                                 a <- b * grad
                                 lines(c(x - b, x + b), c(y - a, y + a), col = col)
                                 if (label.on.off == 1) if (side == "right") text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                                                        else text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                   }
     }
.marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex) 
     {  x <- vec[1]
        y <- vec[2]
        marker.val <- vec[3]
        label.on.off <- vec[4]
        if (is.na(coef[2])) .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
        else if (coef[2] == 0) .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
             else .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
     }
.lin.axes.plot <- function(z.axes, ax.style) 
     {  for (i in 1:length(ax.style$which)) 
          { ax.num <- ax.style$which[i]
            marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
            marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
            x.vals <- marker.mat[, 1]
            y.vals <- marker.mat[, 2]
            lin.coef <- coefficients(lm(y.vals ~ x.vals))
            if (is.na(lin.coef[2])) abline(v = x.vals, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i]) else abline(coef = lin.coef, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
            if (ax.style$label == "Hor") { par(las = 1)
                                           adjust <- c(0.5, 1, 0.5, 0)   }
            if (ax.style$label == "Orthog") {  par(las = 2)
                                               adjust <- c(1, 1, 0, 0)   }
            if (ax.style$label == "Paral") {   par(las = 0)
                                               adjust <- c(0.5, 0.5, 0.5, 0.5)   }
            h <- nrow(marker.mat)
            if (is.na(lin.coef[2])) { if (y.vals[1] < y.vals[h]) 
                                        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                      else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])                      
                                    }
            else { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
                   y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
                   x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
                   x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
                   if (lin.coef[2] == 0) 
                     { if (x.vals[1] < x.vals[h]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                       else mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])                      
                     }
                   if (lin.coef[2] > 0) 
                     {  if (x.vals[1] < x.vals[h]) 
                          if (y1.ster <= usr[4] & y1.ster >= usr[3]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                          else mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                        else 
                          if (y2.ster <= usr[4] & y2.ster >= usr[3]) mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                          else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])                                       
                     }
                   if (lin.coef[2] < 0) 
                     {  if (x.vals[1] < x.vals[h]) 
                          if (y1.ster <= usr[4] & y1.ster >= usr[3]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                          else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                        else 
                           if (y2.ster <= usr[4] & y2.ster >= usr[3]) mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])                                                                                             
                     }   
                 }
          invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
          std.markers <- zapsmall(marker.mat[invals, 3])
          x.vals <- x.vals[invals]
          y.vals <- y.vals[invals]
          if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
          if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
          apply(cbind(x.vals, y.vals, std.markers, label.on.off), 1, 
             .marker.func, coef = lin.coef, col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i], 
                           label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
        }
     }

    if (!is.null(g)) G <- indmat(g) else G <- NULL
    e.vects <- e.vects[1:2]
    X.info <- biplot.check.X(X, scaled.mat)
    X <- X.info$X
    unscaled.X <- X.info$unscaled.X
    means <- X.info$means
    sd <- X.info$sd
    G <- biplot.check.G(G, nrow(X))
    
    n <- nrow(X)
    p <- ncol(X)
    J <- ncol(G)

    svd.out <- svd(X)
    V.mat <- svd.out$v
    U.mat <- svd.out$u
    Sigma.mat <- diag(svd.out$d)
    Vr <- svd.out$v[, e.vects, drop = F]
    fit.out <- cumsum(svd.out$d^2)/sum(svd.out$d^2)

    if (correlation.biplot) { lambda.r <- diag(svd(t(X) %*% X)$d[1:2])
                              Z <- sqrt(n - 1) * X %*% Vr %*% (sqrt(solve(lambda.r)))    }
    else Z <- X %*% Vr
    dimnames(Z) <- list(dimnames(X)[[1]], NULL)

    num.vars <- p
    var.names <- dimnames(X)[[2]]
    ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))
    if (correlation.biplot) axes.direction <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
    else axes.direction <- 1/(diag(Vr %*% t(Vr))) * Vr 
    if (length(ax$which) == 0) z.axes <- NULL
    else z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks)

    samples <- do.call("biplot.sample.control", c(J, samples))

    par(pty = "s")
    plot(Z[, 1] * 1.2, Z[, 2] * 1.2, xlim = range(Z[, 1] * 1.2), ylim = range(Z[, 2] * 1.2), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
    usr <- par("usr")
    if (!is.null(z.axes)) .lin.axes.plot(z.axes, ax)
    .samples.plot(Z, G, samples)
    if (!is.null(Title)) title(main=Title)
    if (biplot.legend) { dev.new()
                         par(pty = "m", mar = c(3, 1, 3, 1))
                         plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
                         legend ("top left", dimnames(G)[[2]], col=samples$col, pch=15, cex=1.5)                                     }

    list(scores=Z, loadings=Vr, quality=fit.out)
}
