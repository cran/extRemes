hist.gpd.fit <- function(x, breaks.method="Sturges", ...)
{
mle <- x$mle
u <- x$threshold
dat <- x$data

#
# function called by gpd.diag
# produces histogram and density plot
#
        h <- hist(dat, freq = FALSE, breaks=breaks.method, plot = FALSE, ...)
        x1 <- seq(u, max(h$breaks), length = 100)
        y <- gpd.dens(mle, u, x1)
        hist(dat, freq = FALSE, breaks=breaks.method, ylim = c(0, max(y)), xlab = "x", ylab = "f(x)",
                main = "Density Plot", ...)
        lines(x1, y, col = 4)
invisible(h)
}

