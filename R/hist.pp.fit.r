hist.pp.fit <- function(x, breaks.method="Sturges", ...)
{
a <- x$mle
u <- x$threshold
npy <- x$npy
dat <- x$data
#
# function called by pp.diag
# produces histogram and density plot
#
        h <- hist(dat, freq = FALSE, breaks=breaks.method, plot = FALSE, ...)
        x1 <- seq(u, max(h$breaks), length = 100)
        gp <- ppp(c(a, u), npy)
        y <- gpd.dens(gp[2:3], u, x1)
        hist(dat, freq = FALSE, breaks=breaks.method, ylim = c(0, max(y)), xlab = "x", ylab = "f(x)",
                main = "Density Plot", ...)
        lines(x1, y, col = 4)
invisible( h)
}

