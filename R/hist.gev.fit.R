hist.gev.fit <- function(x, breaks.method="Sturges",...)
{
a <- x$mle
dat <- x$data
#
# Plots histogram of data and fitted density
# for output of gev.fit stored in z
#
        h <- hist(dat, freq = FALSE, breaks=breaks.method, plot = FALSE,...)
        if(a[3] < 0) {
                x1 <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] -
                        0.001)), length = 100)
        }
        else {
                x1 <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), max(h$
                        breaks), length = 100)
        }
        y <- gev.dens(a, x1)
        hist(dat, freq = FALSE, breaks=breaks.method, ylim = c(0, max(y)), xlab = "z", ylab = "f(z)",
                main = "Density Plot",...)
        points(dat, rep(0, length(dat)))
        lines(x1, y)
invisible(h)
} 
