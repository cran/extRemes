plot.gev.fit <- function (x, ...)
{
    n <- length(x$data)
    z <- (1:n)/(n + 1)
    if (x$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(z, exp(-exp(-sort(x$data))), xlab = "Empirical",
            ylab = "Model",...)
        abline(0, 1, col = 4)
        title("Residual Probability Plot")
        plot(-log(-log(z)), sort(x$data), ylab = "Empirical",
            xlab = "Model",...)
        abline(0, 1, col = 4)
        title("Residual Quantile Plot (Gumbel Scale)")
    }
    else {
        oldpar <- par(mfrow = c(2, 2))
        gev.pp(x$mle, x$data)
        gev.qq(x$mle, x$data)
        gev.rl(x$mle, x$cov, x$data)
        gev.his(x$mle, x$data)
    }
    par(oldpar)
    invisible()
} # end of plot.gev.fit fcn

plot.gum.fit <- function (x,...)
{
    x$mle <- c(x$mle, 0)
    n <- length(z$data)
    z <- (1:n)/(n + 1)
    if (x$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(z, exp(-exp(-sort(x$data))), xlab = "empirical",
            ylab = "model",...)
        abline(0, 1, col = 4)
        title("Residual Probability Plot")
        plot(-log(-log(z)), sort(x$data), xlab = "empirical",
            ylab = "model",...)
        abline(0, 1, col = 4)
        title("Residual Quantile Plot (Gumbel Scale)")
    }
    else {
        oldpar <- par(mfrow = c(2, 2))
        gev.pp(x$mle, x$data)
        gev.qq(x$mle, x$data)
        gum.rl(x$mle, x$cov, x$data)
        gev.his(x$mle, x$data)
    }
    par(oldpar)
    invisible()
} # end of plot.gum.fit fcn

plot.rlarg.fit <- function (x, ...)
{
	n <- z$r
    z2 <- x
    z2$data <- x$data[, 1]
    oldpar <- par(ask = TRUE, mfcol = c(2, 2))
    if (x$trans) {
        for (i in 1:n) {
            rlarg.pp(c(0, 1, 0), x$data[, 1:x$r], i)
            rlarg.qq(c(0, 1, 0), x$data[, 1:x$r], i)
        }
    }
    else {
        gev.diag(z2)
        for (i in 1:n) {
            rlarg.pp(x$mle, x$data, i)
            rlarg.qq(x$mle, x$data, i)
        }
    }
    par(oldpar)
    invisible()
} # end of plot.rlarg.fit fcn

plot.gpd.fit <- function (x, ...)
{
    n <- length(x$data)
    z <- (1:n)/(n + 1)
    if (x$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(z, 1 - exp(-sort(x$data)), xlab = "Empirical", ylab = "Model", ...)
        abline(0, 1, col = 4)
        title("Residual Probability Plot")
        plot(-log(1 - z), sort(x$data), ylab = "Empirical", xlab = "Model", ...)
        abline(0, 1, col = 4)
        title("Residual Quantile Plot (Exptl. Scale)")
    }
    else {
        oldpar <- par(mfrow = c(2, 2))
        gpd.pp(x$mle, x$threshold, x$data)
        gpd.qq(x$mle, x$threshold, x$data)
        gpd.rl(x$mle, x$threshold, x$rate, x$n, x$npy, x$cov,
            x$data, x$xdata)
        gpd.his(x$mle, x$threshold, x$data)
    }
    par(oldpar)
    invisible()
} # end of plot.gpd.fit fcn

plot.pp.fit <- function (x,...)
{
    n <- length(x$data)
    z <- (1:n)/(n + 1)
    if (x$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(z, sort(x$data), xlab = "empirical", ylab = "model")
        abline(0, 1, col = 3)
        title("Residual Probability Plot")
        plot(-log(1 - z), -log(1 - sort(x$data)), ylab = "empirical",
            xlab = "model")
        abline(0, 1, col = 3)
        title("Residual quantile Plot (Exptl. Scale)")
    }
    else {
        oldpar <- par(mfrow = c(1, 2), pty = "s")
        pp.pp(x$mle, x$threshold, x$npy, x$data)
        pp.qq(x$mle, x$threshold, x$npy, x$data)
    }
    par(oldpar)
    invisible()
} # end of plot.pp.fit fcn 
