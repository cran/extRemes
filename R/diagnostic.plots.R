rl.gev.fit <- function (a, mat, dat)
{
    eps <- 1e-06
    a1 <- a
    a2 <- a
    a3 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    a3[3] <- a[3] + eps
    f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5,
        0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
    q <- gevq(a, 1 - f)
    d1 <- (gevq(a1, 1 - f) - q)/eps
    d2 <- (gevq(a2, 1 - f) - q)/eps
    d3 <- (gevq(a3, 1 - f) - q)/eps
    d <- cbind(d1, d2, d3)
    v <- apply(d, 1, q.form, m = mat)
    plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000),
        ylim = c(min(dat, q), max(dat, q)), xlab = "Return Period",
        ylab = "Return Level", xaxt="n")
	axis(1, at=c(0.1, 1, 10, 100, 1000), labels=as.character( c(0.1, 1, 10, 100, 1000)))
    title("Return Level Plot")
    lines(-1/log(f), q)
    lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
    lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
} # end of rl.gev.fit fcn.

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
	rl.gev.fit( x$mle, x$cov, x$data)
        # gev.rl(x$mle, x$cov, x$data)
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
	z <- x
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

rl.gpd.fit <- function (a, u, la, n, npy, mat, dat, xdat)
{
    a <- c(la, a)
    eps <- 1e-06
    a1 <- a
    a2 <- a
    a3 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    a3[3] <- a[3] + eps
    jj <- seq(-1, 3.75 + log10(npy), by = 0.1)
    m <- c(1/la, 10^jj)
    q <- gpdq2(a[2:3], u, la, m)
    d1 <- (gpdq2(a1[2:3], u, la, m) - q)/eps
    d2 <- (gpdq2(a2[2:3], u, la, m) - q)/eps
    d3 <- (gpdq2(a3[2:3], u, la, m) - q)/eps
    d <- cbind(d1, d2, d3)
    mat <- matrix(c((la * (1 - la))/n, 0, 0, 0, mat[1, 1], mat[1,
        2], 0, mat[2, 1], mat[2, 2]), nc = 3)
    v <- apply(d, 1, q.form, m = mat)
    plot( m/npy, q, log = "x", type = "n", xlim = c(0.1, max(m)/npy),
        ylim = c(u, max(xdat, q[q > u - 1] + 1.96 * sqrt(v)[q >
            u - 1])), xlab = "Return period (years)", ylab = "Return level",
        main = "Return Level Plot", xaxt="n")
	axis( 1, at=c(0.1, 1, 10, 100, 1000), labels=as.character( c(0.1, 1, 10, 100, 1000)))
    lines(m[q > u - 1]/npy, q[q > u - 1])
    lines(m[q > u - 1]/npy, q[q > u - 1] + 1.96 * sqrt(v)[q >
        u - 1], col = 4)
    lines(m[q > u - 1]/npy, q[q > u - 1] - 1.96 * sqrt(v)[q >
        u - 1], col = 4)
    nl <- n - length(dat) + 1
    sdat <- sort(xdat)
    points((1/(1 - (1:n)/(n + 1))/npy)[sdat > u], sdat[sdat >
        u])
} # end of 'rl.gpd.fit' fcn.

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
# gpd.rl(x$mle, x$threshold, x$rate, x$n, x$npy, x$cov, x$data, x$xdata)
	rl.gpd.fit( x$mle, x$threshold, x$rate, x$n, x$npy, x$cov, x$data, x$xdata)
	hist( x, ...)
        # hist(x$mle, x$threshold, x$data)
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
