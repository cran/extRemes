"rlplot" <-
function (z, ci=0.05, add.ci=FALSE) 
{
# Originally, these were passed into the function separately.
a <- z$mle
u <- z$threshold
la <- z$rate
n <- z$n
npy <- z$npy
mat <- z$cov
dat <- z$data
xdat <- z$xdata
klaas <- class( z)

if( klaas=="gev.fit") {
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
        ylab = "Return Level")
    title("Return Level Plot")
    lines(-1/log(f), q)
	if( add.ci) {
    		lines(-1/log(f), q + qnorm(1-ci/2) * sqrt(v), col = 4)
    		lines(-1/log(f), q - qnorm(1-ci/2) * sqrt(v), col = 4)
		}
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
	out <- list( period=-1/log(f), level=q)
	class( out) <- "extRemes.return"
} else if( klaas=="gpd.fit") {
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
    plot(m/npy, q, log = "x", type = "n", xlim = c(0.1, max(m)/npy), 
        ylim = c(u, max(xdat, q[q > u - 1] + 1.96 * sqrt(v)[q > 
            u - 1])), xlab = "Return period (years)", ylab = "Return level", 
        main = "Return Level Plot")
    lines(m[q > u - 1]/npy, q[q > u - 1])
    if( add.ci) {
	lines(m[q > u - 1]/npy, q[q > u - 1] + qnorm(1-ci/2) * sqrt(v)[q > u - 1], col = 4)
    	lines(m[q > u - 1]/npy, q[q > u - 1] - qnorm(1-ci/2) * sqrt(v)[q > u - 1], col = 4)
	}
    nl <- n - length(dat) + 1
    sdat <- sort(xdat)
    points((1/(1 - (1:n)/(n + 1))/npy)[sdat > u], sdat[sdat > u])
	out <- list( period=m/npy, level=q)
	class( out) <- "extRemes.return"
	}
invisible(out)
}
