"gpd.rl2" <-
function (a, u, la, n, npy, mat, dat, xdat, ci=0.05, add.ci=FALSE) 
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
    mat <- matrix(c((la * (1 - la))/n, 0, 0, 0, mat[1, 1], mat[1, 2], 0, mat[2, 1], mat[2, 2]), ncol = 3)
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
}
