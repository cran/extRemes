"gpd.rlci" <-
function (z, m, prange=NULL, npy = 365, conf = 0.95, nint = 100) 
{
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    xdat <- z$data
    u <- z$threshold
    la <- z$rate
    v <- numeric(nint)
if( is.null( prange)) {
	prange <- range( z$data, na.rm=TRUE)
	prange[1] <- prange[1] - 0.5*qchisq(conf,1)
	prange[2] <- prange[2] + 0.5*qchisq(conf,1)
	}
x <- seq(xlow, xup, length = nint) 
    m <- m * npy
    sol <- z$mle[2]
    gpd.plik <- function(a) {
        if (m != Inf) 
            sc <- (a * (xp - u))/((m * la)^a - 1)
        else sc <- (u - xp)/a
        if (abs(a) < 10^(-4)) 
            l <- length(xdat) * log(sc) + sum(xdat - u)/sc
        else {
            y <- (xdat - u)/sc
            y <- 1 + a * y
            if (any(y <= 0) || sc <= 0) 
                l <- 10^6
            else l <- length(xdat) * log(sc) + sum(log(y)) * 
                (1/a + 1)
        }
        l
    }
    for (i in 1:nint) {
        xp <- x[i]
        opt <- optim(sol, gpd.plik, method = "BFGS")
        sol <- opt$par
        v[i] <- opt$value
    }
    plot(x, -v, type = "l", xlab = "Return Level", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma)
    abline(h = ma - 0.5 * qchisq(conf, 1))
    invisible()
}
