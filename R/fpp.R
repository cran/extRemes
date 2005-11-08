"fpp" <-
function (xdat, threshold, npy = 365, ydat = NULL, mul = NULL, 
    sigl = NULL, shl = NULL, mulink = identity, siglink = identity, 
    shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, 
    ...) 
{
    z <- list()
    npmu <- length(mul) + 1
    npsc <- length(sigl) + 1
    npsh <- length(shl) + 1
    n <- length(xdat)
    z$trans <- FALSE
    if (is.function(threshold)) 
        stop("`threshold' cannot be a function")
    u <- rep(threshold, length.out = n)
    if (length(unique(u)) > 1) 
        z$trans <- TRUE
	uInd <- xdat > u
    xdatu <- xdat[uInd]
    # xind <- (1:n)[uInd]
    # u <- u[xind]
    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat) - 0.57722 * in2
    if (is.null(mul)) {
        mumat <- as.matrix(rep(1, length(xdat)))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, length(xdat)), ydat[, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if (is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdat)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdat)), ydat[, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if (is.null(shl)) {
        shmat <- as.matrix(rep(1, length(xdat)))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, length(xdat)), ydat[, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
    init <- c(muinit, siginit, shinit)
    z$model <- list(mul, sigl, shl)
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))
    z$threshold <- threshold
    z$npy <- npy
    z$nexc <- length(xdatu)
    z$data <- xdatu
# print( length( u))
    pp.lik <- function(a) {
        mu <- mulink(mumat %*% (a[1:npmu]))
# print( dim( mu))
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
# print( dim( sc))
        xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
# print( dim( xi))
# print( dim( min((1 + ((xi * (u - mu))/sc)) )))
# stop("Just stop!")
        if (any(sc^uInd <= 0)) 
            return(10^6)
        if (min((1 + ((xi * (u - mu))/sc))^uInd) < 0) {
            l <- 10^6
        }
        else {
            y <- (xdat - mu)/sc
            y <- 1 + xi * y
            if (min(y) <= 0) 
                l <- 10^6
            else l <- sum(uInd*log(sc)) + sum(uInd*log(y) * (1/xi + 1)) + 
                n/npy * mean((1 + (xi * (u - mu))/sc)^(-1/xi))
        }
        l
    }
    x <- optim(init, pp.lik, hessian = TRUE, method = method, control = list(maxit = maxit, ...))
    mu <- mulink(mumat %*% (x$par[1:npmu]))
    sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (x$par[seq(npmu + npsc + 1, length = npsh)]))
    z$conv <- x$convergence
    z$nllh <- x$value
    z$vals <- cbind(mu, sc, xi, u)
    z$gpd <- apply(z$vals, 1, ppp, npy)
    if (z$trans) z$data <- as.vector((1 + (xi[uInd] * (xdatu - u[uInd]))/z$gpd[2,uInd])^(-1/xi[uInd]))
    z$mle <- x$par
    z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
    if (show) {
        if (z$trans) 
            print(z[c(2, 3)])
        if (length(z[[4]]) == 1) 
            print(z[4])
        print(z[c(5, 6, 8)])
        if (!z$conv) 
            print(z[c(9, 12, 14)])
    }
    invisible(z)
}

