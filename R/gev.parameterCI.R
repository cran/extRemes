"gev.parameterCI" <-
function (z, m, rl.xlow, rl.xup, xi.xlow=NULL, xi.xup=NULL, conf = 0.95, nint = 100, rl.only=FALSE, xi.only=FALSE, make.plot=FALSE) 
{
lmts <- list()
ma <- -z$nllh
if( !xi.only) {
	est.rl.xup <- is.null( rl.xup)
	est.rl.xlow <- is.null( rl.xlow)
# cat("If routine fails (return level), try changing plotting interval", fill = TRUE)
    p <- 1/m
    v <- numeric(nint)
	rl.mle <- gev.ret( z, m)
        # Try to find a reasonable range of values, within which to search for
        # the CI, using the delta method.
eps <- 1e-06
    a1 <- z$mle
    a2 <- z$mle
    a3 <- z$mle
    a1[1] <- z$mle[1] + eps
    a2[2] <- z$mle[2] + eps
    a3[3] <- z$mle[3] + eps
   # f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5,
   #     0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
	q <- gevq(z$mle, p=p)
    d1 <- (gevq(a1, p=p) - q)/eps
    d2 <- (gevq(a2, p=p) - q)/eps
    d3 <- (gevq(a3, p=p) - q)/eps
    d <- cbind(d1, d2, d3)
	mat <- z$cov
    vv <- apply(d, 1, q.form, m = mat)

# q <- gpdq2( z$mle, u, la, m)
# d1 <- rep(0, length(q))
# d2 <- (gpdq2(c( z$mle[1]+eps, z$mle[2]), u, la, m) - q)/eps
# d3 <- (gpdq2(c( z$mle[1], z$mle[2]), u, la, m) - q)/eps
# d <- cbind(d1, d2, d3)
# mat <- z$cov
# mat <- matrix(c((la * (1 - la))/z$n, 0, 0, 0, mat[1, 1], mat[1, 2], 0, mat[2, 1], mat[2, 2]), nc = 3)
# vv <- apply(d, 1, q.form, m = mat)
        if( est.rl.xlow) rl.xlow <- rl.mle - 1.5*qnorm((1-conf)/2, lower.tail=FALSE)*sqrt(vv)
        if( est.rl.xup) rl.xup <- rl.mle + 1.5*qnorm((1-conf)/2, lower.tail=FALSE)*sqrt(vv)

    x <- seq(rl.xlow, rl.xup, length = nint)
    sol <- c(z$mle[2], z$mle[3])

gum.lik <- function(a) {
        mu <- a[1]
        sc <- a[2]
        if (sc <= 0) return(10^6)
        y <- (z$data - mu)/sc
        sum(log(sc)) + sum(y) + sum(exp(-y))
    } # end of gum.lik fcn

    gev.plik <- function(a) {
        if (p != 0) 
            mu <- xp - a[1]/a[2] * ((-log(1 - p))^(-a[2]) - 1)
        else mu <- xp + a[1]/a[2]
        if (abs(a[2]) < 10^(-6)) l <- gum.lik(c(mu, a[1]))
        else {
            y <- (z$data - mu)/a[1]
            y <- 1 + a[2] * y
            if (any(y <= 0) || a[1] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(y^(-1/a[2])) + 
                sum(log(y)) * (1/a[2] + 1)
        }
        l
    }
    for (i in 1:nint) {
        xp <- x[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v[i] <- opt$value
    }

	lmts$upcross.level <- ma-0.5*qchisq(conf,1)
	lmts$rl$mle <- rl.mle
	sfun <- splinefun(x, -v)
	lmts$rl$sfun <- sfun
	x1 <- order( sfun( c(rl.mle, rl.xup)))
	x2 <- order( sfun( c(rl.mle, rl.xlow)))
	if( x1[1] == 1) lmts$rl$up <- bisearch( rl.mle, rl.xup, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
	else lmts$rl$up <- bisearch( rl.xup, rl.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
	if( x2[1] == 1) lmts$rl$dn <- bisearch( rl.mle, rl.xlow, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
	else lmts$rl$dn <- bisearch( rl.xlow, rl.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
if( make.plot) {
	if( !rl.only & !xi.only) par( mfrow=c(2,1))
	else par( mfrow=c(1,1))
	plot(x, -v, type = "l", xlab = "Return Level", ylab = " Profile Log-likelihood")
	abline(h = ma, col = 4)
        abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
	abline(v=c(lmts$rl$up, lmts$rl$dn), lty=2)
	}
} # end of if !xi.only stmt

if( !rl.only) {
	est.xi.xup <- is.null( xi.xup)
	est.xi.xlow <- is.null( xi.xlow)
# Now find the limits for the shape parameter.
# cat("If routine fails (shape parameter), try changing plotting interval", fill = TRUE)
	xi.mle <- z$mle[3]
        if( est.xi.xlow) xi.xlow <- xi.mle - 1.5*qnorm((1-conf)/2, lower.tail=FALSE)*z$se[2]
        if( est.xi.xup) xi.xup <- xi.mle + 1.5*qnorm((1-conf)/2, lower.tail=FALSE)*z$se[2]
    v <- numeric(nint)
    x <- seq(xi.xup, xi.xlow, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    gev.plikxi <- function(a) {
        if (abs(xi) < 10^(-4))
            l <- gum.lik(c(a[1], a[2]))
        else {
            y <- (z$data - a[1])/a[2]
            y <- 1 + xi * y
            if (any(y <= 0) || a[2] <= 0)
                l <- 10^6
            else l <- length(y) * log(a[2]) + sum(y^(-1/xi)) +
                sum(log(y)) * (1/xi + 1)
        }
        l
    }
    for (i in 1:nint) {
        xi <- x[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par
        v[i] <- opt$value
    }
sfun <- splinefun(x, -v)
lmts$xi$sfun <- sfun
x1 <- order( sfun( c(xi.mle, xi.xup)))
x2 <- order( sfun( c(xi.mle, xi.xlow)))
if( x1[1] == 1) lmts$xi$up <- bisearch( xi.mle, xi.xup, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
else lmts$xi$up <- bisearch( xi.xup, xi.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
if( x2[1] == 1) lmts$xi$dn <- bisearch( xi.mle, xi.xlow, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
else lmts$xi$dn <- bisearch( xi.xlow, xi.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x

if( make.plot) {
	plot(x, -v, type = "l", xlab = "Shape Parameter", ylab = "Profile Log-likelihood")
	ma <- -z$nllh
	abline(h = ma, col = 4)
	abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
	abline(v = c(lmts$xi$dn, lmts$xi$up), lty=2)
	par( mfrow=c(1,1))
}
	} # end of if !rl.only stmt
class( lmts) <- "gev.parameterCI.obj"
invisible(lmts)
}
