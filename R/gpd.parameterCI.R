"gpd.parameterCI" <-
function (z, m, rl.xlow, rl.xup, xi.xlow=NULL, xi.xup=NULL, npy = 365,
		conf = 0.95, nint = 100, rl.only=FALSE, xi.only=FALSE,
		make.plot=FALSE)
{
# cat("If routine fails (return level), try changing plotting interval", fill = TRUE)
lmts <- list()
xdat <- z$data
ma <- -z$nllh
u <- z$threshold
la <- z$rate
v <- numeric(nint)
if( make.plot) {
        if( !rl.only & !xi.only) par( mfrow=c(2,1))
        else par( mfrow=c(1,1))
        }

if( !xi.only) {
x <- seq(rl.xlow, rl.xup, length = nint)
mstar <- m
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
rl.mle <- gpd.ret( z, mstar)
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
	plot(x, -v, type = "l", xlab = "Return Level", ylab = "Profile Log-likelihood")
	abline(h = ma, col="green")
	abline(h = ma - 0.5 * qchisq(conf, 1), col="green")
	abline(v=c(lmts$rl$dn, lmts$rl$up),lty=2)
	} # end of if make.plot stmt.
	} # end of if !xi.only stmt.

if( !rl.only) {
# Now do the shape parameter...
# cat("If routine fails (shape parameter), try changing plotting interval", fill = TRUE)
	xdat <- z$data
	xi.mle <- z$mle[2]
    u <- z$threshold
    v <- numeric(nint)
    x <- seq(xi.xup, xi.xlow, length = nint)
    sol <- z$mle[1]
    gpd.plikxi <- function(a) {
        if (abs(xi) < 10^(-4)) 
            l <- length(xdat) * log(a) + sum(xdat - u)/a
        else {
            y <- (xdat - u)/a
            y <- 1 + xi * y
            if (any(y <= 0) || a <= 0) 
                l <- 10^6
            else l <- length(xdat) * log(a) + sum(log(y)) * (1/xi + 1)
        }
        l
    }
    for (i in 1:nint) {
        xi <- x[i]
        opt <- optim(sol, gpd.plikxi, method = "BFGS")
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
	abline(h = ma, lty = 1, col="green")
	abline(h = ma - 0.5 * qchisq(conf, 1), col="green")
	abline(v=c(lmts$xi$dn, lmts$xi$up), lty=2)
	}
	} # end of if !rl.only stmt
class( lmts) <- "gpd.parameterCI.obj"
invisible(lmts)
}
