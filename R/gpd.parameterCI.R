"gpd.parameterCI" <-
function (z, m, conf = 0.95, nint = 100, rl.xup=NULL, rl.xlow=NULL, xi.xup=NULL, xi.xlow=NULL, rl.only=FALSE, xi.only=FALSE,
		make.plot=FALSE)
{
###
## Function to try to find confidence limits (via profile likelihoods) for the GPD
## parameters (shape and return level).
#
# 'z' is an object of class "gpd.fit" created by the 'gpd.fit' function of the 'ismev' package.
# 'm' is the 'm'-year return level to find CIs for.
# 'conf' is the alpha confidence level.
# 'nint' are the number of points with which to find the likelihood value (and CIs for).
# 'rl.only' if TRUE only the return level parameter will be estimated.
# 'xi.only' if TRUE only the shape parameter will be estimated.
# 'make.plot' if TRUE will plot the profile likelihoods.
#
# Returns a list with components:
# 'rl' and 'xi', each of which are lists containing info on the respective confidence intervals.
# 	Including 'up' and 'dn' (the upper and lower limits), and 'sfun', the spline fit function
#	used to create profile plot.
#
lmts <- list()
xdat <- z$data
ma <- -z$nllh
u <- z$threshold
la <- z$rate
npy <- z$npy
v <- numeric(nint)
eps <- 1e-6
if( make.plot) {
        if( !rl.only & !xi.only) par( mfrow=c(2,1))
        else par( mfrow=c(1,1))
        }

if( !xi.only) {
est.rl.xup <- is.null( rl.xup)
est.rl.xlow <- is.null( rl.xlow)
mstar <- m
m <- m*npy
rl.mle <- gpd.ret( z, mstar)
q <- gpdq2( z$mle, u, la, m)
d1 <- rep(0, length(q))
d2 <- (gpdq2(c( z$mle[1]+eps, z$mle[2]), u, la, m) - q)/eps
d3 <- (gpdq2(c( z$mle[1], z$mle[2]), u, la, m) - q)/eps
d <- cbind(d1, d2, d3)
mat <- z$cov
mat <- matrix(c((la * (1 - la))/z$n, 0, 0, 0, mat[1, 1], mat[1, 2], 0, mat[2, 1], mat[2, 2]), nc = 3)
vv <- apply(d, 1, q.form, m = mat)
if( est.rl.xlow) rl.xlow <- rl.mle - 1.5*1.96*sqrt(vv)
if( est.rl.xup) rl.xup <- rl.mle + 1.5*1.96*sqrt(vv)
x <- seq(rl.xlow, rl.xup, length = nint)
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
lmts$upcross.level <- ma-0.5*qchisq(conf,1)
lmts$rl$mle <- rl.mle
sfun <- splinefun(x, -v)
lmts$rl$sfun <- sfun
# if( !est.rl.xup) {
	x1 <- order( sfun( c(rl.mle, rl.xup)))
	if( x1[1] == 1) lmts$rl$up <- bisearch( rl.mle, rl.xup, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
	else lmts$rl$up <- bisearch( rl.xup, rl.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
# } else lmts$rl$up <- findparlims( sfun, upcross.level=ma-0.5*qchisq(conf,1), guess=rl.xup)
# if( !est.rl.xlow) {
	x2 <- order( sfun( c(rl.mle, rl.xlow)))
	if( x2[1] == 1) lmts$rl$dn <- bisearch( rl.mle, rl.xlow, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
	else lmts$rl$dn <- bisearch( rl.xlow, rl.mle, f=sfun, upcross.level=ma-0.5*qchisq(conf,1))$x
# } else lmts$rl$dn <- findparlims( sfun, upcross.level=ma-0.5*qchisq(conf,1), guess=rl.xlow)

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
	est.xi.xup <- is.null( xi.xup)
	est.xi.xlow <- is.null( xi.xlow)
	xdat <- z$data
	xi.mle <- z$mle[2]
	if( est.xi.xlow) xi.xlow <- xi.mle - 1.5*1.96*z$se[2]
	if( est.xi.xup) xi.xup <- xi.mle + 1.5*1.96*z$se[2]
    u <- z$threshold
    v <- numeric(nint)
    x <- seq(xi.xup+1.96*z$se[2], xi.xlow-1.96*z$se[2], length = nint)
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
