return.level <- function(z, conf=0.05, rlevels= c(10,100,210,510,810,980),
				make.plot=TRUE) {
#
# Function to compute the return levels and confidence intervals.
# This simply is Stuart Coles' function (gev.rl, gpd.rl, etc...), but returns
# actual numbers (if desired) instead of just plotting them.
#
out <- list()
out$conf.level <- conf
eps <- 1e-06
a <- z$mle
mat <- z$cov
dat <- z$data
a1 <- a
a2 <- a
a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
kappa <- qnorm(conf/2, lower.tail=FALSE)
nx <- length( rlevels)
cl <- 1-conf
if( class(z) == "gev.fit") {
	f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5,
			0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
	q <- gevq(a, 1 - f)
	d1 <- (gevq(a1, 1 - f) - q)/eps
	d2 <- (gevq(a2, 1 - f) - q)/eps
	d3 <- (gevq(a3, 1 - f) - q)/eps
	d <- cbind(d1, d2, d3)
	v <- apply(d, 1, q.form, m = mat)
	yl <- c(min(dat, q), max(dat, q))
	conf.low <- numeric(nx)+NA
	conf.up <- conf.low
	cat("\n", "Please be patient, this may take a few seconds.  Loop is 1 to ", nx, "\n")
	options( error=expression(NULL))
	for( i in 1:nx) {
		cat(i, " ")
		if( rlevels[i] <= 100)
		temp <- try( gev.parameterCI( z=z, rl.xlow=yl[1], rl.xup=yl[2], m=rlevels[i], conf=cl, rl.only=TRUE))
		else temp <- try( gev.parameterCI( z=z, rl.xlow=yl[1]*1.25, rl.xup=yl[2]*1.5,m=rlevels[i], conf=cl,
						rl.only=TRUE))
		if( class(temp) != "try-error") {
				conf.low[i] <- temp$rl$dn
				conf.up[i] <- temp$rl$up
				}
		} # end of for 'i' loop.
	options( error=NULL)
	cat("\n")
	conf2 <- matrix(NA, nrow=nx, ncol=2)
	ind.low <- !is.na( conf.low)
	ind.up <- !is.na( conf.up)
	if( sum( ind.low) > 3) {
		low.sfun <- splinefun( rlevels[ind.low], conf.low[ind.low])
		conf2[,1] <- low.sfun( rlevels)
		}
	if( sum( ind.up) > 3) {
		up.sfun <- splinefun( rlevels[ind.up], conf.up[ind.up])
		conf2[,2] <- up.sfun( rlevels)
		}
	if( make.plot) {
		if( any( is.na( yl))) yl <- range( q, na.rm=TRUE)
		plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000),
			ylim = yl, xlab = "Return Period",
			ylab = "Return Level", xaxt="n")
		axis(1, at=c(0.1, 1, 10, 100, 1000), labels=c("0.1", "1", "10", "100", "1000"))
		ind.f <- -1/log(f) < 10
		lines(-1/log(f), q)
		lines(-1/log(f[ind.f]), (q + kappa * sqrt(v))[ind.f], col = "blue")
		lines(-1/log(f[ind.f]), (q - kappa * sqrt(v))[ind.f], col = "blue")
		lines( rlevels, conf2[,1], col="blue")
		lines( rlevels, conf2[,2], col="blue")
		points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
		} # end of if make.plot stmt
	out$return.level <- q
	out$return.period <- -1/log(f)
	conf3 <- cbind( q-kappa*sqrt(v), q+kappa*sqrt(v))
	colnames( conf2) <- c("lower", "upper")
	colnames( conf3) <- colnames( conf2)
	out$confidence <- conf2
	out$confidence.delta <- conf3
	invisible(out)
	} else if( class(z) == "gpd.fit") {
		u <- z$threshold
		la <- z$rate
		a <- c(la, a)
		n <- z$n
		npy <- z$npy
		xdat <- z$xdata

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
		yl <- c(u, max(xdat, q[q > u - 1] + kappa * sqrt(v)[q > u - 1],na.rm=TRUE))
		conf.low <- numeric(nx)+NA
		conf.up <- conf.low
		cat("\n", "Please be patient, this may take a few seconds.  Loop is 1 to ",nx, "\n")
		options( error=expression(NULL))
		for( i in 1:nx) {
			cat(i, " ")
			if( rlevels[i] <= 100) temp <- try( gpd.parameterCI(	z=z,
										m=rlevels[i],
										rl.xlow=yl[1],
										rl.xup=yl[2],
										conf=cl,
										rl.only=TRUE))
			else temp <- try( gpd.parameterCI(	z=z,
								m=rlevels[i],
								rl.xlow=yl[1]*1.25,
								rl.xup=yl[2]*1.5,
								conf=cl,
								rl.only=TRUE))
			if( class(temp) != "try-error") {
				conf.low[i] <- temp$rl$dn
				conf.up[i] <- temp$rl$up
				}
			} # end of for 'i' loop.  
		options( error=NULL)
		cat("\n")
		conf2 <- matrix(NA, nrow=nx, ncol=2)
		ind.low <- !is.na( conf.low)
		ind.up <- !is.na( conf.up)
		if( sum( ind.low) > 3) {
			low.sfun <- splinefun( rlevels[ind.low], conf.low[ind.low])
			conf2[,1] <- low.sfun( rlevels)
			}
		if( sum( ind.up) > 3) {
			up.sfun <- splinefun( rlevels[ind.up], conf.up[ind.up])
			conf2[,2] <- up.sfun( rlevels)
			}
		if( make.plot) {
			if( any( is.na( yl))) yl <- range( q, na.rm=TRUE)
			plot(m/npy, q, log = "x", type = "n", xlim = c(0.1, max(m)/npy),
				ylim = yl, xlab = "Return period (years)", ylab = "Return level",
				xaxt="n")
			axis(1, at=c(0.1, 1, 10, 100, 1000), labels=c("0.1", "1", "10", "100", "1000"))
			lines(m[q > u - 1]/npy, q[q > u - 1])
			indy <- m[q>u-1]/npy < 10
		lines((m[q > u - 1]/npy)[indy], (q[q > u - 1] + kappa * sqrt(v)[q > u - 1])[indy], col = "blue")
		lines((m[q > u - 1]/npy)[indy], (q[q > u - 1] - kappa * sqrt(v)[q > u - 1])[indy], col = "blue")
			lines( rlevels, conf2[,1], col="blue")
			lines( rlevels, conf2[,2], col="blue")
        # points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
			nl <- n - length(dat) + 1
			sdat <- sort(xdat)
			points((1/(1 - (1:n)/(n + 1))/npy)[sdat > u], sdat[sdat > u])
			} # end of if make.plot stmt
	out$return.level <- q
	out$return.period <- m/npy
	conf3 <- cbind( q[q>u-1]-kappa*sqrt(v)[q>u-1], q[q>u-1]+kappa*sqrt(v)[q>u-1])
	colnames( conf2) <- c("lower", "upper")
	colnames( conf3) <- colnames( conf2)
	out$confidence <- conf2
	out$confidence.delta <- conf3
	invisible(out)
	} # end of what class is z stmts
} # end of return.level fcn
