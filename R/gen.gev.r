"gen.gev" <-
function(p, n, trend=NULL) {

# simulates gev data using an exponential
# p is c(mu, sigma, gamma)
# routine by GY

if( p[3] != 0) {
# Generate from a GEV distribution (Frechet or Weibull)
#	x <- rexp(n)
#	if( is.null(trend)) gev.dat <- p[1] + p[2]*(x^(-p[3])-1)/p[3]
	if( is.null(trend))
		gev.dat <- p[1] + p[2]*((-log(runif(n)))^(-p[3])-1)/p[3]
	else {

    # generate gev with a linear trend in mu
    gev.dat<-numeric(n)
# for( i in 1:n) gev.dat[i] <- p[1]+i*trend+p[2]*(x[i]^(-p[3])-1)/p[3]
for( i in 1:n)
	gev.dat[i] <- p[1]+i*trend+p[2]*((-log(runif(n)[i]))^(-p[3])-1)/p[3]
		}
} else {
	# Generate from a Gumbel distribution
	if( is.null( trend)) gev.dat <- p[1] - p[2]*log( -log( runif(n)))
	else {
		gev.dat <- numeric(n)
for( i in 1:n) gev.dat[i] <- p[1] + i*trend - p[2]*log( -log( runif(n)[i]))
		}
	} # end of if else p is not 0 stmt
return( gev.dat)
} # end of gen.gev fcn
