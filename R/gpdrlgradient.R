gpdrlgradient <- function(z,m) {
##
## Function to compute the gradients for the GPD return level delta method CIs.
##
## 'z' list object output from the 'ismev' function 'gpd.fit'.  No covariates in the parameters are
##	allowed.
## 'm' the 'N X npy' return period(s) (i.e., the 'N-year' return period equivalent).
##
## Value: a '3 X n_m' matrix with rows giving the gpd rl gradients for each value of m (columns).
##
a <- c( z$rate, z$mle)
if( length( a) > 3) stop("gpdrlgradient: Covariates not allowed.")
out <- matrix( 0, nrow=3, ncol=length( m))
if( a[3] == 0) {
   out[1,] <- a[2]/a[1]
   out[2,] <- log(m*a[1])
} else {
   out[1,] <- a[2]*m^(a[3])*a[1]^(a[3]-1)
   out[2,] <- a[3]^(-1)*((m*a[1])^(a[3])-1)
   out[3,] <- -a[2]*a[3]^(-2)*((m*a[1])^(a[3])-1) + a[2]*a[3]^(-1)*(m*a[1])^(a[3])*log(m*a[1])
}
return( out)
}
