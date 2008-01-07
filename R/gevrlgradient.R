gevrlgradient <- function(z,p) {
##
## Function to compute the gradient for the GEV return level.
## Assumes that no covariates are used inthe parameters.
##
## 'z' list object as output from the 'gev.fit' function of the 'ismev' package.
## 'p' is a vector of the 1/p return level(s) (0 <= p < 1).
## Value: returns a '3 X n_p' matrix of the gradient values: (delta z_p/delta mu, delta z_p/delta sigma, delta z_p/delta xi)
##

scale <- z$mle[2]
shape <- z$mle[3]
if( shape < 0) zero.p <- p==0 
else zero.p <- logical( length(p))
out <- matrix( NA, nrow=3, ncol=length( p))
out[1,] <- 1
if( any( zero.p)) {
   out[2,zero.p & !is.na( zero.p)] <- rep( -shape^(-1), sum( zero.p,na.rm=TRUE))
   out[3,zero.p & !is.na( zero.p)] <- rep( scale*(shape^(-2)), sum( zero.p,na.rm=TRUE))
}
if( any( !zero.p)) {
   yp <- -log(1-p[!zero.p])
   out[2,!zero.p] <- -shape^(-1)*(1-yp^(-shape))
   out[3,!zero.p] <- scale*(shape^(-2))*(1-yp^(-shape)) - scale*shape^(-1)*yp^(-shape)*log(yp)
}
return( out)
}
