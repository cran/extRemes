logistic <- function( w, p, ... ) {

    # Logistic likelihood.

    h <- 0.5 * ( p^(-1) - 1 ) * ( ( w * (1 - w) )^(-1 - p^(-1) ) ) * 
	    ( ( w^ (-1 / p) + ( 1 - w )^( -1 / p ) )^( p - 2 ) )

    return( h )

} # end of 'logistic' function.

logisticLH <- function( w, p, ... ) {

    h <- logistic( w = w, p = p )

    return( -sum( log( h ) ) )

} # end of 'logisticLH' function.

mixbeta <- function( w, p, ... ) {

        nu1 <- p[1]
        nu2 <- p[2]
	pi1 <- p[3]
	p1 <-  p[4]

        pi2 <- (1/2 - p1*pi1)/(1 - p1)

        h1 <- p1 * dbeta( w, shape1 = nu1 * pi1, shape2 = nu1 * ( 1 - pi1 ) )
        h2 <- ( 1 - p1 ) * dbeta( w, shape1 = nu2 * pi2, shape2 = nu2 * ( 1 - pi2 ) )

        return(h1 + h2)

} # end of 'mixbeta' function.

mixbetaLH <- function( w, p, ... ) {

    h <- mixbeta( w = w, p = p )

    return( -sum( log( h ) ) )

} # end of 'mixbetaLH' function.

##
## Maybe we should just have the user make their own dependence function
## that outputs 'h', and then we would just call the following function?
##

# depmodelLH <- function( w, p, model = "logistic", ... ) {

 #    h <- do.call( model, c( list( w = w, p = p ), list( ... ) ) )

  #   return( -sum( log( h ) ) )

# } # end of 'depmodelLH' function.
