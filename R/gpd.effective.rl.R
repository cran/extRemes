gpd.effective.rl <- function(models, data, return.period=100, plot=TRUE, ...) {
   ##
   ## Function to cacluate the effective return levels for a GEV model.
   ##
   ## Arguments:
   ## 
   ## 'models' a list giving functions (or constants) for each of the parameter models for the GEV df.
   ##	Should have names 'threshold', 'scale', 'shape' and/or 'lambda'.  Each function should have an argument 'x' as input,
   ##	which will have 'data' passed as this argument, and the functions should return a numeric vector of length
   ##	equal to the length of the covariate arguments, if not a constant value.  If a component is NULL, then the
   ##	associated parameter should be constant and passed by way of '...' as "u", "sigma", "xi", and "lambda", respectively.
   ## 'data' a data frame or list whose named components are used by the function(s) in 'model'.  All covariates must be
   ##	vectors of identical length to each other.  Lists might make use of the parameter values (e.g., mu0=0.5, etc.).
   ## 'return.period' numeric giving the value of the return period for which to calculate the effective return level.
   ##	Only one value is allowed.
   ## 'plot' logical, if TRUE a plot of the effective return levels is created.  The plot may or may
   ##	not make any sense depending on the covariates, functions, etc.
   ## '...' optional arguments to functions in model (e.g., sig0=0.5, sig1=0.3, etc.).
   ##
   ## Value: a numeric vector (or matrix) is returned invisibly supplying all desired information (i.e., effective
   ##	return levels and confidence intervals, etc.).  Otherwise, if 'plot' is TRUE, some kind of plot is created.
   ##
   # if( return.period < 1) stop("gpd.effective.rl: sorry, cannot handle return periods < 1 year.")
   # a <- list(...)
   # print( names( args))
   n <- dim( data)[1]
   rl <- numeric(n) + NA
   if( is.function( models$threshold)) thresh <- models$threshold
   else {
	thresh <- function(x,...) {
	   a <- list(...)
	   out <- rep( a$u, n)
	   return( out)
	} # end of 'loc' internal function.
   }  # end of if 'const' term stmts.
   if( is.function( models$scale)) sc <- models$sc
   else {
	sc <- function(x,...) {
	   a <- list(...)
	   
	   out <- rep( a$sigma, n)
	   return( out)
	} # end of 'sc' internal function.
   } # end of if 'const' term stmts.
   if( is.function( models$shape)) sh <- models$shape
   else {
	sh <- function(x,...) {
	   a <- list(...)
	   out <- rep( a$xi, n)
	   return( out)
	} # end of 'sh' internal function.
   } # end of if 'const' term stmts.
   if( is.function( models$lambda)) lam <- models$lambda
   else {
        lam <- function(x,...) {
           a <- list(...)
           out <- rep( a$lambda, n)
           return( out)
        } # end of 'lam' internal function.
   } # end of if 'const' term stmts.

   if( !is.null( models$conf)) conf <- models$conf
   else conf <- 0.95

   # Calculate a single parameter value for each covariate.
   seuil <- do.call( thresh, list(x=data, ...))
   sig <- do.call( sc, list( x=data, ...))
   xi <- do.call( sh, list( x=data, ...))
   lam <- do.call( lam, list( x=data, ...))

   # Calculate the return levels for each parameter value.
   zeta <- return.period*lam
   xi.zero <- xi == 0
   if( any( xi.zero)) rl[ xi.zero] <- seuil[ xi.zero] + sig[ xi.zero]*log( zeta)
   if( any( !xi.zero)) rl[ !xi.zero] <- seuil[ !xi.zero] + (sig[ !xi.zero]/xi[ !xi.zero])*(zeta^(xi[ !xi.zero])-1)
   browser()
   if( plot) plot( 1:n, rl, xlab="index", ylab=paste(return.period, "-year effective return levels", sep=""))
   invisible( rl)
}
