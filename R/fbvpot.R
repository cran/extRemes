##
## Notes:
##
## Add a function to do a risk region.  Rectangular are easy.
##
## Add new function probRiskRegion or some such using output from fbvpot and a point corresponding to user's risk region
## to calulate the probability.

fbvpot <- function( x, threshold, dep.model = "logistic", na.action = na.fail, init = 0.5, lower = 0, upper = 1,
    cutoff, parnames, tform = "tformRankFrechet", ... ) {

    ## Notes:
    ##
    ## Add a toggle for parametric v. non-parametric transformations of marginals.
    ## Currently estimates both, but only uses non-parametric for transformation.
    ##
    ## change cutoff to be a percentage in order to use 'quantile' instead of what
    ## is currently being done.

    theCall <- match.call()

    if( missing( parnames ) ) {

	if( dep.model == "logistic" ) parnames <- "parameter"
	else if( dep.model == "mixbeta" ) parnames <- c( "nu1", "nu2", "pi1", "p1" )
	else parnames <- NULL

    } # end of if 'missing' parnames stmt.

    if( !is.null( parnames ) ) names( init ) <- parnames

    data.name <- deparse( substitute( x ) )

    x <- na.action( x )

    xdim <- dim( x )

    N <- xdim[ 1 ]
    d <- xdim[ 2 ]

    if( missing( cutoff ) ) cutoff <- floor( 0.1 * N )

    if( d != 2 ) stop( "fbvpot: invalid x argument." )

    if( !is.matrix( threshold ) ) {

        if( length( threshold ) == 1 || length( threshold ) == N ) threshold <- matrix( threshold, N, d )
        else if( length( threshold ) == d ) threshold <- matrix( threshold, N, d, byrow = TRUE )
	else stop( "fbvpot: invalid threshold argument." )

   } else if( any( dim( threshold ) != xdim ) ) stop( "fbvpot: invalid threshold argument." )

    out <- list()

    out$original.data <- x
    out$cutoff <- cutoff
    out$init <- init
    out$lower <- lower
    out$upper <- upper

    out$parnames <- parnames

    # y <- apply( x, 2, tformRankFrechet, ... ) 
    if( tform != "tf" ) y <- cbind( do.call( tform, list( x = x[,1], ... ) ), do.call( tform, list( x = x[,2], ... ) ) )
    else y <- tf( ... )

    out$Frechet.transformed.data <- y
 
    out$Frechet.transformed.data <- y


    # margs <- apply( cbind( 1:d ), 1, mfun, x = x, u = threshold, ... )

    # out$marginal.df <- margs

    rad <- apply( y, 1, sum ) 
    ang <- y[, 1] / rad 
    sl <- sort.list( rad, decreasing = TRUE ) 
    polar <- cbind( rad[ sl ], ang[ sl ] )

    out$radial <- rad
    out$angular <- ang
    out$sorting <- sl
    out$polar <- polar

    out$model <- match.fun( dep.model )
    out$model.name <- dep.model  # Maybe change this line to be deparse( substitute( dep.model ) )

    objfun <- match.fun( paste( dep.model, "LH", sep = "" ) )
    out$LH <- objfun

    fit <- nlminb( init, objfun, w = polar[ 1:cutoff, 2 ], lower = lower, upper = upper )
    # Perhaps change the above line to the following in order to avoid the above "LH" lines.
    # fit <- nlminb( init, depmodelLH, w = polar[ 1:cutoff, 2 ], lower = lower, upper = upper, model = dep.model, ... )

    if( !is.null( parnames ) ) names( fit$par ) <- parnames

    out$fit <- fit

    out$threshold <- threshold

    # derOut <- genD( objfun, polar[ 1:cutoff, 2 ], fit$par )
    # out$angular.density.h <- sqrt( 1 / derOut$D[ 2 ] )

    out$call <- theCall
    out$data.name <- data.name

    class( out ) <- "fbvpot"

    return( out )

} # end of 'fbvpot' function.

bvpotbooter <- function(  object, B, rsize,  block.length = 1, shuffle = NULL, replace = TRUE, ... ) {

    bvstat <- function( data, ..., obj ) {

	# obj <- list( ... )

	d <- dim( data )[ 2 ]

	hold <- fbvpot( x = data[ , 1:2 ], threshold = data[ , (d - 1):d ], dep.model = obj$model.name, init = obj$init,
    		    lower = obj$lower, upper = obj$upper, cutoff = obj$cutoff, ... ) 

	return( hold$fit$par )

    } # end of internal 'bvstat' function.

    out <- booter( x = cbind( object$original.data, object$threshold ), statistic = bvstat, B = B,
		block.length = block.length, shuffle = shuffle, replace = replace, obj = object, ... ) 

    return( out )

} # end of 'bvpotbooter' function.

print.fbvpot <- function( x, ... ) {

    cat( "Call:\n" )
    print( x$call )
    print( x$data.name )
    cat("\nBivariate Peak Over Threshold Model with ", x$model.name, " dependence.\n" )

    cat( "\nEstimated Parameter(s):\n")
    print( x$fit$par )

    invisible()

} # end of 'print.fbvpot' function.

plot.fbvpot <- function( x, ..., type = c( "default", "orig", "tf", "hist" ) ) {

    type <- tolower( type )
    type <- match.arg( type )

    if( type == "default" ) {

	op <- par()
	par( mfrow = c( 2, 2 ), oma = c( 0, 2, 0, 0 ) )

    }

    y <- x$Frechet.transformed.data

    N <- dim( y )[ 1 ]

    cutoff <- x$cutoff

    if( type == "default" || type == "orig" ) plot( x$original.data, main = "Original Data", ... )

    if( type == "default" || type == "tf" ) {

	z <- y[ x$sorting, ]
	xl <- range( c( 0, c( z ) ), finite = TRUE )
	plot( z[ ( cutoff + 1 ):N, ], xlim = xl, ylim = xl, pch = ".", main = "Frechet Transformed Data", ... )
	abline(h = 0, lty = 3)
	abline(v = 0, lty = 3)
	lines( c( 0, x$polar[ cutoff, 1 ] ), c( x$polar[ cutoff, 1 ], 0 ) )
	points( z[ 1:cutoff, ], col = "darkblue", ... )
	for( i in c( 1:9, 11 ) ) lines( c( 0, z[ i, 1 ] ), c( 0, z[ i, 2 ] ), lty = 2, col = "darkblue" )

    }

    if( type == "default" || type == "hist" ) hist( x, col = "blue", main = "Histogram", ... )

    if( type == "default" ) {

	par( mfrow = op$mfrow )
	mtext( deparse( x$call ), line = 0.5, outer = TRUE )

    }

    invisible()

} # end of 'plot.fbvpot' function.

hist.fbvpot <- function( x, ... ) {

    ## Notes:
    ## Allow user to change breaks.
    ##

    w <- x$polar[, 2 ]

    N <- length( w )

    cutoff <- x$cutoff

    args <- list( ... )

    xlb <- paste( "Angular Component\n", "(Highest ", cutoff, " values)", sep = "" )

    if( is.null( args$main ) ) {

	res <- hist( w[ 1:cutoff ], breaks = seq(0, 1, .1), freq = FALSE,
                xlab = xlb, main = "Histogram", ... )

    } else {

	res <- hist( w[ 1:cutoff ], breaks = seq(0, 1, .1), freq = FALSE,
		xlab = xlb, ... )

    }

    Model <- do.call( x$model.name, list( w = seq(0, 1, .01), x$fit$par ) )
    lines( seq(0, 1, .01), Model, col = "darkblue" )

    invisible( res )

} # end of 'hist.fbvpot' function.


