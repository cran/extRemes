tf <- function( fit, type = "Frechet", ... ) {

    #
    # Transform data from an 'fevd' object or list of 'fevd' objects.
    # First transform to exponential(1) for x > u and then perform
    # the Laplace transformation.
    #
    # The fitted functions should be GP or PP.
    #

    tfun <- function( x, ... ) {

	xAbove <- trans( x, ... )
	xAbove <- exp( -xAbove )

	dat <- datagrabber( x, cov.data = FALSE )
	Fn <- ecdf( dat )
	xBelow <- Fn( dat )[ dat <= x$threshold ]
	# xAbove <- 1 - ( 1 - Fn( dat )[ dat > x$threshold ] ) * xAbove
	xAbove <- 1 - ( 1 - Fn( x$threshold[ dat > x$threshold ] ) ) * xAbove

	res <- numeric( x$n )

	res[ dat > x$threshold ] <- xAbove
	res[ dat <= x$threshold ] <- xBelow

	return( res )

    } # end of internal 'tfun' function.

    if( class( fit ) == "fevd" ) out <- tfun( fit, ... )
    else out <- lapply( fit, tfun, ... )

    out <- cbind( out[[ 1 ]], out[[ 2 ]] )

    if( type == "Frechet" ) out <- -1 / log( out )
    else stop( "tf: sorry, only Frechet transform currently available." )

    return( out )

} # end of 'tf' function.

