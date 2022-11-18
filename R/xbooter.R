xbooterBM <- function( data, fevd.object, return.period, qcov, qcov.base, init, np, ... ) {

    pm <- fevd.object$par.models
    do.var <- fevd.object$method != "Lmoments"
    do.rl  <- length( return.period ) > 0

    xdim <- dim( data )

    if( !is.null( xdim ) ) {

        fit <- try( fevd( x = data[, 1 ], data = data, location.fun = pm$location, scale.fun = pm$scale,
	    shape.fun = pm$shape, use.phi = pm$log.scale, type = fevd.object$type,
	    method = fevd.object$method, initial = init, period.basis = fevd.object$period.basis,
	    optim.args = fevd.object$optim.args, priorFun = fevd.object$priorFun,
	    priorParams = fevd.object$priorParams ) )

    } else {

	fit <- try( fevd( x = data, use.phi = pm$log.scale, type = fevd.object$type,
            method = fevd.object$method, initial = init, period.basis = fevd.object$period.basis,
            optim.args = fevd.object$optim.args, priorFun = fevd.object$priorFun,
            priorParams = fevd.object$priorParams ) )

    } # end of whether or not there is a 'data' argument stmt.

    if( is( fit, "try-error" ) ) return( rep( NA, np ) )

    nomen <- fevd.object$parnames
    res <- c( distill( fit, cov = FALSE ) )[ 1:length( nomen ) ]

    if( do.rl ) {

	res2 <- return.level( fit, return.period = return.period, qcov = qcov, qcov.base = qcov.base )

	if( !is.matrix( res2 ) ) {

            res <- c( res, res2 )
            nomen <- c( nomen, paste( return.period, "-", fevd.object$period.basis, " ret. lvl.", sep = "" ) )

        } else {

	    rldim <- dim( res2 )
	    res <- c( res, c( res2 ) )
	    nomen <- c( nomen, paste( rep( paste( "erl", return.period, sep = "." ), each = rldim[ 1 ] ),
                                    rep( paste( "t", 1:rldim[ 1 ], sep = "" ), rldim[ 2 ] ), sep = "." ) )


	}

	names( res ) <- nomen

    } # end of if do return levels or not stmt.

    if( do.var ) {

        sumobj <- summary( fit, silent = TRUE )
        varres <- diag( sumobj$cov.theta )

        if( do.rl ) {

            varres2 <- rlvar( fit, return.period = return.period, theta = sumobj,
                            qcov = qcov, qcov.base = qcov.base )

            varres <- c( varres, varres2 )

        } # end of if do return level estimates too stmt.

	res <- c( res, varres )
	nomen <- c( nomen, paste( "var(", nomen, ")", sep = "" ) )
	names( res ) <- nomen

    }

    # TO DO: should scale be converted or not.  If so, check variance to make sure it is correct.
    #     res[ nomen == "log.scale" ] <- exp( res[ nomen == "log.scale" ] )
    #     nomen[ nomen == "log.scale" ] <- "scale"

    return( res )

} # end of 'xbooterBM' function.

xbooterPOT <- function( data, fevd.object, return.period, qcov, qcov.base, init, np, ... ) {

    pm <- fevd.object$par.models
    do.var <- fevd.object$method != "Lmoments"
    do.rl  <- length( return.period ) > 0

    xdim <- dim( data )

    if( is.data.frame( data ) && !is.null( data$threshold ) ) u <- data$threshold
    else u <- fevd.object$threshold

    if( !is.null( dim( data ) ) ) {

        fit <- try( fevd( x = data[, 1 ], threshold = u, data = data, threshold.fun = pm$threshold,
	    location.fun = pm$location, scale.fun = pm$scale,
            shape.fun = pm$shape, use.phi = pm$log.scale, type = fevd.object$type,
            method = fevd.object$method, initial = init, period.basis = fevd.object$period.basis,
            optim.args = fevd.object$optim.args, priorFun = fevd.object$priorFun,
            priorParams = fevd.object$priorParams ) )

    } else {

	fit <- try( fevd( x = data, threshold = u, threshold.fun = pm$threshold,
            location.fun = pm$location, scale.fun = pm$scale,
            shape.fun = pm$shape, use.phi = pm$log.scale, type = fevd.object$type,
            method = fevd.object$method, initial = init, period.basis = fevd.object$period.basis,
            optim.args = fevd.object$optim.args, priorFun = fevd.object$priorFun,
            priorParams = fevd.object$priorParams ) )

    }

    if( is( fit, "try-error" ) ) return( rep( NA, np ) )

    nomen <- fevd.object$parnames
    res <- c( distill( fit, cov = FALSE ) )[ 1:length( nomen ) ]

    if( do.rl ) {

        res2 <- return.level( fit, return.period = return.period, qcov = qcov, qcov.base = qcov.base )

        if( !is.matrix( res2 ) ) {

            res <- c( res, res2 )
            nomen <- c( nomen, paste( return.period, "-", fevd.object$period.basis, " ret. lvl.", sep = "" ) )

        } else {

            rldim <- dim( res2 )
            res <- c( res, c( res2 ) )
            nomen <- c( nomen, paste( rep( paste( "erl", return.period, sep = "." ), each = rldim[ 1 ] ),
                                    rep( paste( "t", 1:rldim[ 1 ], sep = "" ), rldim[ 2 ] ), sep = "." ) )


        }

        names( res ) <- nomen

    } # end of if do return levels or not stmt.

    if( do.var ) {

        sumobj <- summary( fit, silent = TRUE )
        varres <- diag( sumobj$cov.theta )

        if( do.rl ) {

            varres2 <- rlvar( fit, return.period = return.period, theta = sumobj,
                            qcov = qcov, qcov.base = qcov.base )

            varres <- c( varres, varres2 )

        } # end of if do return level estimates too stmt.

        res <- c( res, varres )
        nomen <- c( nomen, paste( "var(", nomen, ")", sep = "" ) )
        names( res ) <- nomen

    }

    # TO DO: should scale be converted or not.  If so, check variance to make sure it is correct.
    #     res[ nomen == "log.scale" ] <- exp( res[ nomen == "log.scale" ] )
    #     nomen[ nomen == "log.scale" ] <- "scale"

    return( res )

} # end of 'xbooterPOT' function.

xbooter <- function( x, B, rsize, block.length = 1, 
    return.period = c( 10, 20, 50, 100, 200, 500 ), qcov = NULL, qcov.base = NULL,
    shuffle = NULL, replace = TRUE, verbose = FALSE, ... ) {

    if( verbose ) begin.tiid <- Sys.time()

    if( x$method == "Bayesian" ) stop( "xbooter: invalid x argument." )

    if( !is.fixedfevd( x ) && is.null( qcov ) ) {

	if( !missing( return.period ) ) stop( "xbooter: Must use qcov argument for models with varying parameters." )
	else return.period <- numeric( 0 )

    }

    theCall <- match.call()

    if( !is.null( qcov.base ) ) warning( "xbooter: this function has not been tested with qcov.base!" )

    if( missing( rsize ) ) rsize <- x$n
    if( rsize > x$n || rsize < 1 ) stop( "xbooter: invalid rsize argument." )

    # Obtain initial estimate information.
    if( x$method == "Lmoments" ) {

        theta.hat <- x$results
        theta.names <- names( theta.hat )

    } else {

        theta.hat <- x$results$par
        theta.names <- names( theta.hat )

    }

    ipar <- list()
    if (any(is.element(c("location", "mu0"), theta.names))) {
        if (is.element("location", theta.names))
            ipar$location <- theta.hat["location"]
        else {
            id <- substring(theta.names, 1, 2) == "mu"
            ipar$location <- theta.hat[id]
            }
        }
        if (is.element("scale", theta.names))
            ipar$scale <- theta.hat["scale"]
        else {
            if (!x$par.models$log.scale)
                id <- substring(theta.names, 1, 3) == "sig"
            else id <- substring(theta.names, 1, 3) == "phi"
            ipar$scale <- theta.hat[id]
        }
        if (!is.element(x$type, c("Gumbel", "Exponential"))) {
            if (is.element("shape", theta.names))
                ipar$shape <- theta.hat["shape"]
        else {
           id <- substring(theta.names, 1, 2) == "xi"
           ipar$shape <- theta.hat[id]
        }
    }

    npar <- length( x$parnames )
    if( is.fixedfevd( x ) ) {

	npar <- npar + length( return.period )
	bigD <- x$x

    } else {

	npar <- npar + length( return.period ) * nrow( qcov )
	# dnames <- names( x$data )
	bigD <- data.frame( x$x, x$cov.data )
	# names( bigD ) <- c( "V1", dnames )
	if( !is.null( x$threshold ) && length( x$threshold ) > 1 ) bigD <- data.frame( bigD, threshold = x$threshold )

    }

    if( is.element( x$type, c( "GP", "PP", "Exponential" ) ) ) {

	sz <- rsize / block.length

        ind <- matrix( rpois( sz * B, lambda = x$rate ), sz, B )
	ind[ ind > 0 ] <- 1
        exceeds <- x$x > x$threshold
        id1 <- sample( (1:x$n)[ exceeds ], size = sz * B, replace = replace )
        id2 <- sample( (1:x$n)[ !exceeds ], size = sz * B, replace = replace )
        id <- id1 * ind + id2 * ( 1 - ind )

        if( block.length > 1 ) {

            bmaker <- function( x, b ) return( apply( cbind( x ), 1, function( x ) return( c( x:( x + b ) ) ) ) )
            id <- apply( id, 2, bmaker, b = block.length )
            if( any( id > x$n ) ) id[ id > x$n ] <- id[ id > x$n ] - x$n

        } # end of if 'block.length' > 1 stmt.

	shuffle.indices <- id

    } # end of if method is POT stmt.

    if( x$method != "Lmoments" ) {

	vt <- (npar + 1):( 2 * npar )
	npar <- npar * 2

	if( is.element( x$type, c( "GEV", "Gumbel" ) ) ) {

	    out <- booter( x = bigD, statistic = xbooterBM, B = B, rsize = rsize,
			block.length = block.length, v.terms = vt, replace = replace,
			fevd.object = x, return.period = return.period,
			qcov = qcov, qcov.base = qcov.base, init = ipar, np = npar, ... )

	} else {

	    if( !missing( block.length ) ) {

	        out <- booter( x = bigD, statistic = xbooterPOT, B = B, rsize = rsize,
                        block.length = block.length, v.terms = vt, shuffle = shuffle.indices,
			replace = replace, fevd.object = x, return.period = return.period,
                        qcov = qcov, qcov.base = qcov.base, init = ipar, np = npar, ... )

	    } else {

		out <- booter( x = bigD, statistic = xbooterPOT, B = B, rsize = rsize,
                        v.terms = vt, shuffle = shuffle.indices, replace = replace,
			fevd.object = x, return.period = return.period,
                        qcov = qcov, qcov.base = qcov.base, init = ipar, np = npar, ... )

	    } # end of whether to use block.length argument or not stmt.

	} # end of if else do BM or POT method stmts.

    } else {

	if( is.element( x$type, c( "GEV", "Gumbel" ) ) ) {

            out <- booter( x = bigD, statistic = xbooterBM, B = B, rsize = rsize,
                        block.length = block.length, replace = replace,
                        fevd.object = x, return.period = return.period,
                        qcov = qcov, qcov.base = qcov.base, init = ipar, np = npar, ... )

        } else {

	    if( !missing( block.length ) ) {

                out <- booter( x = bigD, statistic = xbooterPOT, B = B, rsize = rsize,
                        block.length = block.length, shuffle = shuffle.indices,
			replace = replace, fevd.object = x, return.period = return.period,
                        qcov = qcov, qcov.base = qcov.base, init = ipar, np = npar, ... )

	    } else {

		out <- booter( x = bigD, statistic = xbooterPOT, B = B, rsize = rsize,
                        shuffle = shuffle.indices, replace = replace, fevd.object = x,
			return.period = return.period, qcov = qcov, qcov.base = qcov.base,
			init = ipar, np = npar, ... )

	    } # end of whether to use block.length argument or not stmt.

        } # end of if else do BM or POT method stmts.

    } # end of if else compute variance terms stmt.

    if( verbose ) print( Sys.time() - begin.tiid )

    out$call <- theCall

    return( out )

} # end of 'xbooter' function.

rlvar <- function( x, return.period, theta = NULL, qcov = NULL, qcov.base = NULL ) {

    if( is.null( theta ) ) theta <- summary( x, silent = TRUE )

    grads <- t( rlgrad.fevd( x, period = return.period, qcov = qcov, qcov.base = qcov.base ) )

    cov.theta <- theta$cov.theta

    if ( is.element( x$type, c( "GP", "Beta", "Pareto", "Exponential" ) ) ) {

        lam <- x$rate
        var.theta <- diag( cov.theta )

        if (x$type == "Exponential") cov.theta <- diag( c( lam * (1 - lam) / x$n, var.theta ) )
        else cov.theta <- rbind( c( lam * ( 1 - lam ) / x$n, 0, 0 ), cbind( 0, cov.theta ) )

    }

    res <- diag( t(grads) %*% cov.theta %*% grads )
    names( res ) <- paste( "var(", return.period, "-", x$period.basis, " return level)" )

    return( res )

} # end of 'rlvar' function.

