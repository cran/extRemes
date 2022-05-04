xtibber <-
function( x, type = c( "return.level", "parameter" ), which.one, tib.method = c( "interp", "rm" ),
    nuisance = "shape", B, test.pars, rsize, block.length = 1, shuffle = NULL, replace = TRUE,
    alpha = 0.05, qcov = NULL, qcov.base = NULL, stud = FALSE, step.size, 
    tol = 1e-04, max.iter = 1000, keep.iters = TRUE, verbose = FALSE, ...) {

    # If type is "parameter", then 'which.one' specifies the
    # position in strip( x ) of the parameter,
    # else it specifies the return period.

    theCall <- match.call()

    type <- tolower( type ) 
    type <- match.arg( type )

    tib.method <- tolower( tib.method )
    tib.method <- match.arg( tib.method )

    # if( type == "return.level"  && !is.fixedfevd( x ) ) stop( "xtibber: cannot (yet) handle return levels when parameters vary." )

    thresh <- x$threshold
    pars <- strip( x )
    pnames <- names( pars )
    np <- length( pars )

    # nuisance <- match.arg( nuisance )
    if( type == "parameter" && nuisance == pnames[ which.one ] ) stop( "xtibber: invalid nuisance argument. Must differ from parameter of interest." )

    fid <- ( 1:np )[ pnames == nuisance ]

    designs <- setup.design( x )
    xdat <- datagrabber( x )

#     if( x$data.name[2] == "" ) {
# 
#         y <- xdat
#         cdat <- NULL
# 
#     } else {
# 
#         y <- xdat[,1]
#         cdat <- xdat[,-1]
# 
#     } # end of get the data sorted situation stmts.

    # dotlist <- list( ipars = ipars, obj = x, designs = designs, cdat = cdat, fid = fid, pnames = pnames, np = np,
    # 			do.var = stud, rperiod = which.one, qcov = qcov, qcov.base = qcov.base )

    args <- list( ... )

    if( !is.null( args$lower ) ) dotlist$lower <- args$lower
    if( !is.null( args$upper ) ) dotlist$upper <- args$upper

    if( missing( rsize ) ) rsize <- x$n

    rmod <- function( data, par, n, ... ) {

	a <- list( ... )

	ipars <- a$ipars
	object <- a$obj 
	des <- a$designs 
	cdat <- data[,-1]
 	if( length( cdat ) == 0 ) cdat <- NULL
	data <- data[,1]
	fid <- a$fid 
	pnames <- a$pnames
	np <- a$np

	which.par <- a$id # to be the fixed index for the nuisance parameter.

	if( is.null( a$lower ) ) lower <- -Inf
	else lower <- a$lower

	if( is.null( a$upper ) ) upper <- Inf
	else upper <- a$upper

	hold <- try( nlminb( start = ipars, objective = oevd.profpar, o = object, 
            des = des, x = data, data = cdat, u = object$threshold, fixed.index = fid, 
            fixed.value = par, which.type = "parameter",
            lower = lower, upper = upper ) )

	hc <- class( hold )
	if( !( "try-error" %in% hc ) ) p <- hold$par
	else {

	    # TO DO: What to do if the fit above doesn't work?
	    # For now, returning vector of NA's.  Need to account for
	    # possibility in 'statistic'.
	    warning( "xtibber: failed to fit model to randomly generated data." )
	    return( rep( NA, n ) )

	}

	if( is.null( names( p ) ) ) names( p ) <- pnames

	obj2 <- object
	if( is.element( obj2$method, c( "MLE", "GMLE" ) ) ) obj2$results$par <- p
	else if( obj2$method == "Lmoments" ) obj2$results <- p

	if( length( cdat ) > 0 ) out <- data.frame( y = rextRemes( obj2 ), cdat )
	else out <- c( rextRemes( obj2 ) )

	return( out )

    } # end of internal 'rmod' function.

    st <- function( data, ... ) {

	a <- list( ... )
	object <- a$obj
	np <- a$np
	pm <- object$par.models
	do.var <- a$do.var
	rperiod <- a$rperiod
	qc <- a$qcov
	qcb <- a$qcov.base

	if( all( is.na( data ) ) ) return( rep( NA, np ) )

	y <- datagrabber( object )
	ydim <- dim( y )
	if( is.null( ydim ) || ydim[ 2 ] == 1 ) {

	    fit <- try( fevd( x = data, threshold = object$threshold, use.phi = pm$log.scale,
			type = object$type, method = object$method, period.basis = object$period.basis,
			optim.args = object$optim.args, priorFun = object$priorFun,
            		priorParams = object$priorParams ) )

	} else {

	    fit <- try( fevd( x = c(data[,1]), data = data, threshold = object$threshold, location.fun = pm$location,
			scale.fun = pm$scale, shape.fun = pm$shape, use.phi = pm$log.scale,
			type = object$type, method = object$method, period.basis = object$period.basis, 
                        optim.args = object$optim.args, priorFun = object$priorFun,
                        priorParams = object$priorParams ) )

	}

	cf <- class( fit )
	if( "try-error" %in% cf ) return( rep( NA, np ) )

	sumobj <- summary( fit, silent = TRUE )

	if( type == "parameter" ) {

	    res <- strip( fit )[ which.one ]
	    if( do.var ) res <- c( res, diag( sumobj$cov.theta )[ which.one ] )

	} else {

	    res <- c( return.level( fit, return.period = rperiod, qcov = qc, qcov.base = qcb ) )
	    if( do.var ) res <- c( res, rlvar( fit, return.period = rperiod, theta = sumobj, qcov = qc, qcov.base = qcb ) )

	}

	return( res )

    } # end of internal 'st' function.

    if( tib.method == "interp" ) {

	if( !stud ) {

	    res <- tibber( x = xdat, statistic = st, B = B, rmodel = rmod, test.pars = test.pars,
		    rsize = rsize, block.length = block.length, shuffle = shuffle, replace = replace,
		    alpha = alpha, verbose = verbose, ipars = strip( x, use.names = FALSE ), obj = x, designs = designs,
		    fid = fid, pnames = pnames, np = np, do.var = stud, rperiod = which.one,
		    qcov = qcov, qcov.base = qcov.base )

	} else {

	    res <- tibber( x = xdat, statistic = st, B = B, rmodel = rmod, test.pars = test.pars,
                    rsize = rsize, block.length = block.length, v.terms = 2, shuffle = shuffle, replace = replace,
                    alpha = alpha, verbose = verbose, ipars = strip( x, use.names = FALSE ), obj = x, designs = designs,
                    fid = fid, pnames = pnames, np = np, do.var = stud, rperiod = which.one,
                    qcov = qcov, qcov.base = qcov.base )

	}

	if( is.matrix( res ) || is.vector( res ) ) return( res )

    } else if( tib.method == "rm" ) {

	if( !stud ) {

            res <- tibberRM( x = xdat, statistic = st, B = B, rmodel = rmod, startval = test.pars,
                    rsize = rsize, block.length = block.length, shuffle = shuffle, replace = replace,
		    step.size = step.size, tol = tol, max.iter = max.iter, keep.iters = keep.iters,
                    alpha = alpha, verbose = verbose, ipars = strip( x, use.names = FALSE ), obj = x, designs = designs,
                    fid = fid, pnames = pnames, np = np, do.var = stud, rperiod = which.one,
                    qcov = qcov, qcov.base = qcov.base )

        } else {

            res <- tibberRM( x = xdat, statistic = st, B = B, rmodel = rmod, startval = test.pars,
                    rsize = rsize, block.length = block.length, v.terms = 2, shuffle = shuffle, replace = replace,
		    step.size = step.size, tol = tol, max.iter = max.iter, keep.iters = keep.iters,
                    alpha = alpha, verbose = verbose, ipars = strip( x, use.names = FALSE ), obj = x, designs = designs,
                    fid = fid, pnames = pnames, np = np, do.var = stud, rperiod = which.one,
                    qcov = qcov, qcov.base = qcov.base )

        }

    }

    out <- list()
    out$call <- theCall
    out$results <- res
    if( type == "parameter" ) out$parameter <- pnames[ which.one ]
    else out$parameter <- paste( which.one, "-", x$period.basis, " return level" )

    class( out ) <- "xtibbed"

    return( out )

}
plot.xtibbed <-
function( x, ... ) {

    plot( x$results, ... )

    invisible()

}
print.xtibbed <-
function( x, ... ) {

    cat("Call:\n")
    print( x$call )

    print( x$parameter )

    print( x$results )

    invisible()

}
