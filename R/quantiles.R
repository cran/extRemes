quantilefun =
    function(y)
    approxfun(seq(0, 1, length = length(y)), sort(y),
              yleft = NA, yright = NA)

percentilefun =
    function(y)
    approxfun(sort(y), seq(0, 1, length = length(y)),
              yleft = 0, yright = 1)

qqnorm =
    function(y, pch = 20,
             xlab = "Standard Normal Quantiles",
             ylab = "Sample Quantiles", ...)
    {
        y = sort(na.omit(y))
        n = length(y)
        p = (1:length(y) - .5)/length(y)
        k = .895 / (sqrt(n) * (1 - .01 / sqrt(n) + .85 / n))
        l = suppressWarnings(qnorm(p - k))
        q = qnorm(p)
        u = suppressWarnings(qnorm(p + k))
        plot(q, y, xlim = range(l, q, u, na.rm = TRUE),
             xlab = xlab, ylab = ylab, pch = pch, ...)
        lines(l, y, col = "darkgray")
        lines(u, y, col = "darkgray")
    }

qqplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = 1.36
        p = (1:m - 1)/(m - 1)
        yq = qy(p)
        yl = qy(p - K/sqrt(M))
        yu = qy(p + K/sqrt(M))
        plot(x, yq, pch = pch,
             xlim = range(x),
             ylim = range(yq, yl, yu, na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "gray")
        lines(x, yu, col = "gray")
    }

shiftplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = 1.36
        p = (1:m - 1)/(m - 1)
        yq = qy(p) - x
        yl = qy(p - K/sqrt(M)) - x
        yu = qy(p + K/sqrt(M)) - x
        plot(x, yq, pch = pch,
             xlim = range(x),
             ylim = range(yq, yl, yu, na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "darkgray")
        lines(x, yu, col = "darkgray")
    }

# qqevd <- function( object, conf=NULL, ...) {
#    if( class( object) == "gev.fit") {
# 	n <- length( object$data)
# 	x <- (1:n)/(n + 1)
# 	if( object$trans) {
# 	   plot( -log(-log(x)), sort( object$data), xlab="Empirical", ylab="Model",
# 			main="Residual Quantile Plot (Gumbel Scale)", ...)
# 	   abline(0,1,col=4,lwd=1.5)
# 	   if( !is.null( conf)) {
# 		## Need to make a change in ismev to allow one to know which are the parameter values
# 		## for the case where all covariates are zero!  Then, change mle below accordingly.
# 		d <- gev.rl.gradient( a=object$mle, p=1-1/(log(x)))
# 		v <- apply( d, 1, q.form, m=object$cov)
# 	   }
# 	} # end of if 'trans' stmts.
#    } else if( class( object) == "gpd.fit") {
# 
#    } else {
# 
#    }
# } # end of 'qqevd' function.
