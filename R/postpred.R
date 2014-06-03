postpred <- function(x, return.period = 100, ...) {

    UseMethod("postpred", x)

} # end of 'postpred' function.

postpred.fevd <- function(x, return.period = 100, ..., burn.in = 499, FUN = "mean", interval) {

    if(x$method != "Bayesian") stop("postpred: invalid x argument.")

    if(!is.fixedfevd(x)) stop("postpred: sorry, currently no posterior predictive capability for model type.  Maybe try using default method.")

    p <- x$results
    pdim <- dim(p)
    p <- p[,-pdim[2]]
    if(burn.in > 0) p <- p[-(1:burn.in),]
    pdim <- dim(p)

    pnames <- colnames(p)
    if(is.element("log.scale", pnames)) {

	p[,"log.scale"] <- exp(p[,"log.scale"])
	colnames(p)[ pnames == "log.scale" ] <- "scale"
	pnames[ pnames == "log.scale" ] <- "scale"

    }

    typ <- x$type
    if(typ == "PP") typ <- "GEV"

    if(is.element(typ, c("GP", "Exponential", "Beta", "Pareto"))) u <- x$threshold
    else u <- 0

    if(missing(interval)) {

	# z <- range(x$x, finite = TRUE)
	# dz <- diff(z)
	# interval <- c(z[1] - 100 * dz, z[2] + 100 * dz)
	interval <- c(-1e8, 1e8)

    }

    postpred(x = p, return.period = return.period, ..., burn.in = 0, FUN = FUN, threshold = u, type = typ, lambda = x$rate, interval = interval, period.basis = x$period.basis)

} # end of 'postpred.fevd' function.

postpred.default <- function(x, return.period = 100, ..., burn.in = 499, FUN = "mean", threshold = 0, type = "GEV", lambda = 1, interval, period.basis = "year") {

    m <- return.period

    if(burn.in > 0) x <- x[-(1:burn.in),]

    if(missing(interval)) interval <- c(-1e8, 1e8)

    xnames <- colnames(x)
    xd <- dim(x)

    if(!is.element("location", xnames)) {

	x <- cbind(rep(0, xd[1]), x)
	colnames(x) <- c("location", xnames)
	xnames <- colnames(x)

    }

    if(!is.element("shape", xnames)) {

	x <- cbind(x, rep(0, xd[1]))
	colnames(x) <- c(xnames, "shape")
	xnames <- colnames(x)

    }

    if(length(threshold) == 1) threshold <- rep(threshold, xd[1])

     pfun <- function(id, q, th, u, mod, lam) {

            pevd(q = q, loc = th[id, "location"], scale = th[id, "scale"],
                shape = th[id, "shape"], threshold = u[ id ], type = mod, lambda = lam,
		lower.tail = TRUE, log.p = TRUE)

        } # end of internal 'pfun' function.

    optfun <- function(q, m, th, u, mod, lam, N, FUN, pfun) {

	FUN <- match.fun(FUN)
	pfun <- match.fun(pfun)

	ind <- matrix(1:N, ncol = 1)

	pb <- apply(ind, 1, pfun, q = q, th = th, u = u, mod = mod, lam = lam)

	return(abs(log(1 - 1 / m) - FUN(pb)))

    } # end of internal 'optfun' function.

    out <- optimize(optfun, interval, m = m, th = x, u = threshold, mod = type, lam = lambda,
		N = xd[1], FUN = FUN, pfun = pfun, ...)


    sample <- exp(apply(matrix(1:xd[1], ncol = 1), 1, pfun, q = out$minimum, th = x, u = threshold,
		mod = type, lam = lambda))

    out$return.period <- m
    out$period.basis <- period.basis
    out$sample <- 1 / (1 - sample)

    class(out) <- "postpred"

    return(out)

} # end of 'postpred.default' function.

summary.postpred <- function(object, ..., alpha = 0.05) {

    cat("Posterior predicted ", object$return.period, "-", object$period.basis, " return level.\n")
    pci <- quantile(object$sample, probs = c(alpha / 2, 1 - alpha / 2))
    res <- c(mean(object$sample, na.rm = TRUE), pci)
    names(res) <- c("Estimate", paste(1 - alpha, "% Lower", sep = ""), paste(1 - alpha, "% Upper", sep = ""))
    print(res)

    invisible(res)

} # end of 'summary.postpred' function.

print.postpred <- function(x, ..., alpha = 0.05) {

    summary(x, alpha = alpha)

} # end of 'print.postpred' function.

hist.postpred <- function(x, ...) {

    a <- list(...)

    if(!is.null(a$main)) hist(x$sample, ...)
    else hist(x$sample, main = paste("Histogram of posterior predicted ", x$return.period, "-", x$period.basis, " return level", sep =""), ...)
    abline(v = x$objective, lty = 2)

} # end of 'hist.postpred' function.
