summary.pp.fit <- function(object, ...) {
opt.args <- list(...)
z <- object
cat("\n")
print( paste("Threshold = ", z$threshold, sep=""))
cat("\n")
print( paste("Number of exceedances of threshold = ", z$nexc, sep=""))
cat("\n")
print( paste("Exceedance rate = (per year)", z$rate*z$npy, sep=""))
cat("\n")
print( z$summary1)
print( paste( "Negative log-likelihood: ", z$nllh, sep=""))
cat( "\n", "Parameter covariance:\n")
print( z$cov)
print( paste( "Convergence code (see help file for optim): ", z$conv, sep=""))
invisible(paste(z$summary1, paste( "Negative log-likelihood: ", z$nllh, sep=""),
                "Parameter covariance: ", z$cov,  paste( "Convergence code (see help file for optim): ", z$conv, sep=""), sep="\n"))
}
