summary.gpd.fit <- function(object, ...) {
opt.args <- list(...) # nonsense item for method compatibility.
z <- object
print( z$summary1)
print( paste( "Negative log-likelihood: ", z$nllh, sep=""))
cat( "\n", "Parameter covariance:\n")
print( z$cov)
print( paste( "Convergence code (see help file for optim): ", z$conv, sep=""))
invisible(paste(z$summary1, paste( "Negative log-likelihood: ", z$nllh, sep=""),
                "Parameter covariance: ", z$cov,  paste( "Convergence code (see help file for optim): ", z$conv, sep=""), sep="\n"))
}
