summary.gev.fit <- function(object, ...) {
opt.args <- list(...) # nonsense item for method compatibility.
z <- object
if( z$conv == 0)
                CONV.msg <- paste("Convergence successfull!")
        else if( z$conv == 1)
                CONV.msg <- paste("Iteration limit exceeded.",
                                        "Did not convergence.", sep="\n")
        else if( z$conv == 51 | z$conv == 52)
                CONV.msg <- paste( z$message)
        else CONV.msg <- paste("Convergence: ", z$conv, " (See help for optim for more info).", sep="")

#       tkinsert( base.txt, "end", CONV.msg)
#       tkinsert( base.txt, "end", nl2)
print( CONV.msg)
print( paste( "Maximum Likelihood Estimates:"))
print( z$summary1)
cat("\n", "\n")
print( paste( "Negative log-likelihood: ", z$nllh, sep=""))
cat( "\n", "Parameter covariance:\n")
print( z$cov)
print( paste( "Convergence code (see help file for optim): ", z$conv, sep=""))
invisible()
# invisible(paste(z$summary1, paste( "Negative log-likelihood: ", z$nllh, sep=""),
# 		"Parameter covariance: ", z$cov,  paste( "Convergence code (see help file for optim): ", z$conv, sep=""),
# 		sep="\n"))
}
