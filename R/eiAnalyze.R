eiAnalyze <- function(x, thresholds=quantile(x, seq(0.900,0.995,by=0.005)), conf=0.95, iter=500, plot=FALSE) {
#
# Function to compute the intervals estimator (Ferro and Segers (2003)) of the extremal index
# with conf*100% bootstrapped confidence bands.
#
# 'x' An 'n X 1' vector of original data.
# 'thresholds' is either a numeric or 'm X 1' vector of thresholds above which to estimate the extremal index.
# 'conf' is 'conf'*100% confidence desired.
# 'iter' is number of iterations on which to run the bootstrap algorithm.
#
# Value:
# Returns a list with components:
#
# 'ei' is an 'm X 1' vector of the extremal index estimates.
# 'ci' is an 'm X 2' matrix of lower and upper 'conf'*100% confidence bounds.
#
nu <- length( thresholds)
ei <- nc <- run.length <- numeric(nu)
ci <- matrix(NA,nrow=nu,ncol=2)
out <- list()
for( iu in 1:nu) {
	u <- thresholds[iu]
	z <- x > u
	ei[iu] <- exi.intervals(z)
	dec <- decluster.intervals(z, ei[iu])
	run.length[iu] <- dec$par
	nc[iu] <- dec$nc
	if( dec$par == 0) next
	mat <- boot.matrix(dec,x)
	eib <- matrix(NA, nu, iter)
	for( ib in 1:iter) {
		set.seed(ib)
		zb <- boot.sequence(mat[[1]], mat[[2]], u) > u
		eib[iu,ib] <- exi.intervals(zb)
	} # end of for 'ib' loop.
	ci[iu,] <- quantile(eib[iu,], c((1-conf)/2,(1+conf)/2))
} # end of for 'iu' loop.
if( plot & length( thresholds) > 1) {
	plot( thresholds, ei, type="l", xlab="Thresholds", ylab="Extremal index", ylim=c(0,1))
	if( !is.null( names( thresholds))) axis(3, at=thresholds, labels=names( thresholds))
	lines( thresholds, ci[,1], lty=3)
	lines( thresholds, ci[,2], lty=3)
	} # end of if 'plot' stmt.
out$ei <- ei
out$ci <- ci
out$u <- thresholds
out$nc <- nc
out$run.length <- run.length
return(out)
}
