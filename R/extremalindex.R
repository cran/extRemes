extremalindex <- function(xdat, u) {
#
# Function to estimate the extremal index using the method of Ferro and
#	Segers (2003).
#
# theta = min( 1, theta.hat) if no interexceedance times (T_i) exceed 2,
#	where theta.hat = 2*sum^2 T_i / ((N-1)sum T_i^2), and 'N' is the
#	number of exceedances.
#
# theta = min( 1, theta.tilde) if there is at least one T_i > 2,
#	where theta.tilde = (2*sum^2 (T_i - 1))/((N-1)sum ((T_i - 1)(T_i - 2)))
#
# 'xdat' is an 'n X 1' numeric vector of data.
# 'u' is a user defined threshold.  May be a single number or 'n X 1' vector.
#
# Returns a list object with components:
# 'theta' the estimated extremal index.
# 'C' estimated number of clusters based on Ferro and Segers (2003).
# 'run.length' the estimated run length (=T_(K)) for runs declustering.
# 'msg' message stating the estimate used: 'theta.hat' or 'theta.tilde'
#	and whether or not the estimate was originally > 1.
#
n <- length( xdat)
id <- xdat > u
N <- sum( id)
S <- (1:n)[id]
TT <- diff( S)
if( !any( TT > 2)) {
	# Compute 'theta.hat'
	theta <- 2*sum(TT, na.rm=TRUE)^2/((N-1)*sum(TT^2, na.rm=TRUE))
	msg <- paste("theta.hat used because no values of T > 2.")
	if( theta > 1) {
		theta <- 1
		msg <- paste( msg, "  Using theta = 1 because theta.hat > 1.",
				sep="\n")
		}
} else {
	# Compute 'theta.tilde'
	theta <- 2*sum(TT-1,na.rm=TRUE)^2/((N-1)*sum((TT-1)*(TT-2),na.rm=TRUE))
	msg <- paste("theta.tilde used because a value(s) exists of T > 2.")
        if( theta > 1) {
                theta <- 1
                msg <- paste( msg, "  Using theta = 1 as theta.hat > 1.")
                }
	} # end of if else any T > 2 stmts.
# First guess estimate of the number of clusters.
K <- ifelse( round( theta*N, digits=0) != theta*N, floor( theta*N)+1, theta*N)

# Make sure there are no ties.  If there are, decrease 'K' until
# T_(K-1) < T_(K).
T.order <- order( TT, na.last=TRUE, decreasing=TRUE)
T.ranked <- TT[T.order]
T.K <- T.ranked[K]
if( sum( TT == T.K, na.rm=TRUE) > 1) for(i in 1:K){
	K <- K-1
	T.K <- T.ranked[K]
	if( sum( TT == T.K, na.rm=TRUE) > 1) next
	else break
	} # end of for 'i' loop.
return( list( theta=theta, C=K, run.length=T.K, msg=msg))
}
