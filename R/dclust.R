dclust <- function(xdat, u, r, cluster.by=NULL) {
#
# Function to decluster a vector of data.
#
# 'xdat' an 'n X 1' vector of data to be declustered.
# 'u' threshold by which to decluster the data (may be a single number or 'n X 1' numeric vector).
# 'r' a single number between 1 and n-1 used to define clusters (see Coles 2001).
# 'cluster.by' an 'n X 1' vector that definesa base set of clusters to be clustered further.
n <- length( xdat)
if( r >= n-1) stop("dclust: r is too big!")
else if( r < 1) stop("dclust: r must be >= 1")
check <- matrix( NA, nrow=length( xdat), ncol=r+1)
check[,1] <- xdat > u
if( length( u) == 1) for( i in 2:(r+1)) check[,i] <- c(xdat[i:n] > u, rep( NA, i-1))
else if( length( u) == n) for( i in 2:(r+1)) check[,i] <- c(xdat[i:n] > u[i:n], rep( NA, i-1))
else stop("dclust: threshold must have length of either 1 or the same as xdat")
clust <- numeric(n)+1
if( !is.null( cluster.by)) {
        ind <- diff(cluster.by)>0
	pos.ind <- (1:(n-1))[ind]+1
	nby <- sum( ind)
	for( i in 1:nby) {
		clust[pos.ind[i]:n] <- clust[pos.ind[i]:n]+1
		check[(pos.ind[i]-r):(pos.ind[i]-1),] <- NA
		} # end of for 'i' loop.
	for( j in 1:(n-r)) {
		if( any( is.na( check[j,]))) next
		if( sum( check[j,], na.rm=TRUE) == 1 & check[j,1])
			clust[(j+1):n] <- clust[(j+1):n]+1
		} # end of for 'j' loop.
	if( !is.na( check[n,1]))
		if( sum( check[(n-r):(n-1),1], na.rm=TRUE)==0 & check[n,1])
			clust[n] <- clust[n]+1
} else {
	for( j in 1:(n-r)) if( sum( check[j,], na.rm=TRUE) == 1 & check[j,1])
			clust[(j+1):n] <- clust[(j+1):n]+1
	if( !is.na(check[n,1]))
		if( sum( check[(n-r):(n-1),1], na.rm=TRUE)==0 & check[n,1])
			clust[n] <- clust[n]+1
	} # end of if else cluster.by NULL or not stmts.
ncluster <- clust[n]
# clust <- factor( clust)
xdat.dc <- aggregate(xdat, by=list( clust), FUN=max, na.rm=TRUE)[,2]
ndc <- length( xdat.dc)
if( length( u) == 1) xdat.dc <- c( xdat.dc, rep( min( xdat, u-1, na.rm=TRUE), n-ndc))
else xdat.dc <- c( xdat.dc, rep( min( xdat, min(u)-1, na.rm=TRUE), n-ndc))
out <- list( xdat.dc=xdat.dc, ncluster=ncluster, clust=clust)
return( out)
} # end of dclust fcn 
