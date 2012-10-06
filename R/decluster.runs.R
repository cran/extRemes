"decluster.runs" <-
function(z, r, blocks=NULL) {

  # Description:
  #
  #   Performs runs declustering.
  #
  # Usage:
  #
  #   decluster.runs(z, r)
  #
  # Arguments:
  #
  #   z: logical vector indicating which positions correspond to
  #      extreme values.
  #   r: integer run length
  #
  # Value:
  #
  #   A list containing the following elements.
  #
  #    scheme: name of declustering scheme
  #       par: value of declustering parameter (run length)
  #        nc: number of clusters
  #      size: vector of cluster sizes
  #         s: vector of times of extremes
  #   cluster: vector of numbers identifying clusters to which
  #            extremes belong
  #         t: vector of times between extremes
  #     inter: vector of intercluster time indicators (logical)
  #     intra: vector of intracluster time indicators (logical)
  #
  # Details:
  #
  #   Extremes separated by fewer than `r' non-extremes belong
  #   to the same cluster. Setting `r' < 1 causes each extreme
  #   to form a separate cluster.
  #
  # Warning:
  #
  # References:
  #
  #   Smith RL (1989) Extreme value analysis of environmental time
  #   series: an application to trend detection in ground-level
  #   ozone. Statistical Science 4, 367-393.
  #
  # See Also:
  #
  #   decluster.intervals
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   decluster.runs(x > quantile(x, 0.9), 1)

  nx <- sum(z)
  s <- c(1:length(z))[z]
  cluster <- rep(1,nx)
  if(is.null(blocks)) {
	t <- diff(s)
  	if(nx > 1) cluster[2:nx] <- 1 + cumsum(t > r)
  } else {
	b <- blocks[z]
	m <- aggregate(!is.na(z),by=list(blocks),sum)$x
	n <- aggregate(!is.na(z[z]),by=list(b),sum)$x
	cluster <- rep.int(1:length(m), times=m)
	ifun <- function(z,r) {
	   out <- decluster.runs(z=z,r=r)
	   return(out$cluster)
	} # end of 'ifun' internal function.
	tmp <- c(unlist(aggregate(z, by=list(cluster), ifun, r=r)$x))
	M <- c(0, aggregate(tmp, by=list(b), max, na.rm=TRUE)$x)
	M <- cumsum(M[-length(M)])
	M <- rep.int(M,times=n)
	cluster <- tmp+M
	ifun2 <- function(z,r) {
	   if(sum(z)==1) return(Inf)
	   else if(sum(z)==0) return(-Inf)
           out <- decluster.runs(z=z,r=r)
           return(out$t)
        } # end of 'ifun2' internal function.
	t <- c(unlist(aggregate(z,by=list(blocks),ifun2, r=r)$x)) 
	t <- t[t>0]
	t <- t[-1]
  }
  size <- tabulate(cluster)
  nc <- length(size)
  inter <- rep(FALSE, nx)
  inter[match(1:nc, cluster)] <- TRUE
  list(scheme = "runs", par = r, nc = nc, size = size, s = s, cluster = cluster, t = c(NA, t), inter = inter, intra = !inter, r=r, blocks=blocks)
}

