"decluster.intervals" <-
function(z, ei, blocks=NULL) {

  # Description:
  #
  #   Performs intervals declustering.
  #
  # Usage:
  #
  #   decluster.intervals(z, ei)
  #
  # Arguments:
  #
  #    z: logical vector indicating which positions correspond to
  #       extreme values.
  #   ei: estimate of the extremal index.
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
  #   to the same cluster, where `r' is the `nc'-th largest
  #   interexceedance time and `nc', the number of clusters,
  #   is estimated from the extremal index, `ei', and the times
  #   between extremes. Setting `ei' = 1 causes each extreme to
  #   form a separate cluster.
  #
  # Warning:
  #
  # References:
  #
  #   Ferro CAT & Segers J (2003) Inference for clusters of extreme
  #   values. Journal of the Royal Statistical Society B 65, 545-556.
  #
  # See Also:
  #
  #   decluster.runs, exi.intervals
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   z <- x > quantile(x, 0.9)
  #   decluster.intervals(z, exi.intervals(z))

  if(ei >= 1) {
    r <- 0
  } else {
    s <- c(1:length(z))[z]
    if(is.null(blocks)) t <- diff(s)
    else {
	b <- blocks[z]
	t <- unlist(aggregate(s, by=list(b), diff)$x)
	t <- c(t)
    }
    temp <- rev(sort(t))
    nc <- 1 + floor(ei * (sum(z) - 1))
    while((nc > 1) && (temp[nc-1] == temp[nc])) nc <- nc - 1
    r <- temp[nc]
  }
  out <- decluster.runs(z, r, blocks=blocks)
  out$scheme <- "intervals" 
  out
}

