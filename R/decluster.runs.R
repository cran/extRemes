"decluster.runs" <-
function(z, r) {

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
  t <- diff(s)
  cluster <- rep(1, nx)
  if(nx > 1) cluster[2:nx] <- 1 + cumsum(t > r)
  size <- tabulate(cluster)
  nc <- length(size)
  inter <- rep(FALSE, nx)
  inter[match(1:nc, cluster)] <- TRUE
  list(scheme = "runs", par = r, nc = nc, size = size, s = s, cluster = cluster, t = c(NA, t), inter = inter, intra = !inter, r=r)
}

