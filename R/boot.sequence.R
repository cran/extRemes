"boot.sequence" <-
function(tmat, ymat, u) {

  # Description:
  #
  #   Bootstraps a sequence of exceedances.
  #
  # Usage:
  #
  # Arguments:
  #
  #   tmat: output from `boot.matrix'
  #   ymat: output from `boot.matrix'
  #      u: threshold
  #
  # Value:
  #
  #   Vector of exceedances above `u'.
  # 
  # Details:
  #
  #   The bootstrapped sequence contains the same number of clusters
  #   as identified in the original data. Clusters of exceedances
  #   and inter-exceedance times within clusters are resampled as
  #   single entities; inter-exceedance times between clusters are
  #   resampled independently. The values of non-exceedances are set
  #   equal to the threshold `u'.
  #
  # Warning:
  #
  #   The bootstrapped sequence can be longer than the original
  #   sequence.
  #
  # References:
  #
  #   Ferro CAT & Segers J (2003) Inference for clusters of extreme
  #   values. Journal of the Royal Statistical Society B 65, 545-556.
  #
  # See Also:
  #
  #   `boot.matrix'
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   u <- quantile(x, 0.9, names = FALSE)
  #   dec <- decluster.runs(x > u, 1)
  #   mat <- boot.matrix(dec, x)
  #   boot.sequence(mat[[1]], mat[[2]], u)

  nc <- dim(tmat)[2]
  clusters <- sample(nc, replace = TRUE)
  tb <- tmat[, clusters]
  tb[1, 1] <- 1
  if(nc > 1) tb[1, 2:nc] <- sample(tmat[1, 2:nc], replace = TRUE)
  tb <- tb[!is.na(tb)]
  sb <- cumsum(tb)
  ex <- ymat[, clusters]
  yb <- rep(u, max(sb))
  yb[sb] <- ex[!is.na(ex)]
  yb
}

