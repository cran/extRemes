"boot.matrix" <-
function(x, y) {

  # Description:
  #
  #   Sets up matrices for bootstrapping sequences of extreme values.
  #
  # Usage:
  #
  # Arguments:
  #
  #   x: output from `decluster.runs' or `decluster.intervals'
  #   y: vector of observations
  #
  # Value:
  #
  #   A list containing two matrices, each with columns corresponding
  #   to clusters identified in `x'. The first matrix contains the
  #   inter-exceedance times and the second matrix contains the data
  #   values in `y' corresponding to the exceedances in each cluster.
  # 
  # Details:
  #
  #   This function merely formats the information needed by
  #   `boot.sequence' to improve efficiency.
  #
  # Warning:
  #
  # References:
  #
  # See Also:
  #
  #   `boot.sequence', `decluster.intervals', `decluster.runs'
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   dec <- decluster.runs(x > quantile(x, 0.9), 1)
  #   boot.matrix(dec, x)

  tmat <- matrix(NA, max(x$size), x$nc)
  ymat <- matrix(NA, max(x$size), x$nc)
  for(ic in 1:x$nc) {
    tmat[1:x$size[ic], ic] <- x$t[x$cluster==ic]
    ymat[1:x$size[ic], ic] <- y[x$s[x$cluster==ic]]
  }
  list(tmat, ymat)
}
