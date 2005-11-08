"exi.intervals" <-
function(z) {

  # Description:
  #
  #   Evaluates the intervals estimator for the extremal index.
  #
  # Usage:
  #
  #   exi.intervals(z)
  #
  # Arguments:
  #
  #   z: logical vector indicating which positions correspond to
  #      extreme values.
  #
  # Value:
  # 
  #   Estimate of the extremal index.
  #
  # Details:
  #
  # Warning:
  #
  #   The estimator is not constrained to lie in [0,1] and a
  #   default value of 1 is returned if there are fewer than two
  #   extreme values.
  #
  # References:
  #
  #   Ferro CAT & Segers J (2003) Inference for clusters of extreme
  #   values. Journal of the Royal Statistical Society B 65, 545-556.
  #
  # See Also:
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   exi.intervals(x > quantile(x, 0.9))

  if(sum(z) <= 1) {
    warning("estimator undefined: too few exceedances")
    return(1)
  } else {
    nz <- length(z)          # length of sequence
    s <- c(1:nz)[z]          # exceedance times
    t <- diff(s)             # interexceedance times
    if(max(t) <= 2) {
      t1 <- mean(t)
      t2 <- mean(t^2)
    } else {
      t1 <- mean(t-1)
      t2 <- mean((t-1)*(t-2))
    }
  }
  2*(t1^2)/t2
}

