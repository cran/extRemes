\name{ci.fevd}
\alias{ci.fevd}
\alias{ci.fevd.bayesian}
\alias{ci.fevd.lmoments}
\alias{ci.fevd.mle}
\title{

Confidence Intervals

}
\description{
Confidence intervals for parameters and return levels using fevd objects.
}
\usage{
\method{ci}{fevd}(x, alpha = 0.05, type = c("return.level", "parameter"), 
    return.period = 100, which.par, R = 502, ...)

\method{ci}{fevd.bayesian}(x, alpha = 0.05, type = c("return.level", "parameter"),
    return.period = 100, which.par = 1, FUN = "mean", burn.in = 499, tscale = FALSE,
    ...)

\method{ci}{fevd.lmoments}(x, alpha = 0.05, type = c("return.level", "parameter"),
    return.period = 100, which.par, R = 502, tscale = FALSE,
    return.samples = FALSE, ...)

\method{ci}{fevd.mle}(x, alpha = 0.05, type = c("return.level", "parameter"),
    return.period = 100, which.par, R = 502, method = c("normal",
        "boot", "proflik"), xrange = NULL, nint = 20, verbose = FALSE,
    tscale = FALSE, return.samples = FALSE, ...)

}

\arguments{
  \item{x}{

list object returned by \code{fevd}.

}
  \item{alpha}{

numeric between 0 and 1 giving the desired significance level (i.e., the (1 - \code{alpha}) * 100 percent confidence level; so that the default \code{alpha} = 0.05 corresponds to a 95 percent confidence level).

}

  \item{type}{character specifying if confidence intervals (CIs) are desired for return level(s) (default) or one or more parameter.}

  \item{return.period}{numeric vector giving the return period(s) for which it is desired to calculate the corresponding return levels.}

  \item{\dots}{ optional arguments to the \code{profliker} function.  For example, if it is desired to see the plot (recommended), use \code{verbose} = TRUE.  }

  \item{which.par}{numeric giving the index (indices) for which parameter(s) to calculate CIs.  Default is to do all of them.}

  \item{FUN}{character string naming a function to use to estimate the parameters from the MCMC sample.  The function is applied to each column of the \code{results} component of the returned \code{fevd} object.}

  \item{burn.in}{The first \code{burn.in} values are thrown out before calculating anything from the MCMC sample.}

  \item{R}{the number of bootstrap iterations to do.}

  \item{method}{ character naming which method for obtaining CIs should be used.  Default (\dQuote{normal}) uses a normal approximation, and in the case of return levels (or transformed scale) applies the delta method using the parameter covariance matrix.  Option \dQuote{boot} employs a parametric bootstrap that simulates data from the fitted model, and then fits the EVD to each simulated data set to obtain a sample of parameters or return levels.  Currently, only the percentile method of calculating the CIs from the sample is available.  Finally, \dQuote{proflik} uses function \code{profliker} to calculate the profile-likelihood function for the parameter(s) of interest, and tries to find the upcross level between this function and the appropriate chi-square critical value (see details).
}

  \item{tscale}{For the GP df, the scale parameter is a function of the shape parameter and the threshold.  When plotting the parameters, for example, against thresholds to find a good threshold for fitting the GP df, it is imperative to transform the scale parameter to one that is independent of the threshold.  In particular, \code{tscale} = scale - shape * threshold.}

  \item{xrange, nint}{arguments to \code{profliker} function.}

  \item{return.samples}{logical; should the bootstrap samples be returned?  If so, CIs will not be calculated and only the sample of parameters (return levels) are returned.}

  \item{verbose}{
logical; should progress information be printed to the screen?  For profile likelihood method (\code{method} = \dQuote{proflik}), if TRUE, the profile-likelihood will also be plotted along with a horizontal line through the chi-square critical value.
}

}
\details{

Confidence Intervals (\code{ci}):

\code{ci}: The \code{ci} method function will take output from \code{fevd} and calculate confidence intervals (or credible intervals in the case of Bayesian estimation) in an appropriate manner based on the estimation method.  There is no need for the user to call \code{ci.fevd}, \code{ci.fevd.lmoments}, \code{ci.fevd.bayesian} or \code{ci.fevd.mle}; simply use \code{ci} and it will access the correct functions.

Currently, for L-moments, the only method available in this software is to apply a parameteric bootstrap, which is also available for the MLE/GMLE methods.  A parametric bootstrap is performed via the following steps.

1. Simulate a sample of size n = lenght of the original data from the fitted model.

2. Fit the EVD to the simulated sample and store the resulting parameter estimates (and perhaps any combination of them, such as return levels).

3. Repeat steps 1 and 2 many times (to be precise, \code{R} times) to obtain a sample from the population df of the parameters (or combinations thereof).

4. From the sample resulting form the above steps, calculate confidence intervals.  In the present code, the only option is to do this by taking the alpha/2 and 1 - alpha/2 quantiles of the sample (i.e., the percentile method).  However, if one uses \code{return.samples} = TRUE, then the sample is returned instead of confidence intervals allowing one to apply some other method if they so desire.

As far as guidance on how large \code{R} should be, it is a trial and error decision.  Usually, one wants the smallest value (to make it as fast as possible) that still yields accurate results.  Generally, this means doing it once with a relatively low number (say \code{R} = 100), and then doing it again with a higher number, say \code{R} = 250.  If the results are very different, then do it again with an even higher number.  Keep doing this until the results do not change drastically.

For MLE/GMLE, the normal approximation (perhaps using the delta method, e.g., for return levels) is used if \code{method} = \dQuote{normal}.  If \code{method} = \dQuote{boot}, then parametric bootstrap CIs are found.  Finally, if \code{method} = \dQuote{profliker}, then bounds based on the profile likelihood method are found (see below for more details).

For Bayesian estimation, the alpha/2 and 1 - alpha/2 percentiles of the resulting MCMC sample (after removing the first \code{burn.in} values) are used.  If return levels are desired, then they are first calculated for each MCMC iteration, and the same procedure is applied.  Note that the MCMC samples are availabel in the \code{fevd} output for this method, so any other procedure for finding CIs can be done by the savvy user.


Finding CIs based on the profile-likelihood method:

The profile likelihood method is often the best method for finding accurate CIs for the shape parameter and for return levels associated with long return periods (where their distribution functions are generally skewed so that, e.g., the normal approximation is not a good approximation).  The profile likelihood for a parameter is obtained by maximizing the likelihood over the other parameters of the model for each of a range (\code{xrange}) of values.  An approximation confidence region can be obtained using the deviance function D = 2 * (l(theta.hat) - l_p(theta)), where l(theta.hat) is the likelihood for the original model evaluated at their estimates and l_p(theta) is the likelihood of the parameter of interest (optimized over the remaining parameters), which approximately follows a chi-square df with degrees of freedom equal ot the number of parameters in the model less the one of interest.  The confidence region is then given by

    C_alpha = the set of theta_1 s.t. D <= q,

where q is the 1 - alpha quantile of the chi-square df with degrees of freedom equal to 1 and theta_1 is the parameter of interest.  If we let m represent the maximum value of the profile likelihood (i.e., m = max(l_p(theta))), then consider a horizontal line through m - q.  All values of theta_1 that yield a profile likelihood value above this horizontal line are within the confidence region, C_alpha (i.e., the range of these values represents the (1 - alpha) * 100 percent CI for the parameter of interest).  For combinations of parameters, such as return levels, the same technique is applied by transforming the parameters in the likelihood to reflect the desired combination.

To use the profile-likelihood approach, it is necessary to choose an \code{xrange} argument that covers the entire confidence interval and beyond (at least a little), and the \code{nint} argument may be important here too (this argument gives the number of points to try in fitting a spline function to the profile likelihood, and smaller values curiously tend to be better, but not too small!  Smaller values are also more efficient).  Further, one should really look at the plot of the profile-likelihood to make sure that this is the case, and that resulting CIs are accurately estimated (perhaps using the \code{locator} function to be sure).  Nevertheless, an attempt is made to find the limits automatically.  To look at the plot along with the horizontal line, m - q, and vertical lines through the MLE (thin black dashed) and the CIs (thick dashed blue), use the \code{verbose} = TRUE argument in the call to \code{ci}.  This is not an explicit argument, but available nonetheless (see examples below).

See any text on EVA/EVT for more details (e.g., Coles 2001; Beirlant et al 2004; de Haan and Ferreira 2006).
}
\value{

Either a numeric vector of length 3 (if only one parameter/return level is used) or a matrix.  In either case, they will have class \dQuote{ci}.

}
\references{

Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004). \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001). \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag.

de Haan, L. and Ferreira, A. (2006). \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.


}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{ci.rl.ns.fevd.bayesian}}, \code{\link[distillery:ci]{distillery::ci}}
}
\examples{

data(Fort)

fit <- fevd(Prec, Fort, threshold = 2, type = "GP",
    units = "inches", verbose = TRUE)

ci(fit, type = "parameter")

\dontrun{
ci(fit, type = "return.level", method = "proflik",
    xrange = c(3.5,7.75), verbose = TRUE)
# Can check using locator(2).

ci(fit, method = "boot")

}

}
\keyword{ htest }
\keyword{ distribution }
