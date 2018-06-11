\name{fevd}
\alias{fevd}
\alias{plot.fevd}
\alias{plot.fevd.bayesian}
\alias{plot.fevd.lmoments}
\alias{plot.fevd.mle}
\alias{print.fevd}
\alias{summary.fevd}
\alias{summary.fevd.bayesian}
\alias{summary.fevd.lmoments}
\alias{summary.fevd.mle}
\title{
Fit An Extreme Value Distribution (EVD) to Data
}
\description{
Fit a univariate extreme value distribution functions (e.g., GEV, GP, PP, Gumbel, or Exponential) to data; possibly with covariates in the parameters.
}
\usage{
fevd(x, data, threshold = NULL, threshold.fun = ~1, location.fun = ~1,
    scale.fun = ~1, shape.fun = ~1, use.phi = FALSE,
    type = c("GEV", "GP", "PP", "Gumbel", "Exponential"),
    method = c("MLE", "GMLE", "Bayesian", "Lmoments"), initial = NULL,
    span, units = NULL, time.units = "days", period.basis = "year",
    na.action = na.fail, optim.args = NULL, priorFun = NULL,
    priorParams = NULL, proposalFun = NULL, proposalParams = NULL,
    iter = 9999, weights = 1, blocks = NULL, verbose = FALSE)

\method{plot}{fevd}(x, type = c("primary", "probprob", "qq", "qq2",
    "Zplot", "hist", "density", "rl", "trace"),
    rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800),
    a = 0, hist.args = NULL, density.args = NULL, d = NULL, ...)

\method{plot}{fevd.bayesian}(x, type = c("primary", "probprob", "qq", "qq2",
    "Zplot", "hist", "density", "rl", "trace"),
    rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800),
    a = 0, hist.args = NULL, density.args = NULL, burn.in = 499, d = NULL, ...)

\method{plot}{fevd.lmoments}(x, type = c("primary", "probprob", "qq", "qq2",
    "Zplot", "hist", "density", "rl", "trace"),
    rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800),
    a = 0, hist.args = NULL, density.args = NULL, d = NULL, ...)

\method{plot}{fevd.mle}(x, type = c("primary", "probprob", "qq", "qq2",
    "Zplot", "hist", "density", "rl", "trace"),
    rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800),
    a = 0, hist.args = NULL, density.args = NULL, period = "year",
    prange = NULL, d = NULL, ...)

\method{print}{fevd}(x, ...)

\method{summary}{fevd}(object, ...)

\method{summary}{fevd.bayesian}(object, FUN = "mean", burn.in = 499, ...)

\method{summary}{fevd.lmoments}(object, ...)

\method{summary}{fevd.mle}(object, ...)

}
\arguments{
  \item{x}{\code{fevd}: \code{x} can be a numeric vector, the name of a column of \code{data} or a formula giving the data to which the EVD is to be fit.  In the case of the latter two, the \code{data} argument must be specified, and must have appropriately named columns.

\code{plot} and \code{print} method functions: any list object returned by \code{fevd}.
}
   \item{object}{A list object of class \dQuote{fevd} as returned by \code{fevd}.}
  \item{data}{
A data frame object with named columns giving the data to be fit, as well as any data necessary for modeling non-stationarity through the threshold and/or any of the parameters.
}
  \item{threshold}{
numeric (single or vector).  If fitting a peak over threshold (POT) model (i.e., \code{type} = \dQuote{PP}, \dQuote{GP}, \dQuote{Exponential}) this is the threshold over which (non-inclusive) data (or excesses) are used to estimate the parameters of the distribution function.  If the length is greater than 1, then the length must be equal to either the length of \code{x} (or number of rows of \code{data}) or to the number of unique arguments in \code{threshold.fun}.
}
  \item{threshold.fun}{
formula describing a model for the thresholds using columns from \code{data}.  Any valid formula will work.  \code{data} must be supplied if this argument is anything other than ~ 1.  Not for use with \code{method} \dQuote{Lmoments}.
}
  \item{location.fun, scale.fun, shape.fun}{
formula describing a model for each parameter using columns from \code{data}.  \code{data} must be supplied if any of these arguments are anything other than ~ 1.
}
  \item{use.phi}{
logical; should the log of the scale parameter be used in the numerical optimization (for \code{method} \dQuote{MLE}, \dQuote{GMLE} and \dQuote{Bayesian} only)?  For the ML and GML estimation, this may make things more stable for some data.
}

  \item{type}{
\code{fevd}: character stating which EVD to fit.  Default is to fit the generalized extreme value (GEV) distribution function (df).

\code{plot} method function: character describing which plot(s) is (are) desired.  Default is \dQuote{primary}, which makes a 2 by 2 panel of plots including the QQ plot of the data quantiles against the fitted model quantiles (\code{type} \dQuote{qq}), a QQ plot (\dQuote{qq2}) of quantiles from model-simulated data against the data, a density plot of the data along with the model fitted density (\code{type} \dQuote{density}) and a return level plot (\code{type} \dQuote{rl}). In the case of a stationary (fixed) model, the return level plot will show return levels calculated for return periods given by \code{return.period}, along with associated CIs (calculated using default \code{method} arguments depending on the estimation method used in the fit.  For non-stationary models, the data are plotted as a line along with associated effective return levels for return periods of 2, 20 and 100 years (unless \code{return.period} is specified by the user to other values.  Other possible values for \code{type} include \dQuote{hist}, which is similar to \dQuote{density}, but shows the histogram for the data and \dQuote{trace}, which is not used for L-moment fits.  In the case of MLE/GMLE, the trace yields a panel of plots that show the negative log-likelihood and gradient negative log-likelihood (note that the MLE gradient is currently used even for GMLE) for each of the estimated parameter(s); allowing one parameter to vary according to \code{prange}, while the others remain fixed at their estimated values.  In the case of Bayesian estimation, the \dQuote{trace} option creates a panel of plots showing the posterior df and MCMC trace for each parameter.
}
  \item{method}{
\code{fevd}: character naming which type of estimation method to use.  Default is to use maximum likelihood estimation (MLE).

}
  \item{initial}{
A list object with any named parameter component giving the initial value estimates for starting the numerical optimization (MLE/GMLE) or the MCMC iterations (Bayesian).  In the case of MLE/GMLE, it is best to obtain a good intial guess, and in the Bayesian case, it is perhaps better to choose poor initial estimates.  If NULL (default), then L-moments estimates and estimates based on Gumbel moments will be calculated, and whichever yields the lowest negative log-likelihood is used.  In the case of \code{type} \dQuote{PP}, an additional MLE/GMLE estimate is made for the generalized Pareto (GP) df, and parameters are converted to those of the Poisson Process (PP) model.  Again, the initial estimates yielding the lowest negative log-likelihoo value are used for the initial guess.
}

  \item{span}{
single numeric giving the number of years (or other desired temporal unit) in the data set.  Only used for POT models, and only important in the estimation for the PP model, but important for subsequent estimates of return levels for any POT model.  If missing, it will be calculated using information from \code{time.units}.
}
  \item{units}{
(optional) character giving the units of the data, which if given may be used subsequently (e.g., on plot axis labels, etc.).
}
  \item{time.units}{
character string that must be one of \dQuote{hours}, \dQuote{minutes}, \dQuote{seconds}, \dQuote{days}, \dQuote{months}, \dQuote{years}, \dQuote{m/hour}, \dQuote{m/minute}, \dQuote{m/second}, \dQuote{m/day}, \dQuote{m/month}, or \dQuote{m/year}; where m is a number.  If \code{span} is missing, then this argument is used in determining the value of \code{span}.  It is also returned with the output and used subsequently for plot labelling, etc.
}
  \item{period.basis}{
character string giving the units for the period.  Used only for plot labelling and naming output vectors from some of the method functions (e.g., for establishing what the period represents for the return period).
}
   \item{rperiods}{numeric vector giving the return period(s) for which it is desired to calculate the corresponding return levels.}

   \item{period}{character string naming the units for the return period.}

   \item{burn.in}{The first \code{burn.in} values are thrown out before calculating anything from the MCMC sample.}

    \item{a}{when plotting empirical probabilies and such, the function \code{ppoints} is called, which has this argument \code{a}.}

    \item{d}{numeric determining how to scale the rate parameter for the point process.  If NULL, the function will attempt to scale based on the values of \code{period.basis} and \code{time.units}, the first of which must be \dQuote{year} and the second of which must be one of \dQuote{days}, \dQuote{months}, \dQuote{years}, \dQuote{hours}, \dQuote{minutes} or \dQuote{seconds}.  If none of these are the case, then \code{d} should be specified, otherwise, it is not necessary.}

    \item{density.args, hist.args}{named list object containing arguments to the \code{density} and \code{hist} functions, respectively.}

  \item{na.action}{
function to be called to handle missing values.  Generally, this should remain at the default (na.fail), and the user should take care to impute missing values in an appropriate manner as it may have serious consequences on the results.
}
  \item{optim.args}{
A list with named components matching exactly any arguments that the user wishes to specify to \code{optim}, which is used only for MLE and GMLE methods.  By default, the \dQuote{BFGS} method is used along with \code{grlevd} for the gradient argument.  Generally, the \code{grlevd} function is used for the \code{gr} option unless the user specifies otherwise, or the optimization method does not take gradient information.
}
  \item{priorFun}{
character naming a prior df to use for methods GMLE and Bayesian.  The default for GMLE (not including Gumbel or Exponential types) is to use the one suggested by Martins and Stedinger (2000, 2001) on the shape parameter; a beta df on -0.5 to 0.5 with parameters \code{p} and \code{q}.  Must take \code{x} as its first argument for \code{method} \dQuote{GMLE}.  Optional arguments for the default function are \code{p} and \code{q} (see details section).

The default for Bayesian estimation is to use normal distribution functions.  For Bayesian estimation, this function must take \code{theta} as its first argument.

Note: if this argument is not NULL and \code{method} is set to \dQuote{MLE}, it will be changed to \dQuote{GMLE}.
}
  \item{priorParams}{
named list containing any prior df parameters (where the list names are the same as the function argument names).  Default for GMLE (assuming the default function is used) is to use \code{q} = 6 and \code{p} = 9.  Note that in the Martins and Stedinger (2000, 2001) papers, they use a different EVD parametrization than is used here such that a positive shape parameter gives the upper bounded distribution instead of the heavy-tail one (as emloyed here).  To be consistent with these papers, \code{p} and \code{q} are reversed inside the code so that they have the same interpretation as in the papers.

Default for Bayesian estimation is to use ML estimates for the means of each parameter (may be changed using \code{m}, which must be a vector of same length as the number of parameters to be estimated (i.e., if using the default prior df)) and a standard deviation of 10 for all other parameters (again, if using the default prior df, may be changed using \code{v}, which must be a vector of length equal to the number of parameters).
}
  \item{proposalFun}{
For Bayesian estimation only, this is a character naming a function used to generate proposal parameters at each iteration of the MCMC.  If NULL (default), a random walk chain is used whereby if theta.i is the current value of the parameter, the proposed new parameter theta.star is given by theta.i + z, where z is drawn at random from a normal df.
}
  \item{proposalParams}{
A named list object describing any optional arguments to the \code{proposalFun} function.  All functions must take argument \code{p}, which must be a vector of the parameters, and \code{ind}, which is used to identify which parameter is to be proposed.  The default \code{proposalFun} function takes additional arguments \code{mean} and \code{sd}, which must be vectors of length equal to the number of parameters in the model (default is to use zero for the mean of z for every parameter and 0.1 for its standard deviation).
}
  \item{iter}{
Used only for Bayesian estimation, this is the number of MCMC iterations to do.
}
   \item{weights}{numeric of length 1 or n giving weights to be applied
     in the likelihood calculations (e.g., if there are data points to
     be weighted more/less heavily than others).}
   \item{blocks}{An optional list containing information required to fit point process models in a computationally-efficient manner by using only the exceedances and not the observations below the threshold(s). See details for further information. 
     }
   \item{FUN}{character string naming a function to use to estimate the parameters from the MCMC sample.  The function is applied to each column of the \code{results} component of the returned \code{fevd} object.}
  \item{verbose}{
logical; should progress information be printed to the screen?  If TRUE, for MLE/GMLE, the argument \code{trace} will be set to 6 in the call to \code{optim}.
}
   \item{prange}{matrix whose columns are numeric vectors of length two for each parameter in the model giving the parameter range over which trace plots should be made.  Default is to use either +/- 2 * std. err. of the parameter (first choice) or, if the standard error cannot be calculated, then +/- 2 * log2(abs(parameter)).  Typically, these values seem to work very well for these plots.}
   \item{\dots}{Not used by most functions here.  Optional arguments to \code{plot} for the various \code{plot} method functions.

In the case of the \code{summary} method functions, the logical argument \code{silent} may be passed to suppress (if TRUE) printing any information to the screen.}
}
\details{
See text books on extreme value analysis (EVA) for more on univariate EVA (e.g., Coles, 2001 and Reiss and Thomas, 2007 give fairly accessible introductions to the topic for most audiences; and Beirlant et al., 2004, de Haan and Ferreira, 2006, as well as Reiss and Thomas, 2007 give more complete theoretical treatments).  The extreme value distributions (EVDs) have theoretical support for analyzing extreme values of a process.  In particular, the generalized extreme value (GEV) df is appropriate for modeling block maxima (for large blocks, such as annual maxima), the generalized Pareto (GP) df models threshold excesses (i.e., x - u | x > u and u a high threshold).

The GEV df is given by

    Pr(X <= x) = G(x) = exp[-(1 + shape*(x - location)/scale)^(-1/shape)]

for 1 + shape*(x - location) > 0 and scale > 0.  If the shape parameter is zero, then the df is defined by continuity and simplies to

    G(x) = exp(-exp((x - location)/scale)).

The GEV df is often called a family of distribution functions because it encompasses the three types of EVDs: Gumbel (shape = 0, light tail), Frechet (shape > 0, heavy tail) and the reverse Weibull (shape < 0, bounded upper tail at location - scale/shape).  It was first found by R. von Mises (1936) and also independently noted later by meteorologist A. F. Jenkins (1955).  It enjoys theretical support for modeling maxima taken over large blocks of a series of data.

The generalized Pareo df is given by (Pickands, 1975)

    Pr(X <= x) = F(x) = 1 - [1 + shape*(x - threshold)/scale]^(-1/shape)

where 1 + shape*(x - threshold)/scale > 0, scale > 0, and x > threshold.  If shape = 0, then the GP df is defined by continuity and becomes

    F(x) = 1 - exp(-(x - threshold)/scale).

There is an approximate relationship between the GEV and GP distribution functions where the GP df is approximately the tail df for the GEV df.  In particular, the scale parameter of the GP is a function of the threshold (denote it scale.u), and is equivalent to scale + shape*(threshold - location) where scale, shape and location are parameters from the \dQuote{equivalent} GEV df.  Similar to the GEV df, the shape parameter determines the tail behavior, where shape = 0 gives rise to the exponential df (light tail), shape > 0 the Pareto df (heavy tail) and shape < 0 the Beta df (bounded upper tail at location - scale.u/shape).  Theoretical justification supports the use of the GP df family for modeling excesses over a high threshold (i.e., y = x - threshold).  It is assumed here that \code{x}, \code{q} describe x (not y = x - threshold).  Similarly, the random draws are y + threshold.

If interest is in minima or deficits under a low threshold, all of the above applies to the negative of the data (e.g., - max(-X_1,...,-X_n) = min(X_1, ..., X_n)) and \code{fevd} can be used so long as the user first negates the data, and subsequently realizes that the return levels (and location parameter) given will be the negative of the desired return levels (and location parameter), etc.

The study of extremes often involves a paucity of data, and for small sample sizes, L-moments may give better estimates than competing methods, but penalized MLE (cf. Coles and Dixon, 1999; Martins and Stedinger, 2000; 2001) may give better estimates than the L-moments for such samples.  Martins and Stedinger (2000; 2001) use the terminology generalized MLE, which is also used here.

Non-stationary models:

The current code does not allow for non-stationary models with L-moments estimation.

For MLE/GMLE (see El Adlouni et al 2007 for using GMLE in fitting models whose parameters vary) and Bayesian estimation, linear models for the parameters may be fit using formulas, in which case the \code{data} argument must be supplied.  Specifically, the models allowed for a set of covariates, y, are:

    location(y) = mu0 + mu1 * f1(y) + mu2 * f2(y) + ...

    scale(y) = sig0 + sig1 * g1(y) + sig2 * g2(y) + ...

    log(scale(y)) = phi(y) = phi0 + phi1 * g1(y) + phi2 * g2(y) + ...

    shape(y) = xi0 + xi1 * h1(y) + xi2 * h2(y) + ...

For non-stationary fitting it is recommended that the covariates within the generalized linear models are (at least approximately) centered and scaled (see examples below).  It is generally ill-advised to include covariates in the shape parameter, but there are situations where it makes sense.

Non-stationary modeling is accomplished with \code{fevd} by using formulas via the arguments: \code{threshold.fun}, \code{location.fun}, \code{scale.fun} and \code{shape.fun}.  See examples to see how to do this.


Initial Value Estimates:

In the case of MLE/GMLE, it can be very important to get good initial estimates (e.g., see the examples below).  \code{fevd} attempts to find such estimates, but it is also possible for the user to supply their own initial estimates as a list object using the \code{initial} argument, whereby the components of the list are named according to which parameter(s) they are associated with.  In particular, if the model is non-stationary, with covariates in the location (e.g., mu(t) = mu0 + mu1 * t), then \code{initial} may have a component named \dQuote{location} that may contain either a single number (in which case, by default, the initial value for mu1 will be zero) or a vector of length two giving initial values for mu0 and mu1.

For Bayesian estimation, it is good practice to try several starting values at different points to make sure the initial values do not affect the outcome.  However, if initial values are not passed in, the MLEs are used (which probably is not a good thing to do, but is more likely to yield good results).

For MLE/GMLE, two (in the case of PP, three) initial estimates are calculated along with their associated likelihood values.  The initial estimates that yield the highest likelihood are used.  These methods are:

1.  L-moment estimates.

2.  Let m = mean(xdat) and s = sqrt(6 * var(xdat)) / pi.  Then, initial values assigend for the lcoation parameter when either \code{initial} is NULL or the location component of \code{initial} is NULL, are m - 0.57722 * s.  When \code{initial} or the scale component of \code{initial} is NULL, the initial value for the scale parameter is taken to be s, and when \code{initial} or its shape component is NULL, the initial value for the shape parameter is taken to be 1e-8 (because these initial estimates are moment-based estimates for the Gumbel df, so the initial value is taken to be near zero).

3.  In the case of PP, which is often the most difficult model to fit, MLEs are obtained for a GP model, and the resulting parameter estimates are converted to those of the approximately equivalent PP model.

In the case of a non-stationary model, if the default initial estimates are used, then the intercept term for each parameter is given the initial estimate, and all other parameters are set to zero initially.  The exception is in the case of PP model fitting where the MLE from the GP fits are used, in which case, these parameter estimates may be non-zero.


The generalized MLE (GMLE) method:

This method places a penalty (or prior df) on the shape parameter to help ensure a better fit.  The procedure is nearly identical to MLE, except the likelihood, L, is multiplied by the prior df, p(shape); and because the negative log-likelihood is used, the effect is that of subtracting this term.  Currently, there is no supplied function by this package to calculate the gradient for the GMLE case, so in particular, the trace plot is not the trace of the actual negative log-likelihood (or gradient thereof) used in the estimation.

Bayesian Estimation:

It is possible to give your own prior and proposal distribution functions using the appropriate arguments listed above in the arguments section.  At each iteration of the chain, the parameters are updated one at a time in random order.  The default method uses a random walk chain for the proposal and normal distributions for the parameters.

Plotting output:

\code{plot}: The \code{plot} method function will take information from the \code{fevd} output and make any of various useful plots.  The default, regardless of estimation method, is to produce a 2 by 2 panel of plots giving some common diagnostic plots.  Possible types (determined by the \code{type} argument) include:

1. \dQuote{primary} (default): yields the 2 by 2 panel of plots given by 3, 4, 6 and 7 below.

2. \dQuote{probprob}: Model probabilities against empirical probabilities (obtained from the \code{ppoints} function).  A good fit should yield a straight one-to-one line of points.  In the case of a non-stationary model, the data are first transformed to either the Gumbel (block maxima models) or exponential (POT models) scale, and plotted against probabilities from these standardized distribution functions.  In the case of a PP model, the parameters are first converted to those of the approximately equivalent GP df, and are plotted against the empirical data threshold excesses probabilities.

3. \dQuote{qq}: Empirical quantiles against model quantiles.  Again, a good fit will yield a straight one-to-one line of points.  Generally, the qq-plot is preferred to the probability plot in 1 above.  As in 2, for the non-stationary case, data are first transformed and plotted against quantiles from the standardized distributions.  Also as in 2 above, in the case of the PP model, parameters are converted to those of the GP df and quantiles are from threshold excesses of the data.

4. \dQuote{qq2}: Similar to 3, first data are simulated from the fitted model, and then the qq-plot between them (using the function \code{qqplot} from this self-same package) is made between them, which also yields confidence bands.  Note that for a good fitting model, this should again yield a straight one-to-one line of points, but generally, it will not be as \dQuote{well-behaved} as the plot in 3.  The one-to-one line and a regression line fitting the quantiles is also shown.  In the case of a non-stationary model, simulations are obtained by simulating from an appropriate standardized EVD, re-ordered to follow the same ordering as the data to which the model was fit, and then back transformed using the covariates from \code{data} and the parameter estimates to put the simulated sample back on the original scale of the data.  The PP model is handled analogously as in 2 and 3 above.

5. and 6. \dQuote{Zplot}: These are for PP model fits only and are based on Smith and Shively (1995, see also \url{http://www.stat.unc.edu/postscript/rs/var.pdf}).  The Z plot is a diagnostic for determining whether or not the random variable, Zk, defined as the (possibly non-homogeneous) Poisson intensity parameter(s) integrated from exceedance time k - 1 to exceedance time k (beginning the series with k = 1) is independent exponentially distributed with mean 1.

For the Z plot, it is necessary to scale the Poisson intensity parameter appropriately.  For example, if the data are given on a daily time scale with an annual period basis, then this parameter should be divided by, for example, 365.25.  From the fitted \code{fevd} object, the function will try to account for the correct scaling based on the two components \dQuote{period.basis} and \dQuote{time.units}.  The former currently must be \dQuote{year} and the latter must be one of \dQuote{days}, \dQuote{months}, \dQuote{years}, \dQuote{hours}, \dQuote{minutes} or \dQuote{seconds}.  If none of these are valid for your specific data (e.g., if an annual basis is not desired), then use the \code{d} argument to explicitly specify the correct scaling.


7.  \dQuote{hist}: A histogram of the data is made, and the model density is shown with a blue dashed line.  In the case of non-stationary models, the data are first transformed to an appropriate standardized EVD scale, and the model density line is for the self-same standardized EVD.  Currently, this does not work for non-stationary POT models.

8. \dQuote{density}: Same as 5, but the kernel density (using function \code{density}) for the data is plotted instead of the histogram. In the case of the PP model, block maxima of the data are calculated and the density of these block maxima are compared to the PP in terms of the equivalent GEV df.  If the model is non-stationary GEV, then the transformed data (to a stationary Gumbel df) are used.  If the model is a non-stationary POT model, then currently this option is not available.

9. \dQuote{rl}: Return level plot.  This is done on the log-scale for the abscissa in order that the type of EVD can be discerned from the shape (i.e., heavy tail distributions are concave, light tailed distributions are straight lines, and bounded upper-tailed distributions are convex, asymptoting at the upper bound).  95 percent CIs are also shown (gray dashed lines).  In the case of non-stationary models, the data are plotted as a line, and the \dQuote{effective} return levels (by default the 2-period (i.e., the median), 20-period and 100-period are used; period is usually annual) are also shown (see, e.g., Gilleland and Katz, 2011).  In the case of the PP model, the equivalent GEV df (stationary model) is assumed and data points are block maxima, where the blocks are determined from information passed in the call to \code{fevd}.  In particular, the \code{span} argument (which, if not passed by the user, will have been determined by \code{fevd} using \code{time.units} along with the number of points per year (which is estimated from \code{time.units}) are used to find the blocks over which the maxima are taken.  For the non-stationary case, the equivalent GP df is assumed and parameters are converted.  This helps facilitate a more meaningful plot, e.g., in the presence of a non-constant threshold, but otherwise constant parameters.

10. \dQuote{trace}: In each of cases (b) and (c) below, a 2 by the number of parameters panel of plots are created.

    (a) L-moments: Not available for the L-moments estimation.

    (b) For MLE/GMLE, the likelihood traces are shown for each parameter of the model, whereby all but one parameter is held fixed at the MLE/GMLE values, and the negative log-likelihood is graphed for varying values of the parameter of interest.  Note that this differs greatly from the profile likelihood (see, e.g., \code{profliker}) where the likelihood is maximized over the remaining parameters.  The gradient negative log-likelihoods are also shown for each parameter.  These plots may be useful in diagnosing any fitting problems that might arise in practice.  For ease of interpretation, the gradients are shown directly below the likleihoods for each parameter.

    (c) For Bayesian estimation, the usual trace plots are shown with a gray vertical dashed line showing where the \code{burn.in} value lies; and a gray dashed horizontal line through the posterior mean.  However, the posterior densities are also displayed for each parameter directly above the usual trace plots.  It is not currently planned to allow for adding the prior dnsities to the posterior density graphs, as this can be easily implemented by the user, but is more difficult to do generally.

As with \code{ci} and \code{distill}, only \code{plot} need be called by the user.  The appropriate choice of the other functions is automatically determined from the \code{fevd} fitted object.

Note that when \code{blocks} are provided to \code{fevd}, certain plots
that require the full set of observations (including non-exceedances)
cannot be produced.

Summaries and Printing:

\code{summary} and \code{print} method functions are available, and give different information depending on the estimation method used.  However, in each case, the parameter estimates are printed to the screen.  \code{summary} returns some useful output (similar to distill, but in a list format).  The \code{print} method function need not be called as one can simply type the name of the \code{fevd} fitted object and return to execute the command (see examples below).  The deviance information criterion (DIC) is calculated for the Bayesian estimation method as DIC = D(mean(theta)) + 2 * pd, where pd = mean(D(theta)) - D(mean(theta)), and D(theta) = -2 * log-likelihood evaluated at the parameter values given by theta.  The means are taken over the posterior MCMC sample.  The default estimation method for the parameter values from the MCMC sample is to take the mean of the sample (after removing the first burn.in samples).  A good alternative is to set the \code{FUN} argument to \dQuote{postmode} in order to obtain the posterior mode of the sample.

Using Blocks to Reduce Computation in PP Fitting:

If \code{blocks} is supplied, the user should
provide only the exceedances and not all of the data values. For
stationary models, the list should contain a component called
\code{nBlocks} indicating the number of observations within a block, where
blocks are defined in a manner analogous to that used in GEV
models. For nonstationary models, the list may contain one or more of
several components. For nonstationary models with covariates, the list
should contain a \code{data} component analogous to the \code{data} argument,
providing values for the blocks. If the threshold varies, the list
should contain a \code{threshold} component that is analogous to the
\code{threshold} argument. If some of the observations within any block are
missing (assuming missing at random or missing completely at random),
the list should contain a \code{proportionMissing} component that is a
vector with one value per block indicating the proportion of
observations missing for the block. To weight the blocks, the list can
contain a \code{weights} component with one value per block. Warning: to
properly analyze nonstationary models, any covariates, thresholds, and
weights must be constant within each block.

}
\value{
\code{fevd}: A list object of class \dQuote{fevd} is returned with components:

    \item{call }{the function call.  Used as a default for titles in plots and output printed to the screen (e.g., by \code{summary}).}
    \item{data.name }{character vector giving the name of arguments: \code{x} and \code{data}.  This is used by the \code{datagrabber} method function, as well as for some plot labeling.}
    \item{data.pointer, cov.pointer, cov.data, x, x.fun}{Not all of these are included, and which ones are depend on how the data are passed to \code{fevd}.  These may be character strings, vectors or data frames depending again on the original function call.  They are used by \code{datagrabber} in order to obtain the original data.  If x is a column of data, then x.fun is a formula specifying which column.  Also, if x is a formula, then x.fun is this self-same formula.}
    \item{in.data}{logical; is the argument \code{x} a column of the argument \code{data} (TRUE) or not (FALSE)?}
    \item{method}{character string naming which estimation method ws used for the fit.  Same as \code{method} argument.}
    \item{type}{character string naming which EVD was fit to the data.  Same as \code{type} argument.}
    \item{period.basis}{character string naming the period for return periods.  This is used in plot labeling and naming, e.g., output from \code{ci}.  Same as the argument passed in.}
    \item{units}{Not present if not passed in by the user.  character string naming the data units.  Used for plot labeling.}
    \item{par.models}{A list object giving the values of the threshold and parameter function arguments, as well as a logical stating whether the log of the scale parameter is used or not.  This is present even if it does not make sense (i.e., for L-moments estimation) because it is used by the \code{is.fixedfevd} function for determining whether or not a model is stationary or not.}
    \item{const.loc, const.scale, const.shape}{logicals stating whether the named parameter is constant (TRUE) or not (FALSE).  Currently, not used, but could be useful.  Still present even for L-moments estimation even though it does not really make sense in this context (will always be true).}
    \item{n}{the length of the data to which the model is fit.}
    \item{span, npy}{For POT models only, this is the estimated number of periods (usually years) and number of points per period, both estimated using time.units}
    \item{na.action}{character naming the function used for handling missing values.}
    \item{results}{If L-moments are used, then this is a simple vector of length equal to the number of parameters of the model.  For MLE/GMLE, this is a list object giving the output from \code{optim} (in particular, the par component is a vector giving the parameter estimates).  For Bayesian estimation, this is a matrix whose rows give the results for each iteration of the MCMC simulations.  The columns give the parameters, as well as an additional last column that gives the number of parameters that were updated at each iteration.}
    \item{priorFun, priorParams}{These are only present for GMLE and Bayesian estimation methods.  They give the prior df and optional arguments used in the estimation.}
    \item{proposalFun, proposalParams}{These are only present for Bayesian estimation, and they give the proposal function used along with optional arguments.}
    \item{chain.info}{If estimation method is \dQuote{Bayesian}, then this component is a matrix whose first several columns give 0 or 1 for each parameter at each iteration, where 0 indicates that the parameter was not updated and 1 that it was.  The first row is NA for these columns.  the last two columns give the likelihood and prior values for the current parameter values.}
    \item{chain.info}{matrix whose first n columns give a one or zero depending on whether the parameter was updated or not, resp.  The last two columns give the log-likelihood and prior values associated with the parameters of that sample.}
    \item{blocks}{Present only if \code{blocks} is supplied as an
    argument. Will contain the input information and computed
    block-level information for use in post-processing the model object,
    in particular block-wise design matrices.}
    
\code{print}: Does not return anything.  Information is printed to the screen.


\code{summary}: Depending on the estimation method, either a numeric vector giving the parameter estimates (\dQuote{Lmoments}) or a list object (all other estimation methods) is returned invisibly, and if \code{silent} is FALSE (default), then summary information is printed to the screen.  List components may include:

    \item{par, se.theta}{numeric vectors giving the parameter and standard error estimates, resp.}
    \item{cov.theta}{matrix giving the parameter covariances.}
    \item{nllh}{number giving the value of the negative log-likelihood.}
    \item{AIC, BIC, DIC}{numbers giving the Akaike Information Criterion (AIC, Akaike, 1974), the Bayesian Information Criterion (BIC, Schwarz, 1978), and/or the Deviance Information Criterion, resp.}
}

\references{
Akaike, H. (1974).  A new look at the statistical model identification.  \emph{IEEE Transactions on Automatic Control}, \bold{19}, 716--723.

Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004). \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S. G.  (2001). \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag.

Coles, S. G. and Dixon, M. J. (1999).  Likelihood-based inference for extreme value models.  \emph{Extremes}, \bold{2} (1), 5--23.

El Adlouni, S., Ouarda, T. B. M. J., Zhang, X., Roy, R, and Bob\'{e}e, B. (2007).  Generalized maximum likelihood estimators for the nonstationary generalized extreme value model.  \emph{Water Resources Research}, \bold{43}, W03410, doi: 10.1029/2005WR004545, 13 pp.

Gilleland, E. and Katz, R. W. (2011). New software to analyze how extremes change over time. \emph{Eos}, 11 January, \bold{92}, (2), 13--14.

de Haan, L. and Ferreira, A. (2006). \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Jenkinson, A. F. (1955). The frequency distribution of the annual maximum (or minimum) of meteorological elements. \emph{Quart. J. R.  Met. Soc.}, \bold{81}, 158--171.

Martins, E. S. and Stedinger, J. R. (2000).  Generalized maximum likelihood extreme value quantile estimators for hydrologic data.  \emph{Water Resources Research}, \bold{36} (3), 737--744.

Martins, E. S. and Stedinger, J. R. (2001).  Generalized maximum likelihood Pareto-Poisson estimators for partial duration series.  \emph{Water Resources Research}, \bold{37} (10), 2551--2557.

Pickands, J. (1975). Statistical inference using extreme order statistics.  \emph{Annals of Statistics}, \bold{3}, 119--131.

Reiss, R.-D. and Thomas, M. (2007). \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkh\"{a}user, 530pp., 3rd edition.

Schwarz, G. E. (1978).  Estimating the dimension of a model.  \emph{Annals of Statistics}, \bold{6}, 461--464.

Smith, R. L. and Shively, T. S. (1995).  A point process approach to modeling trends in tropospheric ozone.  \emph{Atmospheric Environment}, \bold{29}, 3489--3499.

von Mises, R. (1936). La distribution de la plus grande de n valeurs, \emph{Rev. Math. Union Interbalcanique} \bold{1}, 141--160.
}
\author{
Eric Gilleland
}
\seealso{

\code{\link{ci.fevd}} for obtaining parameter and return level confidence intervals.

\code{distill.fevd} for stripping out a vector of parameter estimates and perhaps other pertinent information from an fevd object.

For functions to find the density, probability df, quantiles and simulate data from, an EV df, see:
    \code{\link{devd}}, \code{\link{pevd}}, \code{\link{qevd}}, \code{\link{revd}}

For functions to find the probability df and simulate random data from a fitted model from \code{fevd}, see:
    \code{\link{pextRemes}}, \code{\link{rextRemes}}

For functions to determine if the extreme data are independent or not, see:
    \code{\link{extremalindex}}, \code{\link{atdf}}

For functions to help choose a threshold, see: \code{\link{threshrange.plot}}, \code{\link{mrlplot}}

To decluster stationary dependent extremes, see: \code{\link{decluster}}

For more on formulas in R, see: \code{\link{formula}}

To grab the parameters of a fitted \code{fevd} model, see: \code{\link{findpars}}

To calculate the parameter covariance, see:
    \code{\link{optimHess}}, \code{\link{parcov.fevd}}

To see more about the \pkg{extRemes} method functions described here, see: \code{\link{ci}} and \code{\link{distill}}

To calculate effective return levels and CI's for MLE and Bayesian estimation of non-stationary models, see \code{\link{ci.rl.ns.fevd.bayesian}}, \code{\link{ci.rl.ns.fevd.mle}} and \code{\link{return.level}}

To obtain the original data set from a fitted \code{fevd} object, use: \code{\link{datagrabber}}

To calculate the profile likelihood, see: \code{\link{profliker}}

To test the statistical significance of nested models with additional parameters, see: \code{\link{lr.test}}

To find effective return levels for non-stationary models, see: \code{\link{erlevd}}

To determine if an \code{fevd} object is stationary or not, use: \code{\link{is.fixedfevd}} and \code{\link{check.constant}}

For more about the plots created for \code{fevd} fitted objects, see:
    \code{\link{ppoints}}, \code{\link{density}}, \code{\link{hist}}, \code{\link{qqplot}}

For general numerical optimization in R, see: \code{\link{optim}}
}
\examples{

z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit
plot(fit)
plot(fit, "trace")

\dontrun{
## Fitting the GEV to block maxima.

# Port Jervis, New York winter maximum and minimum
# temperatures (degrees centigrade).
data(PORTw)

# Gumbel
fit0 <- fevd(TMX1, PORTw, type="Gumbel", units="deg C")
fit0
plot(fit0)
plot(fit0, "trace")
return.level(fit0)

# GEV
fit1 <- fevd(TMX1, PORTw, units="deg C")
fit1
plot(fit1)
plot(fit1, "trace")
return.level(fit1)
return.level(fit1, do.ci=TRUE)
ci(fit1, return.period=c(2,20,100)) # Same as above.

lr.test(fit0, fit1)
ci(fit1, type="parameter")
par(mfrow=c(1,1))
ci(fit1, type="parameter", which.par=3, xrange=c(-0.4,0.01),
    nint=100, method="proflik", verbose=TRUE)

# 100-year return level
ci(fit1, method="proflik", xrange=c(22,28), verbose=TRUE)

plot(fit1, "probprob")
plot(fit1, "qq")
plot(fit1, "hist")
plot(fit1, "hist", ylim=c(0,0.25))

# Non-stationary model.
# Location as a function of AO index.

fit2 <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit2
plot(fit2)
plot(fit2, "trace")
# warnings are not critical here.
# Sometimes the nllh or gradients
# are not defined.

return.level(fit2)

v <- make.qcov(fit2, vals=list(mu1=c(-1, 1)))
return.level(fit2, return.period=c(2, 20, 100), qcov=v)

# Note that first row is for AOindex = -1 and second
# row is for AOindex = 1.

lr.test(fit1, fit2)
# Also compare AIC and BIC

look1 <- summary(fit1, silent=TRUE)
look1 <- c(look1$AIC, look1$BIC)

look2 <- summary(fit2, silent=TRUE)
look2 <- c(look2$AIC, look2$BIC)

# Lower AIC/BIC is better.
names(look1) <- names(look2) <- c("AIC", "BIC")
look1
look2

par(mfrow=c(1,1))
plot(fit2, "rl")


## Fitting the GP df to threshold excesses.

# Hurricane damage data.

data(damage)

ny <- tabulate(damage$Year)
# Looks like only, at most, 5 obs per year.

ny <- mean(ny[ny > 0])
fit0 <- fevd(Dam, damage, threshold=6, type="Exponential", time.units="2.05/year")
fit0
plot(fit0)
plot(fit0, "trace") # ignore the warning.

fit1 <- fevd(Dam, damage, threshold=6, type="GP", time.units="2.05/year")
fit1
plot(fit1) # ignore the warning.
plot(fit1, "trace")

return.level(fit1)

# lr.test(fit0, fit1)

# Fort Collins, CO precipitation

data(Fort)

## GP df

fit <- fevd(Prec, Fort, threshold=0.395, type="GP", units="inches", verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")

ci(fit, type="parameter")
par(mfrow=c(1,1))
ci(fit, type="return.level", method="proflik", xrange=c(4,7.5), verbose=TRUE)
# Can check using locator(2).

ci(fit, type="parameter", which.par=2, method="proflik", xrange=c(0.1, 0.3),
    verbose=TRUE) 
# Can check using locator(2).


## PP model.

fit <- fevd(Prec, Fort, threshold=0.395, type="PP", units="inches", verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")
ci(fit, type="parameter")

# Same thing, but just to try a different optimization method.
# And, for fun, a different way of entering the data set.
fit <- fevd(Fort$Prec, threshold=0.395, type="PP",
    optim.args=list(method="Nelder-Mead"), units="inches", verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")
ci(fit, type="parameter")

## PP model with blocks argument for computational efficiency # CJP2

system.time(fit <- fevd(Prec, Fort, threshold=0.395, type="PP", units="inches", verbose=TRUE))

FortSub = Fort[Fort$Prec > 0.395, ]
system.time(fit.blocks <- fevd(Prec, FortSub, threshold=0.395,
type="PP", units="inches", blocks = list(nBlocks = 100), verbose=TRUE))
fit.blocks
plot(fit.blocks)
plot(fit.blocks, "trace")
ci(fit.blocks, type="parameter")

#
# Phoenix data
#
# Summer only with 62 days per year.

data(Tphap)

plot(MinT~ Year, data=Tphap)

# GP df
fit <- fevd(-MinT ~1, Tphap, threshold=-73, type="GP", units="deg F",
    time.units="62/year", verbose=TRUE)

fit
plot(fit)
plot(fit, "trace")

# PP
fit <- fevd(-MinT ~1, Tphap, threshold=-73, type="PP", units="deg F", time.units="62/year",
    use.phi=TRUE, optim.args=list(method="BFGS", gr=NULL), verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")


# Non-stationary models

fit <- fevd(Prec, Fort, threshold=0.395,
    scale.fun=~sin(2 * pi * (year - 1900)/365.25) + cos(2 * pi * (year - 1900)/365.25),
    type="GP", use.phi=TRUE, verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")
ci(fit, type="parameter")

# Non-constant threshold.

# GP
fit <- fevd(Prec, Fort, threshold=0.475, threshold.fun=~I(-0.15 * cos(2 * pi * month / 12)),
    type="GP", verbose=TRUE)
fit
plot(fit)
par(mfrow=c(1,1))
plot(fit, "rl", xlim=c(0, 365))

# PP

fit <- fevd(Prec, Fort, threshold=0.475, threshold.fun=~I(-0.15 * cos(2 * pi * month / 12)),
    type="PP", verbose=TRUE)
fit
plot(fit)

## Bayesian PP with blocks for computational efficiency
## Note that 1999 iterations may not be sufficient; used here to
## minimize time spent fitting.
# CJP2
## CJP2: Eric, CRAN won't like this being run as part of the examples
## as it takes a long time; we'll probably want to wrap this in a \dontrun{}

set.seed(0)
system.time(fit <- fevd(Prec, Fort, threshold=0.395,
    scale.fun=~sin(2 * pi * (year - 1900)/365.25) + cos(2 * pi * (year - 1900)/365.25),
    type="PP", method="Bayesian", iter=1999, use.phi=TRUE, verbose=TRUE))
fit
ci(fit, type="parameter")

set.seed(0)
FortSub <- Fort[Fort$Prec > 0.395, ]
system.time(fit2 <- fevd(Prec, FortSub, threshold=0.395,
    scale.fun=~sin(2 * pi * (year - 1900)/365.25) + cos(2 * pi * (year -
1900)/365.25), type="PP", blocks = list(nBlocks= 100, data =
data.frame(year = 1900:1999)), use.phi=TRUE, method = "Bayesian",
iter=1999, verbose=TRUE))
# an order of magnitude faster

fit2
ci(fit2, type="parameter")

data(ftcanmax)

fit <- fevd(Prec, ftcanmax, units="inches")
fit

plot(fit)
par(mfrow=c(1,1))
plot(fit, "probprob")
plot(fit, "hist")
plot(fit, "hist", ylim=c(0,0.01))
plot(fit, "density", ylim=c(0,0.01))
plot(fit, "trace")

distill(fit)
distill(fit, cov=FALSE)

fit2 <- fevd(Prec, ftcanmax, location.fun=~Year)
fit2

plot(fit2)
##
# plot(fit2, "trace") # Gives warnings because of some NaNs produced
                      # (nothing to worry about).

lr.test(fit, fit2)

ci(fit)
ci(fit, type="parameter")

fit0 <- fevd(Prec, ftcanmax, type="Gumbel")
fit0

plot(fit0)
lr.test(fit0, fit)
plot(fit0, "trace")

ci(fit, return.period=c(2, 20, 100))
ci(fit, type="return.level", method="proflik", return.period=20, verbose=TRUE)

ci(fit, type="parameter", method="proflik", which.par=3, xrange=c(-0.1,0.5), verbose=TRUE)

# L-moments
fitLM <- fevd(Prec, ftcanmax, method="Lmoments", units="inches")
fitLM # less info.
plot(fitLM)
# above is slightly slower because of the parametric bootstrap
# for finding CIs in return levels.
par(mfrow=c(1,1))
plot(fitLM, "density", ylim=c(0,0.01))

# GP model.
# CJP2 : fixed so have 744/year (31 days *24 hours/day)

data(Denversp)

fitGP <- fevd(Prec, Denversp, threshold=0.5, type="GP", units="mm",
    time.units="744/year", verbose=TRUE)

fitGP
plot(fitGP)
plot(fitGP, "trace")
# you can see the difficulty in getting good numerics here.
# the warnings are not a coding problem, but challenges in 
# the likelihood for the data.

# PP model.
fitPP <- fevd(Prec, Denversp, threshold=0.5, type="PP", units="mm",
    time.units="744/year", verbose=TRUE)

fitPP
plot(fitPP)
plot(fitPP, "trace")

fitPP <- fevd(Prec, Denversp, threshold=0.5, type="PP", optim.args=list(method="Nelder-Mead"),
    time.units="744/year", units="mm", verbose=TRUE)
fitPP
plot(fitPP) # Much better.
plot(fitPP, "trace")
# Better than above, but can see the difficulty!
# Can see the importance of good starting values!

# Try out for small samples
# Using one of the data example from Martins and Stedinger (2000)
z <- c( -0.3955, -0.3948, -0.3913, -0.3161, -0.1657, 0.3129, 0.3386, 0.5979,
    1.4713, 1.8779, 1.9742, 2.0540, 2.6206, 4.9880, 10.3371 )

tmpML <- fevd( z ) # Usual MLE.

# Find 0.999 quantile for the MLE fit.
# "True" 0.999 quantile is around 11.79
p <- tmpML$results$par
qevd( 0.999, loc = p[ 1 ], scale = p[ 2 ], shape = p[ 3 ] )

tmpLM <- fevd(z, method="Lmoments")
p <- tmpLM$results
qevd( 0.999, loc = p[ 1 ], scale = p[ 2 ], shape = p[ 3 ] )

tmpGML <- fevd(z, method="GMLE")
p <- tmpGML$results$par
qevd( 0.999, loc = p[ 1 ], scale = p[ 2 ], shape = p[ 3 ] )

plot(tmpLM)
dev.new()
plot(tmpGML)

# Bayesian
fitB <- fevd(Prec, ftcanmax, method="Bayesian", verbose=TRUE)
fitB
plot(fitB)
plot(fitB, "trace")

# Above looks good for scale and shape, but location does not appear to have found its way.
fitB <- fevd(Prec, ftcanmax, method="Bayesian", priorParams=list(v=c(1, 10, 10)), verbose=TRUE)
fitB
plot(fitB)
plot(fitB, "trace")

# Better, but what if we start with poor initial values?
fitB <- fevd(Prec, ftcanmax, method="Bayesian", priorParams=list(v=c(0.1, 10, 0.1)),
    initial=list(location=0, scale=0.1, shape=-0.5)), verbose=TRUE)
fitB
plot(fitB)
plot(fitB, "trace")

##
## Non-constant threshold.
##
data(Tphap)

# Negative of minimum temperatures.
plot(-Tphap$MinT)

fitGP2 <- fevd(-MinT ~1, Tphap, threshold=c(-70,-7), threshold.fun=~I((Year - 48)/42), type="GP",
    time.units="62/year", verbose=TRUE)
fitGP2
plot(fitGP2)
plot(fitGP2, "trace")
par(mfrow=c(1,1))
plot(fitGP2, "hist")
plot(fitGP2, "rl")

ci(fitGP2, type="parameter")

##
## Non-stationary models.
##

data(PORTw)

# GEV
fitPORTstdmax <- fevd(PORTw$TMX1, PORTw, scale.fun=~STDTMAX, use.phi=TRUE)
plot(fitPORTstdmax)
plot(fitPORTstdmax, "trace")
# One can see how finding the optimum value numerically can be tricky!

# Bayesian
fitPORTstdmaxB <- fevd(PORTw$TMX1, PORTw, scale.fun=~STDTMAX, use.phi=TRUE,
    method="Bayesian", verbose=TRUE)
fitPORTstdmaxB
plot(fitPORTstdmaxB)
plot(fitPORTstdmaxB, "trace")

# Let us go crazy.
fitCrazy <- fevd(PORTw$TMX1, PORTw, location.fun=~AOindex + STDTMAX, scale.fun=~STDTMAX,
    shape.fun=~STDMIN, use.phi=TRUE)
fitCrazy
plot(fitCrazy)
plot(fitCrazy, "trace")
# With so many parameters, you may need to stretch the device
# using your mouse in order to see them well.

ci(fitCrazy, type="parameter", which=2) # Hmmm.  NA NA.  Not good.
ci(fitCrazy, type="parameter", which=2, method="proflik", verbose=TRUE)
# Above not quite good enough (try to get better bounds).

ci(fitCrazy, type="parameter", which=2, method="proflik", xrange=c(0, 2), verbose=TRUE)
# Much better.


##
## Center and scale covariate.
##
data(Fort)

fitGPcross <- fevd(Prec, Fort, threshold=0.395,
    scale.fun=~cos(day/365.25) + sin(day/365.25) + I((year - 1900)/99),
    type="GP", use.phi=TRUE, units="inches")

fitGPcross
plot(fitGPcross) # looks good!

# Get a closer look at the effective return levels.
par(mfrow=c(1,1))
plot(fitGPcross, "rl", xlim=c(10000,12000))

lr.test(fitGPfc, fitGPcross)

}

}
\keyword{ models }
\keyword{ univar }
\keyword{ ts }
\keyword{ distribution }
\keyword{ hplot }
\keyword{ manip }
\keyword{ optimize }
\keyword{ print }
