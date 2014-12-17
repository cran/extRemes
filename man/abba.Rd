\name{abba}
\alias{abba}
\alias{abba_latent}
\title{Implementation of Stephenson-Shaby-Reich-Sullivan}
\description{
  Implements MCMC methodology for fitting spatial exterme
  value models using Stephenson et al. (2014). Experimental.
}
\usage{
abba(y, sites, iters, Qb = NULL, knots = sites, X = cbind(1, sites), 
  beta = NULL, alpha = 0.5, logbw = 0, tau = c(1, 1, 1), 
  logs = matrix(0, nrow = nf, ncol = nt), 
  u = matrix(0.5, nrow = nf, ncol = nt), 
  MHbeta = matrix(rep(c(0.15, 0.03, 0.015), each = n), ncol = 3), 
  MHalpha = 0.01, MHlogbw = 0, 
  MHs = matrix(0.5, nrow = nf, ncol = nt), 
  MHu = matrix(2.5, nrow = nf, ncol = nt), 
  pribeta = c(10, 10, 10), prialpha = c(1, 1), prilogbw = c(0, 1), 
  pritau = c(0.1, 0.1, 0.1), trace = 0) 
abba_latent(y, sites, iters, Qb = NULL, X = cbind(1, sites), 
  beta = NULL, tau = c(1, 1, 1), 
  MHbeta = matrix(rep(c(0.15, 0.03, 0.015), each = n), ncol = 3), 
  pribeta = c(10, 10, 10), pritau = c(0.1, 0.1, 0.1), trace = 0)
}
\arguments{
  \item{y}{A numeric matrix. The data for the ith site should be 
    in the ith row. Missing values are allowed, however each site must have
    at least one non-missing value. If starting values for \code{beta} are
    not specified, then each site must have at least two non-missing values.}
  \item{sites}{A numeric matrix with 2 columns giving site locations. It
    is best to normalized the columns.}
  \item{iters}{The number of iterations in the MCMC chain.}
  \item{Qb}{A symmetric non-negative definite neighbourhood matrix. If
    not specified, the off-diagonal elements are taken to be proportional
    to the negative inverse of the squared Euclidean distance, with the 
    diagonal elements specified so that the rows and columns sum to zero.
    It is probably better to specify your own neighbourhood structure.
    Note that this implementation does not explicitly take advantage of any 
    sparsity, so having a large numbers of zeros will not necessarily speed 
    things up.}
  \item{knots}{A numeric matrix with 2 columns giving knot locations. By
    default the knots are taken to be the site locations.}
  \item{X}{The design matrix or matrices of the GEV parameters. Should
    be a list of length three containing design matrices for the location,
    log scale and shape respectively. Can also be a matrix, which is then
    used by all three parameters. By default, an intercept and the 2 columns
    in \code{sites} are used. Note that the intercept will not be affected by
    the data if the rows and columns of \code{Qb} sum to zero.}
  \item{beta}{A matrix with 3 columns with GEV parameter starting values for 
    every site. If not specified, marginal method of moment estimators, 
    assuming a zero shape, are used.}
  \item{alpha}{Starting value for alpha.}
  \item{logbw}{Starting value for the log bandwidth.}  
  \item{tau}{Starting values for tau, for the three GEV parameters. This is the
    inverse of the delta values in the publication, so a lack of variation
    corresponds to large values. Since the posterior for the tau values has a 
    closed form, this argument is relatively unimportant as it usually affects only 
    the first couple of iterations.}
  \item{logs}{A matrix with rows equal to the number of knots and columns equal 
    to the number of columns in \code{y}. Gives the starting values for the log
    of the positve stable variables.}
  \item{u}{A matrix with rows equal to the number of knots and columns equal 
    to the number of columns in \code{y}. Gives the starting values for the U
    variables.}
  \item{MHbeta}{A matrix with 3 columns with GEV parameter jump standard deviations
    for every site.}
  \item{MHalpha}{Jump standard deviation value for alpha. It can be set to zero
    to fix alpha at the starting value.}
  \item{MHlogbw}{Jump standard deviation value for the log bandwidth. It can be 
    set to zero to fix the bandwidth at the starting value.}  
  \item{MHs}{A matrix with rows equal to the number of knots and columns equal 
    to the number of columns in \code{y}. Gives the jump standard deviations 
    for the log of the positve stable variables. It can be challenging to specify
    this to make the acceptance rates reasonable.}
  \item{MHu}{A matrix with rows equal to the number of knots and columns equal 
    to the number of columns in \code{y}. Gives the jump standard deviations 
    for the U variables.} 
  \item{pribeta}{A vector of length three giving prior parameters for the three
    beta vectors. For simplicity each beta has a MVN(0, pI) prior, where p is a 
    single parameter and I is the identity matrix of dimension corresponding to 
    \code{X}.}
  \item{prialpha}{Shape1 and shape2 parameters of the prior beta distribution 
    for alpha. See \code{\link{rbeta}}.}
  \item{prilogbw}{Mean and standard deviation parameters for the prior normal
    distribution for log bandwidth. See \code{\link{rnorm}}.}
  \item{pritau}{A vector of length three giving prior parameters for each of the
    three taus. For simplicity each tau has a beta(p,p) prior, where p is a 
    single parameter, equal to both shape1 and shape2.See \code{\link{rbeta}}.}
  \item{trace}{Prints the log posterior density after every \code{trace} iterations.
    Use zero to supress printing.}
}
\details{
  The function \code{abba} implements the method of Stephenson et al. (2014), 
  which is a variation of Reich and Shaby (2012). The function \code{abba_latent}
  implements a standard latent variable approach which is a special case of 
  \code{abba}, obtained when the parameter alpha is equal to one.
  
  The function \code{abba} can be challenging to implement. In particular, it can be 
  difficult to specify \code{MHs} to achieve suitable acceptance rates for all
  positive stable random variables. Also, alpha and the bandwidth may mix slowly.
  It is recommended that (i) the variables in \code{sites}, \code{knots} and \code{X}
  are standardized, and that (ii) the function \code{abba_latent} be used first in 
  order to pass on good starting values to \code{abba}, and that (iii) you consider
  fixing either alpha or the bandwidth if there is slow mixing.
}
\value{
  A list object with the following components

  \item{beta.samples}{A three dimensional array containing the simulated GEV parameter
    values for each site. The first dimension is the number of iterations, the second 
    is the number of sites, and the third corresponds to the three GEV parameters of 
    location, log scale and shape.}
  \item{param.samples}{A matrix containing the linear predictor and tau parameters, and
    for \code{abba} also alpha and log bandwidth. The last column contains log 
    posterior values.}
  \item{psrv.samples}{Only exists for function \code{abba}. A three dimensional array 
    containing the simulated postive stable variables. The first dimension is the 
    number of iterations, the second is the number of knots, and the third is the number
    of columns in \code{y}.}
  \item{urv.samples}{Only exists for function \code{abba}. A three dimensional array 
    containing the simulated U variables. The first dimension is the number of 
    iterations, the second is the number of knots, and the third is the number
    of columns in \code{y}.}
}
\references{
  Stephenson, A. G., Shaby, B.A., Reich, B.J. and Sullivan, A.L. (2015). 
  Estimating spatially varying severity thresholds of the forest fire danger 
  rating system using max-stable extreme event modelling. 
  Journal of Applied Meteorology and Climatology. In Press.
  
  Reich, B.J. and Shaby, B.A. (2012).
	A hierarchical max-stable spatial model for extreme precipitation.
	Ann. Appl. Stat. 6(4), 1430-1451
}
\seealso{
  \code{\link{rbeta}}, \code{\link{rnorm}}
}
\examples{
  dat <- matrix(-log(rexp(9 * 20)), nrow = 9, ncol = 20)
  sites <- cbind(rep(c(-1,0,1),3), rep(c(-1,0,1),each = 3))
  abba_latent(dat, sites, iters = 50, trace = 10)
}
\keyword{models}


