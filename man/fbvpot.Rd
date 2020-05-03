\name{fbvpot}
\alias{fbvpot}
\title{
	Estimate the Bivariate Peaks-Over-Threshold (POT) Model
}
\description{
	Fit the bivariate POT model to data.
}
\usage{
fbvpot(x, threshold, dep.model = "logistic", na.action = na.fail,
       init = 0.5, lower = 0, upper = 1, cutoff, parnames,
       tform = "tformRankFrechet", ...)
}
\arguments{
  \item{x}{
	  \code{n} by 2 matrix giving each component of the bivariate vectors to which the bivariate peaks-over-threshold (POT) model is to be fit.
}
  \item{threshold}{
	  A single number, \code{n} by 2 matrix, or length 2 vector.  If a single number, this value will be used as the threshold over which the marginal POT models will be fit to each component variable.  If an \code{n} by 2 matrix, then each column will be a vector of thresholds for each component variable, respectively.  If a length 2 vector, then the first element is used for the threshold in fitting the POT model to the first component, and the second to the second.
}
  \item{dep.model}{
	  character string giving the name of a bivariate dependence model function. The user may make their own function; such functions must take arguments \code{w}, \code{p} and \code{\ldots} and should return a a vector of the likelihood values.  A second function with the same name, but ending in \dQuote{LH} that takes the same input arguments, calls the other function, and returns the single numeric value giving the likelihood.  Options included with this package include: \code{logistic} and \code{mixbeta}.
}
  \item{na.action}{
	  function determining how missing values should be handled.  Default is to error out.
}
  \item{init}{
	  vector giving an initial guess for each parameter in the dependence model defined by \code{dep.model}.
}
  \item{lower, upper}{
Arguments to the \code{nlminb} function, which is used to optimize the likelihood numerically.
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~ Dan?
}
  \item{parnames}{
	  optional character vector giving the names of the parameters of the dependence model.
}
  \item{tform}{
	  character string giving the name of the function to be used to transform the component variables to the same scale.  Default transforms to the Frechet scale using the rank transformation.
}
  \item{\dots}{
	  optional arguments to the \code{tform} function.
}
}
\details{

    See Beirlant et al. (2004) for more about bivariate threshold exceedance modeling, as well as Coles and Tawn (1994).

}
\value{
	A list object of class \dQuote{fbvpot} with components:
  \item{orig.data }{original matrix of the data from the x argument.}
  \item{ threshold, cutoff, init, lower, upper, parnames }{Same as values passed in through the input arguments.}
  \item{Frechet.transformed.data}{matrix with the Frechet-transformed data.}
  \item{ radial, angular }{ vectors giving the radial and angular components, respectively.}
  \item{ sorting, polar }{} % Dan?
  \item{ model, LH }{ function giving the dependence model used and its companion likelihood function, respectively. }
  \item{ model.name }{ character naming the dependence model used. }
  \item{ fit }{ the result of nlminb giving the optimized parameter value(s).}
  \item{ call}{ original call to this function. }
  \item{ data.name }{ character string giving the name of the data used in the x argument. }
}
\references{

    Beirlant, J., Y. Goegebeur, J. Segers, and J. Teugels (2004) \emph{Statistics of Extremes: Theory and Applications}.  Wiley, West Sussex, England, United Kingdom, 514 pp.

	 Coles, S. G. and J. A. Tawn (1994) Statistical methods for multivariate extremes: an application to structural design (with Discussion).  \emph{Appl. Statist.}, \bold{43}, 1--48, doi: 10.2307/2986112.
}
\author{
	Dan Cooley and Eric Gilleland
}
\seealso{
\code{\link{logistic}}, \code{\link{mixbeta}}, \code{\link{bvpotbooter}}
}
\examples{
data( "SantaAna" )

Z <- SantaAna[,3:4]

mfit1 <- fevd( x = temp, data = Z, threshold = 36.75, type = "GP" )
mfit2 <- fevd( x = windspeeds, data = Z, threshold = 7.09875, type = "GP" )

fit2 <- fbvpot( x = Z, threshold = apply( Z, 2, quantile, probs = 0.95 ),
	       tform = "tf", fit = list( mfit1, mfit2 ) )
fit2

plot( fit2 )

hist( fit2 )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{ multivariate }% __ONLY ONE__ keyword per line
