\name{logistic}
\alias{logistic}
\alias{logisticLH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Logistic Dependence Model Likelihood
}
\description{
	Calculate the logistic dependence model (negative log) likelihood.
}
\usage{
logistic(w, p, ...)

logisticLH( w, p, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
	  numeric vector giving the angular component of the bivariate data.
}
  \item{p}{
	  single numeric giving the value of the logistic dependence model parameter.
}
  \item{\dots}{
	  Not used.
}
}
\details{
	This function is used by the \code{fbvpot} function to fit a bivariate POT model using the logistic dependence model.  The logistic dependence model has a single parameter, and is given by (Eq (8.11) in Coles 2001)

	0.5 * ( p^(-1) - 1 ) * ( ( w * (1 - w) )^(-1 - p^(-1) ) ) *
	            ( ( w^ (-1 / p) + ( 1 - w )^( -1 / p ) )^( p - 2 ) )

		    See Beirlant et al. (2004) for a thorough treatment of multivariate extreme-value analysis.
}
\value{
	logistic returns a vector giving the likelihood contribution for each angular component value and logisticLH calls logistic and returns the negative of the sum of the log of these values (i.e., the negative log-likelihood).  
}
\references{
	Beirlant, J., Y. Goegebeur, J. Segers, and J. Teugels (2004). \emph{Statistics of Extremes: Theory and Applications}.  Wiley, West Sussex, England, United Kingdom, 490 pp.

	Coles, S. G.  (2001). \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag, 208 pp.

}
\author{
	Eric Gilleland and Dan Cooley
}

\seealso{
\code{\link{fbvpot}}
}
\examples{
# See the help file for 'fbvpot' for an example (the logistic dependence model is the default).
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{ multivariate }% __ONLY ONE__ keyword per line
