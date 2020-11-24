\name{mixbeta}
\alias{mixbeta}
\alias{mixbetaLH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Mixed Beta Dependence Model Likelihood
}
\description{
	Calculate the mixed beta dependence model likelihood in conjunction with fbvpot.
}
\usage{
mixbeta(w, p, ...)

mixbetaLH(w, p, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
	  numeric vector giving the angular component of the bivariate data.
}
  \item{p}{
	  numeric of length 4 giving the values of the logistic dependence model parameter.
}
  \item{\dots}{
	  Not used.
}
}
\details{
	The mixed beta dependence model is given by 

	p1 * dbeta( w, shape1 = nu1 * pi1, shape2 = nu1 * ( 1 - pi1 ) ) +
		( 1 - p1 ) * dbeta( w, shape1 = nu2 * pi2, shape2 = nu2 * ( 1 - pi2 ) )

	where nu1 = p[ 1 ], nu2 = p[ 2 ], pi1 = p[ 3 ], p1 = p[ 4 ], and pi2 = ( 1/2 - p[ 4 ] * p[ 3 ] ) / ( 1 - p[ 4 ] ).  See Beirlant et al. (2004) for a thorough treatment of multivariate extreme-value analysis.

	

}
\value{
	mixbeta returns a vector giving the likelihood contribution for each angular component value and mixbetaLH calls mixbeta and returns the negative of the sum of the log of these values (i.e., the negative log-likelihood).
}
\references{
	Beirlant, J., Y. Goegebeur, J. Segers, and J. Teugels (2004). \emph{Statistics of Extremes: Theory and Applications}.  Wiley, West Sussex, England, United Kingdom, 490 pp.
}
\author{
	Eric Gilleland and Dan Cooley
}

\seealso{
\code{\link{fbvpot}}
}
\examples{

data( "SantaAna" )

Z <- SantaAna[,3:4]

mfit1 <- fevd( x = temp, data = Z, threshold = 36.75, type = "GP" )
mfit2 <- fevd( x = windspeeds, data = Z, threshold = 7.09875, type = "GP" )

fit2 <- fbvpot( x = Z, threshold = apply( Z, 2, quantile, probs = 0.95 ),
	       dep.model = "mixbeta", init = c( 1, 2, 0.5, 0.5 ),
	       tform = "tf", fit = list( mfit1, mfit2 ) )
fit2

plot( fit2 )

hist( fit2 )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{ multivariate }% __ONLY ONE__ keyword per line
