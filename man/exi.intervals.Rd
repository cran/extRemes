\name{exi.intervals}
\alias{exi.intervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extremal index estimator}
\description{
Evaluates the intervals estimator for the extremal index.
}
\usage{
exi.intervals(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ Logical vector indicating which positions correspond to extreme values. }
}
\value{
Estimate of the extremal index.
}
\references{ Ferro CAT and Segers J (2003) Inference for clusters of extreme
  values. Journal of the Royal Statistical Society B 65, 545-556. }
\author{ Chris Ferro}
\note{ Maintained by Eric Gilleland.}

\section{Warning }{The estimator is not constrained to lie in [0,1] and a default value of 1 is
returned if there are fewer than two extreme values.}

\seealso{ \code{\link{decluster.intervals}}}
\examples{
x <- rnorm(1000)
exi.intervals(x > quantile(x, 0.9))
}
\keyword{ misc }% at least one, from doc/KEYWORDS
