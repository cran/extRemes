\name{xtibber}
\alias{xtibber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Test-Inversion Bootstrap for Extreme-Value Analysis
}
\description{
	Test-inversion bootstrap (TIB) for fevd class objects.
}
\usage{
xtibber(x, type = c("return.level", "parameter"), which.one,
	tib.method = c("interp", "rm"), nuisance = "shape", B,
	test.pars, rsize, block.length = 1, shuffle = NULL,
	replace = TRUE, alpha = 0.05, qcov = NULL,
	qcov.base = NULL, stud = FALSE, step.size, tol = 1e-04,
	max.iter = 1000, keep.iters = TRUE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  List object of class \dQuote{fevd}.
}
  \item{type}{
	  character string stating whether to calculate TIB intervals for a return level or a parameter as this funciton will only calculate an interval for a single parameter/return level at a time.
}
  \item{which.one}{
	  number or character stating which return level or which parameter to find CIs for.
}
  \item{tib.method}{
	  character stating whether to estimate the TIB interval by interpolating from a series of pre-determined values of the nuisance parameter or to use the Robbins-Monroe (RM) method.  See the help file for \code{tibber} from the distillery package for more information.
}
  \item{nuisance}{
	  character naming the nuisance parameter.
}
  \item{B, rsize, block.length, shuffle, replace}{
	  See the help file for \code{booter} from the distillery package for more information on these arguments.
}
  \item{test.pars}{
	  numeric vector giving the sequence of nuisance parameter values for the interpolation method, or a numeric vector of length two giving the starting values for the RM method.
}
  \item{alpha}{
	  numeric between zero and one giving the desired confidence level.
}
\item{qcov}{numeric matrix with rows the same length as \code{q} and columns equal to the number of parameters (+ 1 for the threshold, if a POT model).  This gives any covariate values for a nonstationary model.  If NULL, and model is non-stationary, only the intercept terms for modeled parameters are used, and if a non-constant threshold, only the first threshold value is used.  Not used if model is stationary. }
   \item{qcov.base}{numeric matrix analogous to \code{qcov}.  When provided, the function returns the difference in return levels between the level for the covariates in \code{qcov} and the level for covariates in \code{qcov.base}.}
  \item{stud}{
	  logical if TRUE will calculate Studentized intervals (generally not profitable with the TIB method).
}
  \item{step.size}{
	  Used with the RM method only.  Numeric giving the size of increments to use in the root-finding algorithm.
}
  \item{tol}{
	  Used with the RM method only.  Numeric stating how close to the desired level of confidence is satisfactory.
}
  \item{max.iter}{
	  numeric giving the maximum number of iterations for the root-finding algorithm before giving up.
}
  \item{keep.iters}{
	  logical, should all of the values in the root-finding search be kept?  Needed if a plot will be made.
}
  \item{verbose}{
	  logical, if TRUE will print progress information to the screen.
}
  \item{\dots}{
	  optional arguments to \code{nlminb}.
}
}
\details{
	This function provides a wrapper to the \code{tibber} function from distillery for \dQuote{fevd} objects.
}
\value{
	See the help file for tibber for more information on the value
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}, \code{\link{tibber}}, \code{\link{booter}}
}
\examples{
\dontrun{
data("ftcanmax")
fit <- fevd( Prec, data = ftcanmax )

tbfit <- xtibber( fit, which.one = 100, B = 500,
		 test.pars = seq(-0.01,0.2,,100), verbose = TRUE )

tbfit

plot( tbfit )

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")
