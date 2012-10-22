\name{taildep.test}
\alias{taildep.test}
\alias{relative.rank}
\title{
Tail Dependence Test
}
\description{
Testing tail dependence against tail independence.
}
\usage{
taildep.test(x, y, cthresh = -0.5, trans = "relative.rank", na.action = na.fail, ...)

relative.rank(x, div = "n", ...)
}
\arguments{
  \item{x,y}{
numeric vectors of same length.  For \code{taildep.test}, \code{x} may be a two-column matrix or data frame, in which case each column is assumed to be the two vectors of interest.
}
  \item{cthresh}{
single numeric between -1 and 0 (non-inclusive) over which the transformed and shifted x + y variable is tested (see Details).
}
  \item{trans}{
character string naming a function to transform the x and y variables so that they are in the lower left quadrant (see Details).  If variables are already transformed as such (or it is not necessary), then use \dQuote{identity}.
}
  \item{div}{character one of \dQuote{n} or \dQuote{n+1} stating whether to divide the ranks by n or n + 1 so that the reslting transformations are in [0,1] or (0,1), resp.}
  \item{na.action}{function to be called to handle missing values.}
  \item{\dots}{
optional arguments to the \code{trans} function.  In the case of \code{relative.rank} these are optional arguments to the function \code{rank}.
}
}
\details{
This is the tail dependence test described in Reiss and Thomas (2007) section 13.3.  It is, unusually, a test whose null hypothesis is that the two random variables, X and Y, are dependent.  So, for example, if a significance level alpha = 0.01 test is desired, then the null huypothesis (dependence) is accepted for values of the statistic with p-values less than 0.01.

To do the test, the variables must first be transformed to the left lower quadrant.  Following Reiss and Thomas (2007), the default is to transform the data by means of the sample distribution functions (df's), u = Fhat_n(x) and v = Fhat_n(y) (i.e., using the function \code{relative.rank}).  This yields random variables between 0 and 1, and subsequently they are shifted to be between -1 and 0 (this is done by \code{taildep.test} so should not be done by the \code{trans} function).

Ultimately, the test statistic is given by

    -(sum(log(c.tilde) + m)/sqrt(m)),

where c.tilde = (u + v)*1(u+v > c)/c, for c a threshold (i.e., \code{cthresh}).  The statistic is assumed to be N(0,1), and the p-value is calculated accordingly.

The test is somewhat sensitive to the choice of threshold, \code{cthresh}, and it is probably a good idea to try several values (approaching zero from the left).  Ideally, the threshold should yield about 10 - 15 percent excesses.
}
\value{
A list object of class \dQuote{htest} is returned with components:
  \item{call }{the calling string}
  \item{data.name }{character vector giving the names of the data sets employed (if x is a matrix, then the second component will be \dQuote{ }.}
  \item{method}{character string, which will always be \dQuote{Reiss-Thomas (13.35)}.}
  \item{transformation}{same as trans argument.}
  \item{parameter}{named vector giving the value of the threshold and any arguments passed to the trans function (perhaps this is not a good idea, and may be changed eventually).}
  \item{c.full}{value of the vector u + v after having been transformed and shifted to be between -1 and 0.  This is so that the user can adjust the threshold so that 10 - 15 percent of the values exceed it.}
  \item{statistic}{numeric giving the value of the test statistic.}
  \item{alternative}{character string stating \dQuote{greater}.}
  \item{p.value}{numeric between 0 and 1 giving the p-value for the test.}
}
\references{
Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkh\"{a}user, 530pp., 3rd edition.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{taildep}}, \code{\link{atdf}}, \code{\link{lr.test}}
}
\examples{
x <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
               sd = sqrt(0.1796))

y <- x + rnorm(63)

taildep.test(x, y)

# Recall that null hypothesis is tail dependence!

\dontrun{
data(PORTw)
taildep.test(PORTw$TMX1, PORTw$TMN0, cthresh=-0.3)

data(FCwx)
taildep.test(FCwx$MxT, FCwx$Prec, cthresh=-0.4)

# Run the example (13.3.6) in Reiss and Thomas (2007)
# using the 'wavesurge' dataset from package 'ismev'.
data(wavesurge)
cth <- seq(-0.46,-0.35,0.01)
tab13.1 <- matrix(NA, 2, 12)
colnames(tab13.1) <- as.character(cth)
for(i in 1:12) {
    tmp <- taildep.test(wavesurge, cthresh=cth[i], ties.method="max")
    tab13.1[1,i] <- tmp$parameter["m"]
    tab13.1[2,i] <- tmp$p.value
} # end of for 'i' loop.

rownames(tab13.1) <- c("m", "p-value")
tab13.1
}

}
\keyword{ htest }
\keyword{ multivariate }
