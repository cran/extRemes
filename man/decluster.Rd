\name{decluster}
\alias{decluster}
\alias{decluster.data.frame}
\alias{decluster.default}
\alias{decluster.intervals}
\alias{decluster.runs}
\alias{plot.declustered}
\alias{print.declustered}
\title{
Decluster Data Above a Threshold
}
\description{
Decluster data above a given threshold to try to make them independent.
}
\usage{
decluster(x, threshold, ...)

\method{decluster}{data.frame}(x, threshold, ..., which.cols, method = c("runs", "intervals"), 
    clusterfun = "max")

\method{decluster}{default}(x, threshold, ..., method = c("runs", "intervals"),
    clusterfun = "max")

\method{decluster}{intervals}(x, threshold, ..., clusterfun = "max", groups = NULL, replace.with, 
    na.action = na.fail)

\method{decluster}{runs}(x, threshold, ..., data, r = 1, clusterfun = "max", groups = NULL, 
    replace.with, na.action = na.fail)

\method{plot}{declustered}(x, which.plot = c("scatter", "atdf"), qu = 0.85, xlab = NULL, 
    ylab = NULL, main = NULL, col = "gray", ...)

\method{print}{declustered}(x, ...)

}
\arguments{
  \item{x}{
An R data set to be declustered.  Can be a data frame or a numeric vector.  If a data frame, then \code{which.cols} must be specified.

\code{plot} and \code{print}: an object returned by \code{decluster}.
}
  \item{data}{A data frame containing the data.  }

  \item{threshold}{
numeric of length one or the size of the data over which (non-inclusive) data are to be declustered.
}
   \item{qu}{quantile for \code{u} argument in the call to \code{atdf}.}

   \item{which.cols}{numeric of length one or two.  The first component tells which column is the one to decluster, and the second component tells which, if any, column is to serve as groups.}

   \item{which.plot}{character string naming the type of plot to make.}

   \item{method}{character string naming the declustering method to employ.}

   \item{clusterfun}{character string naming a function to be applied to the clusters (the returned value is used).  Typically, for extreme value analysis (EVA), this will be the cluster maximum (default), but other options are ok as long as they return a single number.}

   \item{groups}{numeric of length \code{x} giving natural groupings that should be considered as separate clusters.  For example, suppose data cover only summer months across several years.  It would probably not make sense to decluster the data across years (i.e., a new cluster should be defined if they occur in different years).}

   \item{r}{integer run length stating how many threshold deficits should be used to define a new cluster.}

   \item{replace.with}{number, NaN, Inf, -Inf, or NA.  What should the remaining values in the cluster be replaced with?  The default replaces them with \code{threshold}, which for most EVA purposes is ideal.}

    \item{na.action}{function to be called to handle missing values.}

    \item{xlab, ylab, main, col}{optioal arguments to the \code{plot} function.  If not used, then reasonable default values are used.}

  \item{\dots}{
optional arguments to \code{decluster.runs} or \code{clusterfun}.

\code{plot}: optional arguments to \code{plot}.

Not used by \code{print}.
}
}
\details{
Runs declustering (see Coles, 2001 sec. 5.3.2): Extremes separated by fewer than \code{r} non-extremes belong to the same cluster.

Intervals declustering (Ferro and Segers, 2003): Extremes separated by fewer than \code{r} non-extremes belong to the same cluster, where \code{r} is the nc-th largest interexceedance time and nc, the number of clusters, is estimated from the extremal index, theta, and the times between extremes. Setting theta = 1 causes each extreme to form a separate cluster.

The print statement will report the resulting extremal index estimate based on either the runs or intervals estimate depending on the \code{method} argument as well as the number of clusters and run length.  For runs declustering, the run length is the same as the argument given by the user, and for intervals method, it is an estimated run length for the resulting declustered data.  Note that if the declustered data are independent, the extremal index should be close to one (if not equal to 1).
}
\value{
A numeric vector of class \dQuote{declustered} is returned with various attributes including:

  \item{call}{the function call.}
  \item{data.name}{character string giving the name of the data.}
  \item{decluster.function}{value of \code{clusterfun} argument.  This is a function.}
  \item{method}{character string naming the method.  Same as input argument.}
  \item{threshold}{threshold used for declustering.}
  \item{groups}{character string naming the data used for the groups when applicable.}
  \item{run.length}{the run length used (or estimated if \dQuote{intervals} method employed).}
  \item{na.action}{function used to handle missing values.  Same as input argument.}
  \item{clusters}{muneric giving the clusters of threshold exceedances.}
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Ferro, C. A. T. and Segers, J. (2003). Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{extremalindex}}, \code{\link{fevd}}
}
\examples{
y <- rnorm(100, mean=40, sd=20)
y <- apply(cbind(y[1:99], y[2:100]), 1, max)
bl <- rep(1:3, each=33)

ydc <- decluster(y, quantile(y, probs=c(0.75)), r=1, groups=bl)
ydc

plot(ydc)

\dontrun{
look <- decluster(-Tphap$MinT, threshold=-73)
look
plot(look)

# The code cannot currently grab data of the type of above.
# Better:
y <- -Tphap$MinT
look <- decluster(y, threshold=-73)
look
plot(look)

# Even better.  Use a non-constant threshold.
u <- -70 - 7 *(Tphap$Year - 48)/42
look <- decluster(y, threshold=u)
look
plot(look)

# Better still: account for the fact that there are huge
# gaps in data from one year to another.
bl <- Tphap$Year - 47
look <- decluster(y, threshold=u, groups=bl)
look
plot(look)


# Now try the above with intervals declustering and compare 
look2 <- decluster(y, threshold=u, method="intervals", groups=bl)
look2
dev.new()
plot(look2)
# Looks about the same,
# but note that the run length is estimated to be 5.
# Same resulting number of clusters, however.
# May result in different estimate of the extremal
# index.


#
fit <- fevd(look, threshold=u, type="GP", time.units="62/year")
fit
plot(fit)

# cf.
fit2 <- fevd(-MinT~1, Tphap, threshold=u, type="GP", time.units="62/year")
fit2
dev.new()
plot(fit2)

#
fit <- fevd(look, threshold=u, type="PP", time.units="62/year")
fit
plot(fit)

# cf.
fit2 <- fevd(-MinT~1, Tphap, threshold=u, type="PP", time.units="62/year")
fit2
dev.new()
plot(fit2)


}
}
\keyword{ manip }
\keyword{ data }
