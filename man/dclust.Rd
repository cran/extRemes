\name{dclust}
\alias{dclust}
\title{ Decluster Data by Runs Declustering }
\description{
Decluster data by assuming that exceedances belong to the same cluster if
they are separated by fewer than 'r' (run length) values below a given
threshold.
}
\usage{
dclust(xdat, u, r, cluster.by = NULL, verbose=getOption("verbose"))
}
\arguments{
  \item{xdat}{ a single numeric vector of data to be declustered.}
  \item{u}{ single number or vector of thresholds. }
  \item{r}{ run length }
  \item{cluster.by}{ If there are blocks implying a natural clustering that is to be preserved
	(e.g., if data cover several years, but only for a single season), this is a vector
	defining the blocks to ensure that clusters do not cross over from one block to another.}
  \item{verbose}{ logical whether to field progress information to screen or not.}
}
\details{
This function applies runs declustering to automatically decluster a dataset.
To ensure that clusters do not cross natural or decided boundaries, use
the \code{cluster.by} option.  That is, suppose data are measured only in the summer,
say from June 1 through August 1.  In such a case, it is perhaps not desired to have
a value from August 1, 2003 and June 1, 2004 in the same cluster.  To account for this,
create a \code{cluster.by} vector defining years in order to keep clusters within years.
For the example of data from June 1 to August 1 (62 days), a vector like
c(rep(1, 62), rep(2, 62), ..., rep(n, 62)) should be used for the \code{cluster.by} argument.

This function will return a vector of the same length as the original data
vector, but with maximums from each cluster followed by \dQuote{filler} numbers that
are below the given threshold, \code{u}.

Missing values are not handled.  The function will still run, but the results
will be questionable.
}
\value{ A list with components:
  \item{xdat.dc}{Maximums from each cluster with additional filler values below the given threshold u in order to maintain the same length as the original data vector xdat.  This is for compatability with extRemes GUI data object of class \dQuote{ev.data}.}
  \item{ncluster}{The number of clusters found by runs declustering.}
  \item{clust}{numeric vector giving the clusters.}
}
\references{
Coles, S. (2001)  \emph{An Introduction to Statistical Modeling of Extreme Values}.  London: Springer-Verlag, 208pp.
}
\author{ Eric Gilleland }

\examples{
# Load a dataset.
data(Tphap)

plot( Tphap[,"MaxT"])
abline( h=115)

# Decluster using a threshold of 115 degrees and a run length of 'r=1'.
temp <- dclust(xdat=Tphap[,"MaxT"], u=115, r=1, cluster.by = Tphap[,"Year"])
temp[["ncluster"]] # See how many clusters were found.

# Now do the same as above, but with a run length of 3 for comparison.
# Note: 'r=2' gives same clusters as 'r=1' for these data.
temp2 <- dclust(xdat=Tphap[,"MaxT"], u=115, r=3, cluster.by = Tphap[,"Year"])
temp2[["ncluster"]]

}
\keyword{ manip}
