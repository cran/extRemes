\name{dclust}
\alias{dclust}
\title{ Decluster data by runs declustering. }
\description{
Decluster data by assuming that exceedances belong to the same cluster if
they are separated by fewer than 'r' (run length) values below a given
threshold.
}
\usage{
dclust(xdat, u, r, cluster.by = NULL)
}
\arguments{
  \item{xdat}{ a single numeric vector of data to be declustered.}
  \item{u}{ single number or vector of thresholds. }
  \item{r}{ run length }
  \item{cluster.by}{ If there is a natural clustering that is to be preserved,
	such as if data is for one season over several years, this is a vector
	ensuring that clusters do not cross over from one year to another.}
}
\details{
This function applies runs declustering to automatically decluster a dataset.
To ensure that that clusters do not cross natural or decided boundaries, use
the 'cluster.by' option.  That is, suppose data represents a particular season,
say summer, and clusters are to not cross over from one year of data to another.
Use a vector that defines the year in order to keep clusters within years.

This function will return a vector of the same length as the original data
vector, but with maximums from each cluster followed by 'filler' numbers that
are below the given threshold, 'u'.

Missing values are not handled.  The function will still run, but the results
will be questionable.
}
\value{
  \item{xdat.dc}{Maximums from each cluster with additional filler values below
	the given threshold 'u' in order to maintain the same length as the
	original data vector 'xdat'.  This is for compatability with extRemes
	GUI data object of class ``ev.data".  Missing values not allowed!  It
	will still run, but the results may be questionable.}
  \item{ncluster}{The number of clusters found by runs declustering.}
  \item{clust}{numeric vector giving the clusters.}
}
\references{ Coles, Stuart (2001).  An Introduction to Statistical Modeling of
	Extreme Values.  Springer-Verlag, London. }
\author{ Eric Gilleland }

\examples{
# Load a dataset.
data(Tphap)

# Decluster using a threshold of 115 degrees and a run length of 'r=1'.
temp <- dclust(xdat=Tphap[,"MaxT"], u=115, r=1, cluster.by = Tphap[,"Year"])
# plot it to get an idea of where the clusters are.
plot( Tphap[,"MaxT"])
abline( v=temp[["clust"]], lty=2, col="red")
abline( h=115)
# See how many clusters were found.
temp[["ncluster"]]

# Now do the same as above, but with a run length of 3 for comparison.
# Note: 'r=2' gives same clusters as 'r=1' for these data.
temp2 <- dclust(xdat=Tphap[,"MaxT"], u=115, r=3, cluster.by = Tphap[,"Year"])
plot( Tphap[,"MaxT"])
abline( v=temp2[["clust"]], lty=2, col="red")
abline( h=115)
temp2[["ncluster"]]

}
\keyword{ manip}
