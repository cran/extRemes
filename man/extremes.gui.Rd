\name{extremes.gui}
\alias{extremes.gui}
\title{ open the toolkit main dialog window }
\description{
Simply opens the toolkit dialog window.
}
\usage{
extremes.gui()
}
\value{
Opens a Tcl/Tk dialog window.
}
\author{ Greg Young and Eric Gilleland }
\note{ This function is called on start-up (i.e., when library( extRemes) is invoked).
This function may be used to re-open the main dialog window if it is closed while the
package is still loaded. }
\references{
Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for
     the 'Extremes Toolkit: Weather and Climate Applications of Extreme
     Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.
}
\examples{
## Don't run
# extremes.gui()
}
\keyword{ misc }% at least one, from doc/KEYWORDS
