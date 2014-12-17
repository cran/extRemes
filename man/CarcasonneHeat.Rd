\name{CarcasonneHeat}
\alias{CarcasonneHeat}
\docType{data}
\title{
European Climate Assessment and Dataset
}
\description{
Blended temperature multiplied by ten (deg Celsius) series of station STAID: 766 in Carcasonne, France.
}
\usage{data("CarcasonneHeat")}
\format{
  The format is:
 int [1:4, 1:12054] 104888 19800101 96 0 104888 19800102 57 0 104888 19800103 ...
}
\details{
European Climate Assessment and Dataset blended temperature (deg Celsius) series of station STAID: 766 in Carcasonne, France.  Blended and updated with sources: 104888 907635.  See Klein Tank et al. (2002) for more information.

This index was developed by Simone Russo at the European Commission, Joint Research Centre (JRC).
Reports, articles, papers, scientific and non-scientific works of any form, including tables, maps, or any other kind of output, in printed or electronic form, based in whole or in part on the data supplied, must reference to Russo et al. (2014).
}

\author{Simone Russo <simone.russo@jrc.ec.europa.eu>}

\source{
We acknowledge the data providers in the ECA&D project.

Klein Tank, A.M.G. and Coauthors, 2002. Daily dataset of 20th-century surface air
temperature and precipitation series for the European Climate Assessment. Int. J. of Climatol.,
22, 1441-1453.

Data and metadata available at
\url{http://www.ecad.eu}
}

\references{
Russo, S. and Coauthors, 2014. Magnitude of extreme heat waves in present climate and their projection in a warming world. \emph{J. Geophys. Res.}, doi:10.1002/2014JD022098.
}

\examples{
data(CarcasonneHeat)
str(CarcasonneHeat)

# see help file for hwmi for an example using these data.
}
\keyword{datasets}


