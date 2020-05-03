\name{hwmi}
\alias{hwmi}
\title{Heat Wave Magnitude Index}

\description{
This function computes the Heat Wave Magnitude Index and the associated duration and starting date of a heat wave event.
}

%% Note: Need to update the usage statement to reflect additional arguments.  Must be exactly the same as in the code.
%% You can use args(hwmi) and copy and paste what it gives you.
\usage{hwmi(yTref, Tref, yTemp, Temp)}

\arguments{

  \item{yTref}{a single numeric value giving the starting year of Tref}
  \item{Tref}{a numeric vector of daily maximum temperatures for a 32-year reference period used to calculate threshold and empirical cumulative distribution function (ecdf).}
\item{yTemp}{a single numeric value giving the starting year of Temp}
  \item{Temp}{a numeric vector of daily maximum temperature for at least one year (with length not shorter than 365 days)  or n-years (each with length not shorter than 365 days) containing the data to which the HWMI is to be calculated.}

}

\details{
This function takes a daily maximum temperature time series as input and computes the climate index HWMI (Heat Wave Magnitude Index).  The Heat Wave Magnitude Index is defined as the maximum magnitude of the heat waves in a year. A \dQuote{heat wave} is defined as a sequence of 3 or more days in which the daily maximum temperature is above the 90th percentile of daily maximum temperature for a 31-day running window surrounding this day during the baseline period (e.g., 1981-2010).

Note that the argument \code{Tref} must have daily maximum temperatures for a 32-year period (e.g., using the baseline period of 1981 to 2010, \code{Tref} must be for 1980 through 2011).  The first and the 32nd years are needed to calculate the daily threshold over the first and the last year of the baseline period (1981-2010).

If HWMI is calculated in the Southern Hemisphere, then, in order not to split a Heat Wave event into two, the year should start on the 1st of July and end on the 30th of June of the following year.

In other words:

1. Tref will be from the 1st of July 1980 up to the 30th of June 2012

2. Similarly for Temp, each 365 days in a year must be taken between the 1st of July and the 30th of June.
}

\value{
A list with the following components: 
hwmi: a numeric vector containing the hwmi value for each year and the associated duration and starting day (number between 1 and 365) for each heat wave event in a year.
thr: a numeric vector containing 365 temperature values representing the daily threshold for the reference period (1981-2010 or any other 30 years period).
pdfx: the "n" (n was fixed to 512) coordinates of the points (sum of three daily maximum temperatures) where the density is estimated.
pdfy: the estimated density values.  These will be non-negative, but can be zero.
}

\author{Simone Russo <simone.russo@jrc.ec.europa.eu>}

\references{
Russo, S. and Coauthors, 2014. Magnitude of extreme heat waves in present climate and their projection in a warming world. \emph{J. Geophys. Res.}, doi:10.1002/2014JD022098.

\url{http://www.nature.com/news/russian-summer-tops-universal-heatwave-index-1.16250}
}
\examples{

data("CarcasonneHeat")

tiid <- CarcasonneHeat[2,]

jan1980 <- which(tiid == 19800101)
jan2003 <- which(tiid == 20030101)
dec2003 <- which(tiid == 20031231)
dec2011 <- which(tiid == 20111231)

Temp <- CarcasonneHeat[3, jan2003:dec2003] / 10
Tref <- CarcasonneHeat[3, jan1980:dec2011] / 10

##hwmi calculation
hwmiFr2003 <- hwmi(1980, Tref, 2003, Temp)

#### Heat Wave occurred in Carcassonne, France, 2003

plot(c(150:270), Temp[150:270], xlim = c(150, 270),
    ylim = c((min(hwmiFr2003$thr[150:270]) - 
        sd(hwmiFr2003$thr[150:270])), max(Temp[150:270])),
    xlab = "days", ylab = "temperature", col = 8)

par(new = TRUE)
plot(c(150:270), hwmiFr2003$thr[150:270], type = "l",
    xlim = c(150,270),
    ylim = c((min(hwmiFr2003$thr[150:270]) - sd(hwmiFr2003$thr[150:270])),
    max(Temp)), xlab = "", ylab = "", col = 1, lwd = 2)

par(new = TRUE)
plot(c(hwmiFr2003$hwmi[1,3]:(hwmiFr2003$hwmi[1,3] + hwmiFr2003$hwmi[1,2]-1)),
    Temp[hwmiFr2003$hwmi[1,3]:(hwmiFr2003$hwmi[1,3] + hwmiFr2003$hwmi[1,2] - 1)],
    xlim = c(150,270),
    ylim = c((min(hwmiFr2003$thr[150:270]) - sd(hwmiFr2003$thr[150:270])),
        max(Temp[150:270])),
    xlab = "", ylab = "", col = 4, type = "b",
    main = "Carcassonne, France, 2003", lwd = 2)

text(175, 42, "hwmi = 3.68", col = 4, font = 2)
text(175, 41, "Duration = 12 days", col = 4, font = 2)
text(175, 40, "Starting day = 214 (02.Aug.2003)", col = 4, font = 2)


}

\keyword{methods}
