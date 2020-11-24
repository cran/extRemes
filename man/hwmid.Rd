\name{hwmid}
\alias{hwmid}
\title{Heat Wave Magnitude Index}

\description{
This function computes the Heat Wave Magnitude Index and the associated duration and starting date of a heat wave event.
}

%% Note: Need to update the usage statement to reflect additional arguments.  Must be exactly the same as in the code.
%% You can use args(hwmid) and copy and paste what it gives you.
\usage{hwmid(yTref, Tref, yTemp, Temp)}

\arguments{

  \item{yTref}{a single numeric value giving the starting year of Tref}
  \item{Tref}{a numeric vector of daily maximum temperatures for a 32-year reference period used to calculate threshold, T30ymax and T30ymin as defined below.}
\item{yTemp}{a single numeric value giving the starting year of Temp}
  \item{Temp}{a numeric vector of daily maximum (minimum or mean) temperature for at least one year (with length not shorter than 365 days)  or n-years (each with length not shorter than 365 days) containing the data to which the HWMId is to be calculated.}

}

\details{
This function takes a daily temperature time series as input and computes the climate index HWMId (Heat Wave Magnitude Index daily).  The Heat Wave Magnitude Index daily is defined as the maximum magnitude of the heat waves in a year. A \dQuote{heat wave} is defined as a sequence of 3 or more days in which the daily maximum temperature is above the 90th percentile of daily maximum temperature for a 31-day running window surrounding this day during the baseline period (e.g., 1981-2010).

Note that the argument \code{Tref} must have daily temperatures for a 32-year period (e.g., using the baseline period of 1981 to 2010, \code{Tref} must be for 1980 through 2011).  The first and the 32nd years are needed to calculate the daily threshold over the first and the last year of the baseline period (1981-2010).
}

\value{
A list with the following components: 
hwmid: a numeric vector containing the hwmid value for each year and the associated duration and starting day (number between 1 and 365) for each heat wave event in a year.
thr: a numeric vector containing 365 temperature values representing the daily threshold for the reference period (1981-2010 or any other 30 years period).
T30y75p: a single numeric value giving the 75th percentile value of the time series calculated from Tref and composed of 30-year annual maximum temperatures of the baseline period.
T30y25p: a single numeric value giving the 25th percentile value of the time series calculated from Tref and composed of 30-year annual maximum temperatures of the baseline period.
}

\author{Simone Russo <simone.russo@jrc.ec.europa.eu, simone.russo@isprambiente.it>}

\references{

Russo, S., J. Sillmann, E. Fischer, 2015. Top ten European heatwaves since 1950 and their occurrence in the coming decades. \emph{Environmental Research Letters}, \bold{10}, 124003, doi:10.1088/1748-9326/10/12/124003.

Russo, S. and Coauthors, 2014. Magnitude of extreme heat waves in present climate and their projection in a warming world. \emph{J. Geophys. Res.}, doi:10.1002/2014JD022098.

\url{https://www.nature.com/news/russian-summer-tops-universal-heatwave-index-1.16250}
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


##hwmid calculation
hwmidFr2003 <- hwmid(1980, Tref, 2003, Temp)
hwmiFr2003 <- hwmi(1980, Tref, 2003, Temp)

T30y25p <- hwmidFr2003$T30y25p
T30y75p <- hwmidFr2003$T30y75p
range30y <- (T30y75p - T30y25p)

#daymag<-(Temp[214:225]-hwmidFr2003$T30ymin)/(hwmidFr2003$T30ymax-hwmidFr2003$T30ymin)
#### Heat Wave occurred in Carcassonne, France, 2003

split.screen( rbind( c(0, 1, 0.6, 1), c(0, 0.5, 0, 0.6), c(0.5, 1, 0, 0.6) ) )
screen(1)
par( mar = c(2, 2, 2, 0) )
plot( c(1:365), Temp[1:365], xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", cex.axis = 1.1, col = 8, font.axis = 2)

par( new = TRUE )
plot( c(150:270), hwmiFr2003$thr[150:270], type = "l",xlim = c(190, 240),
    ylim = c(25, 50), xlab = "", ylab = "", col = 1, lwd = 2, axes = FALSE)

par(new = TRUE)


plot(c(214:216), Temp[214:216], xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 4, type = "b", lwd = 2, axes = FALSE,
    pch = "a", cex = 1.2)

par( new = TRUE)
plot(c(217:219), Temp[217:219], xlim = c(190, 240), ylim = c(25,50),
    xlab = "", ylab = "", col = 4, type = "b", lwd = 2, axes = FALSE,
    pch = "b", cex = 1.2)

par(new=TRUE)
plot(c(220:222), Temp[220:222], xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 4, type = "b", lwd = 2, axes = FALSE,
    pch = "c", cex = 1.2)

par(new=TRUE)
plot(c(223:225), Temp[223:225], xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 4, type = "b", lwd = 2, axes = FALSE,
    pch = "d", cex = 1.2)


par(new=TRUE)
plot(c(214:216), (Temp[214:216]+5), xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 3, type = "b", lwd = 2, axes = FALSE,
    pch = "a", cex = 1.2)

par(new=TRUE)
plot(c(217:219), (Temp[217:219]+5), xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 3, type = "b", lwd = 2, axes = FALSE,
    pch = "b", cex = 1.2)

par(new=TRUE)
plot(c(220:222), (Temp[220:222]+5), xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 3, type = "b", lwd = 2, axes = FALSE,
    pch = "c", cex = 1.2)

par(new=TRUE)
plot(c(223:225), (Temp[223:225]+5), xlim = c(190, 240), ylim = c(25, 50),
    xlab = "", ylab = "", col = 3, type = "b", lwd = 2, axes = FALSE,
    pch = "d", cex = 1.2)

text(200, 50, "HW2", col = 3, font = 2)
text(200, 48, "HWMI=4", col = 3, font = 2)
text(200, 46, "HWMId = 41.9",col = 3, font = 2)

text(200, 42, "HW1", col = 4, font = 2)
text(200, 40, "HWMI = 3.68", col = 4, font = 2)
text(200, 38, "HWMId=18.6",col = 4, font = 2)


box()
}

\keyword{methods}
