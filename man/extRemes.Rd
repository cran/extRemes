\name{extRemes}
\alias{extRemes}
\title{extRemes -- The Extremes Toolkit: Weather and Climate Applications of Extreme-Value Statistics}
\description{
\pkg{extRemes} is a graphical user interface (GUI) for modeling extreme values with the purpose of providing
a pedagogical tool for scientists who are unfamiliar with extreme value theory or extreme value software.  It
is not designed to teach the R programming language; although, R command lines invoked by the GUI dialogs are
printed to a log file (extRemes.log) in the current working directory.  This package relies heavily on the
\pkg{ismev} package, and most of the underlying functions are from this package.  \pkg{ismev} is an R port of
Stuart Coles' S-Plus extreme value statistical routines.

For help using \pkg{extRemes}, please see the tutorial at:

\url{http://www.assessment.ucar.edu/toolkit}

Also, please consider registering at this site.  We need to determine if enough people are using the package
to continue with its maintenance and development.


Extreme Value Statistics:

Extreme value statistics are used primarily to quantify the stochastic behavior of a process at
unusually large (or small) values. Particularly, such analyses usually require estimation of the probability of events
that are more extreme than any previously observed. Many fields have begun to use extreme value theory and some have
been using it for a very long time including meteorology, hydrology, finance and ocean wave modeling to name just a few.

Example Datasets:

There are several example datasets included with this toolkit.  In each case, it is possible to load these datasets
into R using the \code{data} function.  Use the \code{as.extRemesDataObject} function to coerce these data to the form
that the GUI windows can recognize when loading data in this way.  Generally, it is possible to load all of these
datasets using the GUI windows (File --> Read Data --> browse to data and select --> choose R source --> OK).  The
example datasets will be in the data directory of the \pkg{extRemes} file structure.  It is not necessary to unzip
any files to do this; if you find yourself unzipping files, you have chosen the wrong file!  The example datasets
included with \pkg{extRemes} are:

Denmint	-- Denver daily minimum temperature.

Flood.dat -- U.S. Flood damage (in terms of monetary loss) ('dat' file used as example of reading in common data
			using the extRemes dialog).

ftcanmax -- Annual maximum precipitation amounts at one rain gauge in Fort Collins, Colorado.

HEAT -- Summer maximum (and minimum) temperature at Phoenix Sky Harbor airport.

Ozone4H.dat -- Ground-level ozone order statistics from 1997 from 513 monitoring stations in the eastern United States.

PORTw -- Maximum and minimum temperature data (and some covariates) for Port Jervis, New York.

Rsum -- Frequency of Hurricanes.

SEPTsp -- Maximum and minimum temperature data (and some covariates) for Sept-Iles, Quebec.

damage -- Hurricane monetary damage.

Denversp -- Denver precipitation.

Flood -- R source version of the above mentioned 'Flood.dat' dataset.

FtCoPrec -- Precipitation amounts at one rain gauge in Fort Collins, Colorado.

Peak -- Salt River peak stream flow.

Potomac -- Potomac River peak stream flow.

Tphap -- Daily maximum and minimum temperatures at Phoenix Sky Harbor Airport.

For more information on any of these datasets, type

\code{help( [name_of_dataset])}

from the R prompt.

Primary Functions for extRemes:

As mentioned above, \pkg{extRemes} is primarily a pedagogical tool that provides a GUI interface to the \pkg{ismev}
package.  Listed below is the function to invoke the main toolkit dialog window, followed by a few underlying functions
provided with \pkg{extRemes} that are not provided with \pkg{ismev}.

\code{extremes.gui}:

The main toolkit dialog window should appear upon loading extRemes with \code{library(extRemes)}.  If this window
is closed while the package is still loaded, use \code{extremes.gui()} from the R command prompt to re-open the main
dialog window.

-------------------------
The following is targeted at the more advanced R user.  It is hoped that a person unfamiliar with R can use this toolkit
without learning more than a very few R commands; and the tutorial at the above website should be sufficient to get started
for such a user.

Extremes Data Object:

For ease of operation, it is convenient to have a special class of objects for the GUI windows to operate on.  This
class, \code{extRemesDataObject}, might be considered an orphaned object in R lingo because it does not have the
usual method functions associated with it--they generally do not make sense for this object.  The object simply stores
information so that the GUI windows know which elements of the object to use in particular situations.  It would not,
for example, make sense to do \code{plot( tmp)} (where \code{tmp} is an object of class \code{extRemesDataObject}
because there are numerous possible plots depending on what is included in the object; it would be tantamount to
plotting the entire workspace (in some cases).  The \code{extRemesDataObject} is a list object with, at the very
least, a \code{data} component that includes either a data frame or matrix.  Often it will also have \code{name} and
\code{file.path} components (whenever data is loaded via the GUI windows) which give the name of the original file,
which can be different from the name of the \code{extRemesDataObject} list object, and the path to the original file.

-----------------------

The Extremes Toolkit is funded by the National Science Foundation (NSF) through the NCAR Weather and Climate Impact
Assessment Science Initiative with additional support from the NCAR Geophysical Statistics Project (GSP).
}
\keyword{misc}
