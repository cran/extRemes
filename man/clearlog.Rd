\name{clearlog}
\alias{clearlog}
\title{ Clear File extRemes.log }
\description{
Overwrites the file extRemes.log with the date and time that the file is cleared.
}
\usage{
clearlog(base.txt)
}
\arguments{
  \item{base.txt}{ Unused argument for compatibility with GUI windows. }
}
\details{
As the GUI windows are used, the major commands invoked by the GUI windows are written to the
file extRemes.log, and this file can get large with use.  This function is invoked when
'Clear log file' is selected from the File menu of the main toolkit dialog.

The file is cleared, but still exists with a message indicating the time the file was last
cleared.  If no extRemes.log file exists, then one is created with the date and time message.
}
\value{
No value returned.  Has the effect of erasing any information contained in extRemes.log, or
of creating this file if it does not exist; and adds the date and time when the file was
cleared/created.
}
\author{ Eric Gilleland }

\keyword{ misc }
