write.extRemesMainMessage <- function(txt) {
tkconfigure( txt, state="normal")
   nl <- paste(" ", " ", sep="\n")
   msg01 <- paste("The Extremes Toolkit (extRemes): ",
			"Weather and Climate Applications of Extreme-Value Statistics", " ", " ", sep="\n")
   msg02 <- paste("Type \'help( extRemes)\' for more information, and",
			"to get started, please see the tutorial at:", " ", sep="\n")
   msg03 <- paste("http://www.assessment.ucar.edu/toolkit/", sep="")
   tkinsert( txt, "end", nl)
   tkinsert( txt, "end", msg01)
   tkinsert( txt, "end", nl)
   tkinsert( txt, "end", msg02)
   tkinsert( txt, "end", nl)
   tkinsert( txt, "end", msg03)
tkconfigure(txt, state = "disabled")
invisible()
}
