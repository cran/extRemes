logtrans.gui <- function(base.txt) {

# This function provides a gui interface for finding the logarithmic
# transformation of a dataset.  The gui will list all objects of class
# "extRemesDataObject" and the column names of the user selected object.
# After taking the log of the selected data, will return a new column with
# the same column name(s), but with a ".lt" extension (for base e) and a
# ".Blt" where 'B' is the user given base other than 'exp' if a base other
# than 'e' is chosen.

# Set the tcl variables

ebase <- tclVar(1)
bb <- tclVar( 10)

# Internal functions

refresh <- function() {

# When data is selected, this function fills the lists for the columns of
# the data set so that the user can select which column(s) to transform.

	tkdelete( col.listbox, 0.0, "end")

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else dd <- extRemesData

	for( i in 1:ncol( dd$data))
		tkinsert( col.listbox, "end",
			paste( colnames( dd$data)[i]))
	invisible()
	} # end of refresh fcn

log.trans <- function() {
# Function invoked when the "ok" button is pressed.  Actually takes log
# transformation of the data.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd.cmd <- paste( "dd <- get( \"", full.list[ data.select], "\")", sep="")
	} else dd.cmd <- "dd <- extRemesData"
	eval( parse( text=dd.cmd))
	write( dd.cmd, file="extRemes.log", append=TRUE)

if( tclvalue(ebase) == 1 ) log.b <- exp(1)
else {
	log.b <- as.numeric( tclvalue( bb))
	bbext <- deparse( log.b)
	}

cols.selected.cmd <- "cols.selected <- character(0)"
eval( parse( text=cols.selected.cmd))
write( cols.selected.cmd, file="extRemes.log", append=TRUE)

# Gather column names to add log transform(s) to end.
# cnames <- colnames( dd$data)
# cnamesCMD <- paste( "cnames <- colnames( ", full.list[ data.select], "$data)", sep="")
cnamesCMD <- "cnames <- colnames( dd[[\"data\"]])"
eval( parse( text=cnamesCMD))
write( cnamesCMD, file="extRemes.log", append=TRUE)

temp <- as.numeric( tkcurselection( col.listbox)) + 1

# Make sure a column has been selected.
if( is.na( temp)) return()

# cols.selected <- cnames[temp]
for( i in 1:length( temp)) {
	cols.selected.cmd <- paste( "cols.selected <- c( cols.selected, \"", cnames[temp[i]], "\")", sep="")
	eval( parse( text=cols.selected.cmd))
	write( cols.selected.cmd, file="extRemes.log", append=TRUE)
	} # end of for 'i' loop.

# dd$data[ ,cols.selected] <-
# lt <- log( dd$data[ ,cols.selected], base=log.b)
# lt.cmd <- paste( "lt <- log( ", full.list[ data.select], "$data[, cols.selected], base=", log.b, ")", sep="")
lt.cmd <- paste( "lt <- log( dd[[\"data\"]][, cols.selected], base=", log.b, ")", sep="")
eval( parse( text=lt.cmd))
write( lt.cmd, file="extRemes.log", append=TRUE)
if( tclvalue( ebase) == 1) newnames <- paste( cnames[temp], ".lt", sep="")
else newnames <- paste( cnames[temp], ".", bbext, "lt", sep="")
# cnames <- c( cnames, newnames)
for( i in 1:length( newnames)) {
	cnamesCMD <- paste( "cnames <- c( cnames, \"", newnames[i], "\")", sep="")
	eval( parse( text=cnamesCMD))
	write( cnamesCMD, file="extRemes.log", append=TRUE)
	} # end of for 'i' loop.

# dd$data <- cbind( dd$data, lt)
# dataCMD <- paste( full.list[ data.select], "$data <- cbind( ", full.list[ data.select], "$data, lt)", sep="")
dataCMD <- "dd[[\"data\"]] <- cbind( dd[[\"data\"]], lt)"
eval( parse( text=dataCMD))
write( dataCMD, file="extRemes.log", append=TRUE)
# colnames( dd$data) <- cnames
# colnamesCMD <- paste( "colnames( ", full.list[ data.select], "$data) <- cnames", sep="")
colnamesCMD <- "colnames( dd[[\"data\"]]) <- cnames"
eval( parse( text=colnamesCMD))
write( colnamesCMD, file="extRemes.log", append=TRUE)

assignCMD <- paste( "assign( \"", full.list[ data.select], "\", dd, pos=\".GlobalEnv\")", sep="")
eval( parse( text=assignCMD))
write( assignCMD, file="extRemes.log", append=TRUE)

# tkconfigure( base.txt, state="normal")
msg <- paste( "\n", "log base ", round( log.b, digits=6), " taken for", "\n",
		" ", cols.selected, " and assigned to ", newnames, sep="")
# tkinsert( base.txt, "end", msg)
cat( msg)
tkdestroy( base)
# tkconfigure( base.txt, state="disabled")
invisible()
	} # end of log.trans fcn

lthelp <- function() {
	# tkconfigure( base.txt, state="normal")
	help.msg1 <- paste( " ",
"This is a simple function that takes a data set X and returns the",
"log (base bb) of X.  That is", "  ", sep="\n")
	help.msg2 <- paste(" ", "log( X, base=bb)", " ", sep="\n")
	help.msg3 <- paste( " ", "Returns an object of class \"extRemesDataObject\" ",
			" ", sep="\n")
	# tkinsert( base.txt, "end", help.msg1)
	cat( help.msg1)
	# tkinsert( base.txt, "end", help.msg2)
	cat( help.msg2)
	# tkinsert( base.txt, "end", help.msg3)
	cat( help.msg3)
	# tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of lthelp

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Logarithmic Transformation")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# Top frame for data sets...

data.listbox <- tklistbox( top.frm,
			yscrollcommand=function(...) tkset(data.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

data.scroll <- tkscrollbar( top.frm, orient="vert",
			command=function(...) tkyview( data.listbox, ...))

temp <- ls(all=TRUE, name=".GlobalEnv")
full.list <- character(0)
is.nothing <- TRUE
for( i in 1:length( temp)) {
        if( is.null( class( get( temp[i])))) next
        if( (class(get( temp[i]))[1] == "extRemesDataObject")) {
                tkinsert( data.listbox, "end", paste( temp[i]))
        	full.list <- c( full.list, temp[i])
		is.nothing <- FALSE
		}
} # end of for i loop

tkpack( tklabel( top.frm, text="Data Object", padx=4), side="top")
tkpack( data.listbox, data.scroll, side="left", fill="y")
# tkpack( data.scroll, side="right", fill="y")
tkpack( top.frm)
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# Middle frame for columns of chosen dataset...

leftmid.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
col.listbox <- tklistbox( leftmid.frm,
			yscrollcommand=function(...) tkset( col.scroll, ...),
			selectmode="multiple",
			width=20,
			height=5,
			exportselection=0)

col.scroll <- tkscrollbar( leftmid.frm, orient="vert",
			command=function(...) tkyview( col.listbox, ...))

if( is.nothing) {
for( i in 1:ncol( extRemesData$data))
	tkinsert( col.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
	} else tkinsert( col.listbox, "end", "")

tkpack( tklabel( leftmid.frm, text="Variables to Transform", padx=4),
		side="top")
tkpack( col.listbox, side="left")
tkpack( col.scroll, side="right")

# Choose which base if other than 'exp'...

rightmid.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")

exp.base <- tkcheckbutton( rightmid.frm, text="Use Exponential Base",
			variable=ebase)
tkpack( exp.base, side="top")

other.base <- tkentry( rightmid.frm, textvariable=bb, width=4)
tkpack( other.base, tklabel( rightmid.frm, text="Use other base", padx=4),
		side="bottom")
tkpack( leftmid.frm, rightmid.frm, side="left")

ok.but <- tkbutton( bot.frm, text="OK", command=log.trans)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)

help.but <- tkbutton( bot.frm, text="Help", command=lthelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings for return key.
tkbind( ok.but, "<Return>", log.trans)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", lthelp)

tkpack( top.frm, fill="x")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of logtrans.gui fcn
