gpddiag.gui <- function( base.txt) {
#
# This function provides a gui for the 'gpd.diag' fcn of
# Stuart Coles.
#

# Internal functions.

submit <- function() {

# This is the primary function that is called when the 
# "ok" button is pressed.

	# Grab the chosen (or default) data object.
	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else dd <- .ev

	# Make sure it has a 'gpd.fit' object, and if it
	# does not, print a message.
	if( is.null( dd$gpd.fit)) {
		msg <- paste( "************", "No gpd.fit in data object!",
			"Must fit a GPD first.", "************", sep="\n")
		tkconfigure( base.txt, state="normal")
		tkinsert( base.txt, "end", msg)
	} else {
		gpd.diag( dd$gpd.fit)
		ddname <- ifelse( is.nothing, ".ev", full.list[ data.select])
		msg <- paste( "************", "GPD fit diagnostics plotted for",
				ddname, "************", sep="\n")
		tkconfigure( base.txt, state="normal")
                tkinsert( base.txt, "end", msg)
		} # end of if else 'gpd.fit' stmt

	tkdestroy( base)
	tkconfigure( base.txt, state="disabled")
	} # end of submit fcn

gpddiaghelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg <- paste( " ", "Simply creates diagnostic plots for GPD fit",
"Must give it an object of class \"gpd.fit\" .", " ", sep="\n")
	tkinsert( base.txt, "end", help.msg)
	tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of gpddiaghelp fcn

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup.
#####################

base <- tktoplevel()
tkwm.title( base, "Diagnostic Plots for GPD Fit")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# Top frame to select the data object.

data.listbox <- tklistbox( top.frm,
			yscrollcommand=function(...) tkset( data.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

data.scroll <- tkscrollbar( top.frm, orient="vert",
			command=function(...) tkyview( data.listbox, ...))

temp <- ls( all=TRUE, name=".GlobalEnv")
full.list <- character(0)
is.nothing <- TRUE
for( i in 1:length( temp)) {
	if( is.null( class( get( temp[i])))) next
	if( (class( get( temp[i]))[1] == "ev.data")) {
		tkinsert( data.listbox, "end", paste( temp[i]))
		full.list <- c( full.list, temp[i])
		is.nothing <- FALSE
		}
	} # end of for i loop

tkpack( tklabel( top.frm, text="Data Object (if empty uses most recent set)",
		padx=4), side="left")
tkpack( data.listbox, side="left")
tkpack( data.scroll, side="right", fill="y")
tkpack( top.frm)

# Bottom frame for execution and cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=gpddiaghelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings on buttons.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", gpddiaghelp)

tkpack( top.frm, side="top")
tkpack( bot.frm, side="bottom")
invisible()
} # end of fcn
