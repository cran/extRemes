gpdfitrange.gui <- function( base.txt) {

#
# Function to provide guis for the 'gpd.fitrange' fcn of Stuart Coles.
#

# Initialize tcl variables.

umin.value <- tclVar("")
umax.value <- tclVar("")
nint.value <- tclVar("")

# Internal functions...

refresh <- function() {
#
# function to determine which variables exist given data object chosen.

	# Remove recent contents of 'var.listbox'.
	tkdelete( var.listbox, 0.0, "end")

	# Obtain recent data object chosen.
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])

	# put column names into 'var.listbox'.
	tmp <- colnames( dd$data)
	for( i in 1:length( tmp))
		tkinsert( var.listbox, "end", paste( tmp[i]))

	invisible()
	} # end of refresh fcn

submit <- function() {

# Function that is executed when the "OK" button is hit.  Actually calls
# the 'gpd.fitrange' fcn.

# Obtain argument values entered into gui.
	umin.val <- as.numeric( tclvalue( umin.value))
	umax.val <- as.numeric( tclvalue( umax.value))
	nint.val <- as.numeric( tclvalue( nint.value))

# Obtain data to use in 'gpd.fitrange' fcn.
	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else  dd <- .ev
		# end of if else !is.nothing stmt

	var.select <- as.numeric( tkcurselection( var.listbox))+1
	var.val <- dd$data[, var.select]

	gpd.fitrange( var.val, umin.val, umax.val, nint.val)

	msg <- paste( "  ", "gpd.fitrange executed",
			"If error message occurs try",
			"different threshold ranges.", " ", sep="\n")
	msg2 <- paste( "Current range: ", umin.val, " to ", umax.val, " with ",
			nint.val, " intervals.", sep="")
	nl1 <- paste( " ", " ", sep="\n")
	tkconfigure( base.txt, state="normal")
	tkinsert( base.txt, "end", msg)
	tkinsert( base.txt, "end", msg2)
	tkinsert( base.txt, "end", nl1)

	tkdestroy( base)
        tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of submit fcn

gpdfitrangehelp <- function() {
	help( gpd.fitrange)
	invisible()
	} # end of gpdfitrangehelp fcn

endprog <- function() {
	tkdestroy( base)
	} # end of endprog fcn

#######################
# Frame/button setup
#######################

base <- tktoplevel()
tkwm.title( base, "Fit GPD for threshold ranges")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# Top frame to select data object.

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
	if( (class( get( temp[i])) == "ev.data")) {
		tkinsert( data.listbox, "end", paste( temp[i]))
		full.list <- c( full.list, temp[i])
		is.nothing <- FALSE
			}
	} # end of for i loop

tkpack( tklabel( top.frm, text="Data Object", padx=4), side="left")
tkpack( data.listbox, side="left")
tkpack( data.scroll, side="right", fill="y")
tkpack( top.frm)

# place binding on 'data.listbox' to reflect the chosen data from list.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# Middle frame tochoose which columns (vars) of data to use and other args
# to 'gpd.fitrange' fcn.

midleft <- tkframe( mid.frm, borderwidth=2, relief="groove")

var.listbox <- tklistbox( midleft,
			yscrollcommand=function(...) tkset( var.scroll, ...),
			selectmode="single",
			width=15,
			height=6,
			exportselection=0)

var.scroll <- tkscrollbar( midleft, orient="vert",
			command=function(...) tkyview( var.listbox, ...))

# Insert column names of '.ev' if no other data objects exist.
# Otherwise, begin with no variables.
if( is.nothing) {
	for( i in 1:length( colnames( .ev$data)))
		tkinsert( var.listbox, "end",
				paste( colnames( .ev$data)[i]))
} else tkinsert( var.listbox, "end", " ")

tkpack( tklabel( midleft, text="Select Variable", padx=4), side="top")
tkpack( var.listbox, side="left")
tkpack( var.scroll, side="right")

# Other args to 'gpd.fitrange' fcn.

midright <- tkframe( mid.frm, borderwidth=2, relief="groove")

umin.frm <- tkframe( midright, borderwidth=2, relief="flat")
umin.entry <- tkentry( umin.frm, textvariable=umin.value, width=5)

umax.frm <- tkframe( midright, borderwidth=2, relief="flat")
umax.entry <- tkentry( umax.frm, textvariable=umax.value, width=5)

nint.frm <- tkframe( midright, borderwidth=2, relief="flat")
nint.entry <- tkentry( nint.frm, textvariable=nint.value, width=5)

tkpack( tklabel( umin.frm, text="Minimum Threshold", padx=4), umin.entry,
 		side="left", fill="y", anchor="w")
tkpack( tklabel( umax.frm, text="Maximum Threshold", padx=4), umax.entry,
 		side="left", fill="y", anchor="w")
tkpack( tklabel( nint.frm, text="Number of thresholds", padx=4), nint.entry,
 		side="left", fill="y", anchor="w")

tkpack( umin.frm)
tkpack( umax.frm)
tkpack( nint.frm)

tkpack( midleft, side="left")
tkpack( midright, side="right")

# Bottom frame for execution or cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=gpdfitrangehelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# Place bindings on "OK" and "Cancel" so that Return key will execute them.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", gpdfitrangehelp)

tkpack( top.frm, side="top")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of fcn
