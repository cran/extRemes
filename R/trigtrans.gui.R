trigtrans.gui <- function(base.txt) {

# This function provides a gui interface for finding the trigonometric
# transformation of a dataset.  The gui will list all objects of class
# "ev.data" and the column names of the user selected object.
# After taking the sine (sin(2pi*t/T) and cosine (cos(2pi*t/T)) of the selected data,
# will return two new columns with the same column names, but with a ".sinT" or ".cosT"
# extension (for sine/cosine with period T).

# Set the tcl variables

period.val <- tclVar(365.25)

# Internal functions

refresh <- function() {

# When data is selected, this function fills the lists for the columns of
# the data set so that the user can select which column(s) to transform.

	tkdelete( col.listbox, 0.0, "end")

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else dd <- .ev

	for( i in 1:ncol( dd$data))
		tkinsert( col.listbox, "end",
			paste( colnames( dd$data)[i]))

	invisible()
	} # end of refresh fcn

trig.trans <- function() {
# Function invoked when the "ok" button is pressed.  Actually takes trigonometric
# transformation of the data.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	} else dd <- .ev


cols.selected <- character(0)

# Gather column names to add trig transforms to end.
cnames <- colnames( dd$data)

temp <- as.numeric( tkcurselection( col.listbox)) + 1

X <- dd$data[,temp]
Tp <- as.numeric( tclvalue( period.val))
sinT <- sin( 2*pi*X/Tp)
cosT <- cos( 2*pi*X/Tp)

# Make sure a column has been selected.
if( is.na( temp)) return()

cols.selected <- cnames[temp]

newnames <- c( paste( cnames[temp], ".", "sin", round(Tp, digits=0), sep=""),
		paste( cnames[temp], ".", "cos", round(Tp, digits=0), sep=""))
cnames <- c( cnames, newnames)
dd$data <- cbind( dd$data, sinT, cosT)
colnames( dd$data) <- cnames
assign( full.list[ data.select], dd, pos=".GlobalEnv")

tkconfigure( base.txt, state="normal")
msg <- paste( " ", "sin(2pi*X/T) and cos(2pi*X/T) taken for", " ", sep="\n")
tkinsert( base.txt, "end", msg)
for( i in 1:length( cols.selected)) {
	msg1 <- paste( cols.selected[i], ", ", sep="")
	msg2 <- paste(" assigned to ", deparse( newnames), sep="")
	tkinsert( base.txt, "end", msg1)
	tkinsert( base.txt, "end", msg2)
	} # end of for i loop
nl1 <- paste(" ", "**************", " ", sep="\n")
tkinsert( base.txt, "end", nl1)
tkdestroy( base)
tkconfigure( base.txt, state="disabled")
invisible()
	} # end of trig.trans fcn

trigtranshelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg1 <- paste( " ",
"This is a simple function that takes a data set X and returns two columns:",
"sin(2*pi*X/T) and cos(2*pi*X/T), with T = period.", " ",
"Returns two extra columns for each column transformed to data set.",
"New data set will have the same previous column names, but with sinT and cosT",
"extensions, where T is the period rounded to 0 digits.", sep="\n")
	tkinsert( base.txt, "end", help.msg1)
	tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of trigtranshelp

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Trigonometric Transformation")

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
        if( (class(get( temp[i]))[1] == "ev.data")) {
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
for( i in 1:ncol( .ev$data))
	tkinsert( col.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
	} else tkinsert( col.listbox, "end", "")

tkpack( tklabel( leftmid.frm, text="Variables to Transform", padx=4),
		side="top")
tkpack( col.listbox, side="left")
tkpack( col.scroll, side="right")

# Choose period...

rightmid.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")

period.entry <- tkentry( rightmid.frm, textvariable=period.val, width=4)
tkpack( period.entry, tklabel( rightmid.frm, text="Period", padx=4), side="right")
tkpack( leftmid.frm, rightmid.frm, side="left")

ok.but <- tkbutton( bot.frm, text="OK", command=trig.trans)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)

help.but <- tkbutton( bot.frm, text="Help", command=trigtranshelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings for return key.
tkbind( ok.but, "<Return>", trig.trans)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", trigtranshelp)

tkpack( top.frm, fill="x")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of logtrans.gui fcn
