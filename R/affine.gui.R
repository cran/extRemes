affine.gui <- function(base.txt) {

# This function provides a gui interface for taking an affine transformation of data, 'x'.
# That is, (x-c)/b, where 'c' and 'b' are constants.  The gui will list all objects of class
# "ev.data" and the column names of the user selected object.
#
# After taking the affine transformation of the selected data, will return a new column with
# the same column name(s), but with a ".cNbM" extension where 'N' is the value used for 'c' and
# 'M' the value used for 'b'.

# Set the tcl variables

c <- tclVar("0")
b <- tclVar("1") # Internal functions

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

affine.trans <- function() {
# Function invoked when the "ok" button is pressed.  Actually takes affine
# transformation of the data.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	} else dd <- .ev

cols.selected <- character(0)

# Gather column names to add affine transform(s) to end.
cnames <- colnames( dd$data)

temp <- as.numeric( tkcurselection( col.listbox)) + 1

# Make sure a column has been selected.
if( is.na( temp)) return()

cols.selected <- cnames[temp]
c.value <- as.numeric(tclvalue(c))
b.value <- as.numeric(tclvalue(b))
cb <- (dd$data[ ,cols.selected]-c.value)/b.value

newnames <- paste( cnames[temp], ".c", c.value, "b", b.value, sep="")
cnames <- c( cnames, newnames)
dd$data <- cbind( dd$data, cb)
colnames( dd$data) <- cnames
assign( full.list[ data.select], dd, pos=".GlobalEnv")

tkconfigure( base.txt, state="normal")
msg <- paste( "\n", "Affine transformation (x - ", c.value, ")/", b.value, " taken for", "\n",
		" ", cols.selected, " and assigned to ", newnames, sep="")
tkinsert( base.txt, "end", msg)
tkdestroy( base)
tkconfigure( base.txt, state="disabled")
invisible()
	} # end of affine.trans fcn

affinehelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg1 <- paste( " ",
"This is a simple function that takes a data set X and returns the",
"affine transformation of X.  That is", "  ", sep="\n")
	help.msg2 <- paste(" ", "(X - c)/b", " ", sep="\n")
	help.msg3 <- paste( " ", "Returns object of class \"ev.data\" with an extra column in the data",
			"indicated by a .cNbM extension where N and M are the values of b and c respectively.",
			" ", sep="\n")
	tkinsert( base.txt, "end", help.msg1)
	tkinsert( base.txt, "end", help.msg2)
	tkinsert( base.txt, "end", help.msg3)
	tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of affinehelp

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Affine Transformation")

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
        if( (class(get( temp[i])) == "ev.data")) {
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

var.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
col.listbox <- tklistbox( var.frm,
			yscrollcommand=function(...) tkset( col.scroll, ...),
			selectmode="multiple",
			width=20,
			height=5,
			exportselection=0)

col.scroll <- tkscrollbar( var.frm, orient="vert",
			command=function(...) tkyview( col.listbox, ...))

if( is.nothing) {
for( i in 1:ncol( .ev$data))
	tkinsert( col.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
	} else tkinsert( col.listbox, "end", "")

tkpack( tklabel( var.frm, text="Variables to Transform", padx=4),
		side="top")
tkpack( col.listbox, side="left")
tkpack( col.scroll, side="right")
tkpack( var.frm, side="top")

c.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
c.entry <- tkentry( c.frm, textvariable=c, width=4)
tkpack( tklabel( c.frm, text="c", padx=4), c.entry, side="left")

b.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
b.entry <- tkentry( b.frm, textvariable=b, width=4)
tkpack( tklabel( b.frm, text="b", padx=4), b.entry, side="left")
tkpack( c.frm, b.frm, side="left")

ok.but <- tkbutton( bot.frm, text="OK", command=affine.trans)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)

help.but <- tkbutton( bot.frm, text="Help", command=affinehelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings for return key.
tkbind( ok.but, "<Return>", affine.trans)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", affinehelp)

tkpack( top.frm, fill="x")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of affine.gui fcn
