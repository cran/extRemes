negtrans.gui <- function(base.txt) {

# This function provides a gui interface for finding the negative (for mins)
# transformation of a dataset.  The gui will list all objects of class
# "ev.data" and the column names of the user selected object.
# After taking the negative of the selected data, will add a new column to the
# data object with the extension ".neg" attached to the original column name(s).

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

neg.trans <- function() {
# Function invoked when the "ok" button is pressed.  Actually takes negative
# transformation of the data.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	} else dd <- .ev

cols.selected <- character(0)

# Gather column names to add negative transform(s) to end.
cnames <- colnames( dd$data)

temp <- as.numeric( tkcurselection( col.listbox)) + 1

# Make sure a column has been selected.
if( length( temp) == 0) return()

cols.selected <- cnames[temp]

neg <- -dd$data[ ,cols.selected]
newnames <- paste( cnames[temp], ".neg", sep="")
cnames <- c( cnames, newnames)
dd$data <- cbind( dd$data, neg)
colnames( dd$data) <- cnames
assign( full.list[ data.select], dd, pos=".GlobalEnv")

tkconfigure( base.txt, state="normal")
msg <- paste( "\n", "Negative transformation taken for", "\n",
		" ", cols.selected, " and assigned to ", newnames, sep="")
tkinsert( base.txt, "end", msg)
tkdestroy( base)
tkconfigure( base.txt, state="disabled")
invisible()
	} # end of neg.trans fcn

neghelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg1 <- paste( " ",
"This is a simple function that takes a data set X and returns -X.", "  ",
			sep="\n")
	tkinsert( base.txt, "end", help.msg1)
	tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of neghelp fcn

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Negative Transformation")

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
tkpack( top.frm)
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# Middle frame for columns of chosen dataset...

col.listbox <- tklistbox( mid.frm,
			yscrollcommand=function(...) tkset( col.scroll, ...),
			selectmode="multiple",
			width=20,
			height=5,
			exportselection=0)

col.scroll <- tkscrollbar( mid.frm, orient="vert",
			command=function(...) tkyview( col.listbox, ...))

if( is.nothing) {
for( i in 1:ncol( .ev$data))
	tkinsert( col.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
	} else tkinsert( col.listbox, "end", "")

tkpack( tklabel( mid.frm, text="Variables to Transform", padx=4),
		side="top")
tkpack( col.listbox, side="left")
tkpack( col.scroll, side="right")

ok.but <- tkbutton( bot.frm, text="OK", command=neg.trans)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)

help.but <- tkbutton( bot.frm, text="Help", command=neghelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings for return key.
tkbind( ok.but, "<Return>", neg.trans)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", neghelp)

tkpack( top.frm, fill="x")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of negtrans.gui fcn
