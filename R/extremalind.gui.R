extremalind.gui <- function( base.txt) {
# This function provides gui support for the 'extremalindex' fcn

# Initialize tcl values.

u.value <- tclVar(0)

# Internal functions.

refresh <- function() {
	tkdelete( var.listbox, 0.0, "end")

	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])

	tmp <- colnames( dd$data)
	for( i in 1:length( tmp))
		tkinsert( var.listbox, "end", paste( tmp[i]))

	invisible()
	} # end of refresh fcn

submit <- function() {
	u.val <- as.numeric( tclvalue(u.value))
	
	data.select <- as.numeric( tkcurselection( data.listbox))+1
        dd <- get( full.list[ data.select])

	var.select <- as.numeric( tkcurselection( var.listbox))+1
	xdat <- dd$data[, var.select]
	var.name <- colnames( dd$data)[ var.select]

	look <- extremalindex( xdat, u.val)

	tkconfigure( base.txt, state="normal")
	msg1 <- paste("Extremal index estimate: ",
			round( look$theta, digits=5), sep="")
	msg2 <- paste("Estimated number of clusters: ", look$C, sep="")
	msg3 <- paste("Estimated optimal run length (for runs declustering): ",
			look$run.length, sep="")
	msg4 <- look$msg
	nl1 <- paste("","***********","","",sep="\n")
	nl2 <- paste("","",sep="\n")
	tkinsert( base.txt, "end", nl1)
	tkinsert( base.txt, "end", msg1)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg2)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg3)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg4)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", nl1)
	tkconfigure( base.txt, state="disabled")
	tkdestroy( base)
	invisible()
	} # end of submit fcn

extindhelp <- function() {
	help( extremalindex)
	invisible()
	}

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Extremal index estimation")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# Top frame to select data object.

data.listbox <- tklistbox( top.frm,
			yscrollcommand=function(...) tkset( data.scroll,...),
			selectmode="single",
                        width=20,
                        height=5,
                        exportselection=0)

data.scroll <- tkscrollbar( top.frm, orient="vert",
                        command=function(...) tkyview( data.listbox, ...))

temp <- ls(all=TRUE, name=".GlobalEnv")
full.list <- character(0)
for( i in 1:length( temp)) {
        if( is.null( class( get( temp[i])))) next
        if( (class(get( temp[i])) == "ev.data")) {
                tkinsert( data.listbox, "end", paste( temp[i]))
        	full.list <- c( full.list, temp[i])
		}
	} # end of for i loop

tkpack( tklabel( top.frm, text="Data Object", padx=4), side="left")
tkpack( data.listbox, side="left")
tkpack( data.scroll, side="right", fill="y")
tkpack( top.frm)

# place binding on data.listbox to reflect the chosen data from the list.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# Middle frame to choose which column of data to use and other args.

midleft <- tkframe( mid.frm, borderwidth=2, relief="groove")

var.listbox <- tklistbox( midleft,
			yscrollcommand=function(...) tkset( var.scroll, ...),
			selectmode="single",
			width=15,
			height=6,
			exportselection=0)

var.scroll <- tkscrollbar( midleft, orient="vert",
                        command=function(...) tkyview( var.listbox, ...))

tkinsert( var.listbox, "end", " ")
tkpack( tklabel( midleft, text="Select Variable", padx=4),
		side="top")
tkpack( var.listbox, side="left")
tkpack( var.scroll, side="right")

# Frame for threshold.
midright <- tkframe( mid.frm, borderwidth=2, relief="groove")

u.entry <- tkentry( midright, textvariable=u.value, width=5)

tkpack( tklabel( midright, text="Threshold", padx=4), u.entry, side="top")

tkpack( midleft, midright, side="left")

# Bottom frame for execution or cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=extindhelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

tkpack( top.frm, side="top")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")

} # end of fcn
