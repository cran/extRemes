extremalind.gui <- function( base.txt) {
# This function provides gui support for the 'extremalindex' fcn
# As of version 1.51 it supports the function 'eiAnalyze' instead.

# Initialize tcl values.

u.value <- tclVar("")
conf.value <- tclVar("0.95")
iter.value <- tclVar("500")
plotVal <- tclVar(0)

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
	if( tclvalue(u.value)=="") u.val <- NULL
	else u.val <- as.numeric( tclvalue(u.value))

	conf.val <- as.numeric( tclvalue( conf.value))
	iter.val <- as.numeric( tclvalue( iter.value))
	plotValue <- as.logical( as.numeric( tclvalue(plotVal)))

	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd.cmd <- paste( "dd <- get( \"", full.list[ data.select], "\")", sep="")
	eval( parse( text=dd.cmd))
	write( dd.cmd, file="extRemes.log", append=TRUE)

	var.select <- as.numeric( tkcurselection( var.listbox))+1
	xdat.cmd <- paste( "xdat <- dd[[\"data\"]][, ", var.select, "]", sep="")
	eval( parse( text=xdat.cmd))
	write( xdat.cmd, file="extRemes.log", append=TRUE)

	# look.cmd <- paste("look <- extremalindex( xdat, ", u.val, ")", sep="")
	if( !is.null( u.val)) look.cmd <- paste("look <- eiAnalyze( x=xdat, thresholds=", u.val, ", conf=", conf.val, ", iter=", iter.val, ")", sep="")
	else look.cmd <- paste("look <- eiAnalyze( x=xdat, conf=", conf.val, ", iter=", iter.val, ", plot=", plotValue, ")", sep="")
	eval( parse( text=look.cmd))
	write( look.cmd, file="extRemes.log", append=TRUE)
	if( length( look$ei) == 1) {
	  msg0 <- paste("Threshold = ", look$u, sep="")
	  msg1 <- paste("Extremal index estimate: ", round( look$ei, digits=5),
			" (", round( look$ci[1], digits=5), ", ", round( look$ci[2], digits=5), ")", sep="")
	  msg2 <- paste("Estimated number of clusters: ", look$nc, sep="")
          msg3 <- paste("Estimated optimal run length (for runs declustering): ", look$run.length, sep="")
	  cat( msg0, "\n")
	  cat( msg1, "\n")
	  cat( msg2, "\n")
	  cat( msg3, "\n")
	} else {
	   MsgTable <- data.frame( look$u, look$nc, look$run.length,
				paste("  ", round(look$ei,digits=5), " (",
				round( look$ci[,1],digits=5), ", ", round( look$ci[,2],digits=5), ")", sep=""))
	   colnames( MsgTable) <- c("Thresholds", "No. of Clusters", "Run Length", "Extremal Index")
	   print( MsgTable)
	}
	# msg4 <- look$msg
	tkdestroy( base)
	invisible()
	} # end of submit fcn

extindhelp <- function() {
	cat( "\n", paste( "Invokes the function: \'eiAnalyze\'  ",
				"Use \'help( eiAnalyze)\' for more help.", " ", sep="\n"))
	help( eiAnalyze)
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
        if( (class(get( temp[i]))[1] == "extRemesDataObject")) {
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
u.frm <- tkframe( midright, borderwidth=2, relief="flat")
confFrm <- tkframe( midright, borderwidth=2, relief="flat")
iterFrm <- tkframe( midright, borderwidth=2, relief="flat")
plotFrm <- tkframe( midright, borderwidth=2, relief="flat")

u.entry <- tkentry( u.frm, textvariable=u.value, width=5)
tkpack( tklabel( u.frm, text="Threshold", padx=4), u.entry, side="left")

conf.entry <- tkentry( confFrm, textvariable=conf.value, width=5)
tkpack( tklabel( confFrm, text="Confidence", padx=4), conf.entry, side="left")

iter.entry <- tkentry( iterFrm, textvariable=iter.value, width=5)
tkpack( tklabel( iterFrm, text="Bootstrap interations", padx=4), iter.entry, side="left")

plot.but<- tkcheckbutton(plotFrm,text="Plot estimates (only if Threshold field blank)", variable=plotVal)
tkpack(plot.but,side="left")

tkpack( u.frm, confFrm, iterFrm, plotFrm, side="top")

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
