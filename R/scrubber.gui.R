scrubber.gui <- function( base.txt) {

##################################
# Internal functions
##################################

# Refresh fcn 
refresh <- function() {
	tkdelete( fit.listbox, 0.0, "end")
	tkdelete( resp.listbox, 0.0, "end")
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd <- get( full.list[ data.select])
                } else stop("fitdiag.gui: Must load a data object!")
	for( i in 1:ncol(dd$data)) tkinsert( resp.listbox, "end", paste( colnames( dd$data)[i]))
	models.fit <- names( dd$models)
	for( i in 1:length( models.fit))
		tkinsert( fit.listbox, "end", paste( models.fit[i]))
	invisible()
} # end of refresh fcn.

submit <- function() {
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
		data.name <- full.list[ data.select]
                dd <- get( full.list[ data.select])
                } else stop("scrubber.gui: Must load a data object!")

	# Scrub any selected data columns.
	resp.select <- as.numeric( tkcurselection( resp.listbox))+1
	if( tclvalue( tkcurselection( resp.listbox)) != "") {
		tmp.names <- colnames( dd$data)
		if( length( resp.select) == ncol( dd$data)) {
			warning("scrubber.gui: Removing entire data set!")
			dd$data <- NULL
		} else {
			tmp <- dd$data[,-resp.select]
			colnames( tmp) <- tmp.names[-resp.select]
			dd$data <- tmp
			} # end of if else remove all data stmt.
		} # end of if a data column selected or not stmt.

	# Scrub any selected model fits.
	fit.select <- as.numeric( tkcurselection( fit.listbox))+1
	if( tclvalue( tkcurselection( fit.listbox)) != "" ) {
		if( length( fit.select) == length( dd$models)) {
			warning("scrubber.gui: Removing all model fits!")
			dd$models <- NULL
		} else {
			tmp.names <- names( dd$models)[-fit.select]
			tmp <- list()
			for( i in 1:length( tmp.names)) if( !any( i == fit.select)) tmp[[i]] <- dd$models[[i]]
			names( tmp) <- tmp.names
			dd$models <- tmp
			} # end of if else remove all models stmt.
		} # end of if any models selected or not stmt.
	assign( data.name, dd, pos=".GlobalEnv")
	tkdestroy(base)
    tkconfigure(base.txt,state="disabled")
} # end of submit fcn.

scrubberhelp <- function() {
	tkconfigure( base.txt, state="normal")
	nl1 <- paste(" ", "---------------------------------", " ", sep="\n")
        help.msg1 <- paste( " ", "Scrubber Info:", " ", "Select data columns and/or model fits for deletion.", " ", sep="\n")
	help.msg2 <- paste(" ", "To remove an entire data object use: ", " ", "> rm object.name", " ",
				" ", "from the R command prompt.", sep="\n")
	tkinsert( base.txt, "end", nl1)
        tkinsert( base.txt, "end", help.msg1)
	tkinsert( base.txt, "end", help.msg2)
	tkinsert( base.txt, "end", nl1)
        tkconfigure( base.txt, state="disabled")
        invisible()
	} # end of scrubberhelp fcn

endprog <- function() {
	tkdestroy( base)
	}
# Function to plot diagnostic plots for various fits.

#####################
# Frame/button setup.
#####################

base <- tktoplevel()
tkwm.title( base, "Scrubber")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
top.l <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

## Data Objects.
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

tkpack( tklabel( top.frm, text="Data Object:  ", padx=4), side="left")
tkpack( data.listbox, data.scroll,  side="left", fill="y")

# Place bindings on data listbox to update fit listbox.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

## Data Columns (to be scrubbed).
resp.listbox <-
        tklistbox(top.l,yscrollcommand=function(...)tkset(resp.scroll,...),
                        selectmode="multiple",width=35,height=4,exportselection=0)
resp.scroll <- tkscrollbar(top.l,orient="vert",
                        command=function(...)tkyview(resp.listbox,...))

if( is.nothing) {
for( i in 1:ncol(.ev$data))
        tkinsert( resp.listbox, "end", paste(colnames(.ev$data)[i]))
# end of for i loop
        } else tkinsert( resp.listbox, "end", "")

tkpack(tklabel(top.l,text="Data Columns:",padx=4), side="left")
tkpack(resp.listbox,side="left")
tkpack(resp.scroll,side="right",fill="y")

## Middle frame for choosing which fit to plot.
fit.listbox <- tklistbox( mid.frm,
			yscrollcommand=function(...) tkset( fit.scroll, ...),
			selectmode="multiple",
			width=20,
			height=5,
			exportselection=0)

fit.scroll <- tkscrollbar( mid.frm, orient="vert",
			command=function(...) tkyview( fit.listbox, ...))
tkinsert( fit.listbox, "end", "")

tkpack( tklabel( mid.frm, text="Model Fits: ", padx=4), side="left")
tkpack( fit.listbox, fit.scroll, side="left", fill="y")

# Bottom frame for execution and cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=scrubberhelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings on buttons.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", scrubberhelp)

tkpack( top.frm, top.l, mid.frm, bot.frm, side="top", fill="x")
invisible()


} # end of fcn
