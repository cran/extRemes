rlplot.gui <- function( base.txt) {

conf.value <- tclVar("0.05")
rlevel.value <- tclVar("")

# Refresh fcn 
refresh <- function() {
	tkdelete( fit.listbox, 0.0, "end")
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd <- get( full.list[ data.select])
                } else stop("fitdiag.gui: Must load a data object!")
	models.fit <- names( dd$models)
	for( i in 1:length( models.fit))
		tkinsert( fit.listbox, "end", paste( models.fit[i]))
	invisible()
} # end of refresh fcn.

submit <- function() {
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd <- get( full.list[ data.select])
                } else stop("fitdiag.gui: Must load a data object!")
	fit.select <- as.numeric( tkcurselection( fit.listbox))+1
	rlvl <- as.character( tclvalue( rlevel.value))
	if( rlvl=="") return.level( dd$models[[ fit.select]], conf=as.numeric(tclvalue(conf.value)))
	else {
		rlvl <- get( rlvl)
		return.level( dd$models[[ fit.select]], conf=as.numeric(tclvalue(conf.value)),rlevels=rlvl)
		}
	invisible()
} # end of submit fcn.

rlhelp <- function( base.txt) {
	help( return.level)
	} # end of rlhelp fcn

endprog <- function() {
	tkdestroy( base)
	}
# Function to plot diagnostic plots for various fits.

#####################
# Frame/button setup.
#####################

base <- tktoplevel()
tkwm.title( base, "Return Level Plot for Fitted Object")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
rlvl.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

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

tkpack( tklabel( top.frm, text="Data Object:  ", padx=4), side="left")
tkpack( data.listbox, data.scroll,  side="left", fill="y")

# Place bindings on data listbox to update fit listbox.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# Middle frame for choosing confidence level and which fit to plot.
conf.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
conf.entry <- tkentry( conf.frm, textvariable=conf.value, width=5)
tkpack( tklabel( conf.frm, text="Confidence Level", padx=4), conf.entry, side="left")

fit.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
fit.listbox <- tklistbox( fit.frm,
			yscrollcommand=function(...) tkset( fit.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

fit.scroll <- tkscrollbar( fit.frm, orient="vert",
			command=function(...) tkyview( fit.listbox, ...))
tkinsert( fit.listbox, "end", "")

tkpack( tklabel( fit.frm, text="Select a fit: ", padx=4), side="left")
tkpack( fit.listbox, fit.scroll, side="left", fill="y")
tkpack(conf.frm, fit.frm, side="top")

# Return Level frame.
rlvl.entry <- tkentry( rlvl.frm, textvariable=rlevel.value, width=10)
tkpack( tklabel( rlvl.frm, text="Specific Return Levels",padx=4),rlvl.entry,side="left")

# Bottom frame for execution and cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=rlhelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings on buttons.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", rlhelp)

tkpack( top.frm, mid.frm, rlvl.frm, bot.frm, side="top", fill="x")
invisible()


} # end of fcn
