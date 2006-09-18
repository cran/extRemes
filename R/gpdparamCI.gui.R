gpdparamCI.gui <- function( base.txt) {

#
# This function provides a gui for 'gpd.prof' of Stuart Coles.
#

# Initialize tcl variables.

m.value <- tclVar("100")
do.rl <- tclVar(1)
do.xi <- tclVar(1)
rl.xlow.value <- tclVar("")
rl.xup.value <- tclVar("")
xi.xlow.value <- tclVar("")
xi.xup.value <- tclVar("")
conf.value <- tclVar("0.95")
nint.value <- tclVar("100")
makeplot <- tclVar(0)
# npy.value <- tclVar("365")

# Internal functions.

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
	do.rl.value <- ifelse( as.numeric( tclvalue( do.rl)) == 1, TRUE, FALSE)
	do.xi.value <- ifelse( as.numeric( tclvalue( do.xi)) == 1, TRUE, FALSE)
	rl.only <- do.rl.value & !do.xi.value
	xi.only <- !do.rl.value & do.xi.value
	# Grab the data object and make sure it has a 'gpd.fit' component.
	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd.cmd <- paste( "dd <- get( \"", full.list[ data.select], "\")", sep="")
		} else dd.cmd <- "dd <- extRemesData"
	eval( parse( text=dd.cmd))
	write( dd.cmd, file="extRemes.log", append=TRUE)

	# fit <- dd$models[[as.numeric( tkcurselection( fit.listbox))+1]]
	# fit.cmd <- paste( "fit <- ",
	# 	full.list[ data.select], "$models[[", as.numeric( tkcurselection( fit.listbox))+1, "]]", sep="")
	fit.cmd <- paste( "fit <- dd[[\"models\"]][[", as.numeric( tkcurselection( fit.listbox))+1, "]]", sep="")
	eval( parse( text=fit.cmd))
	write( fit.cmd, file="extRemes.log", append=TRUE)
	if( length( fit$mle) != 2) stop("gpdprof.gui: No trends in parameters allowed!")
	if( class( fit) != "gpd.fit") {
		msg <- paste( "************", "Selected fit is not of class gpd.fit!",
                        "Must use a fitted GPD object.", "************", sep="\n")
		cat( msg)
                # tkconfigure( base.txt, state="normal")
                # tkinsert( base.txt, "end", msg)
		} else {
			# Collect inputs for fcn args.
			m.val <- as.numeric( tclvalue( m.value))
			conf.val <- as.numeric( tclvalue( conf.value))
			nint.val <- as.numeric( tclvalue( nint.value))
			makeplot2 <- ifelse( as.numeric( tclvalue( makeplot))==1, TRUE, FALSE)
			estRLup <- tclvalue(rl.xup.value)
			if( estRLup == "") estRLup <- "NULL"
			else estRLup <- as.numeric( estRLup)

			estRLdn <- tclvalue(rl.xlow.value)
			if( estRLdn == "") estRLdn <- "NULL"
			else estRLdn <- as.numeric( estRLdn)

			estXIup <- tclvalue(xi.xup.value)
			if( estXIup == "") estXIup <- "NULL"
			else estXIup <- as.numeric( estXIup)

			estXIdn <- tclvalue(xi.xlow.value)
			if( estXIdn == "") estXIdn <- "NULL"
                        else estXIdn <- as.numeric( estXIdn)

			# Here is the actual function call.
# 			ci <- gpd.parameterCI(fit,
# 					m=m.val,
# 					conf=conf.val,
# 					nint=nint.val,
# 					rl.only=rl.only,
# 					xi.only=xi.only,
# 					rl.xup=estRLup,
# 					rl.xlow=estRLdn,
# 					xi.xup=estXIup,
#                                         xi.xlow=estXIdn,
# 					make.plot=makeplot2)
			ci.cmd <- paste( "ci <- gpd.parameterCI(fit, m=", m.val,
                                      ", conf=", conf.val,
                                      ", nint=", nint.val,
                                      ", rl.only=", rl.only,
                                      ", xi.only=", xi.only,
                                      ", rl.xup=", estRLup,
                                      ", rl.xlow=", estRLdn,
                                      ", xi.xup=", estXIup,
                                      ",  xi.xlow=", estXIdn,
                                      ", make.plot=", makeplot2, ")", sep="")
			eval( parse( text=ci.cmd))
			write( ci.cmd, file="extRemes.log", append=TRUE)
	#		est.rl <- gpd.ret( fit, m.val)
			est.rl <- ci$rl$mle
			nl1 <- paste( " ", "*****", " ", sep="\n")
			nl2 <- paste( "  ", "  ", sep="\n")
if( !rl.only & !xi.only) msg1 <- paste( "Estmating CIs for GPD ", m.val,
			"-yr. return level and shape parameter (xi).", sep="")
else if( rl.only) msg1 <- paste( "Estmating CIs for GPD ", m.val,
                        "-yr. return level.", sep="")
else if( xi.only) msg1 <- paste( "Estmating CIs for GPD shape parameter (xi).", sep="")
if( !xi.only) {
	npy.msg <- paste("Using ", fit$npy, " days per year.", sep="")
	msg2b <- paste( "Estimated ", m.val, "-yr. return level = ",
			round( est.rl, digits=4), sep="")
	msg4 <- paste(m.val, "-year return level: ", 100*conf.val,
                	"% confidence interval approximately", sep="")
	msg5 <- paste("(", round( ci$rl$dn, digits=5), ",",
			round( ci$rl$up, digits=5), ")", sep="")
	}
if( !rl.only) {
	msg2c <- paste("Estimated (MLE) shape parameter = ", round( fit$mle[2], digits=4), sep="")
	msg6 <- paste("shape parameter (xi): ", 100*conf.val,
			"% confidence interval approximately", sep="")
	msg7 <- paste("(", round( ci$xi$dn, digits=5), ",",
			round( ci$xi$up, digits=5), ")", sep="")
	}

			# tkconfigure( base.txt, state="normal")
                	# tkinsert( base.txt, "end", nl1)
			cat( nl1)
			# tkinsert( base.txt, "end", msg1)
			cat( msg1)
			# tkinsert( base.txt, "end", nl2)
			cat( nl2)
	if( !xi.only) {
		# tkinsert( base.txt, "end", npy.msg)
		cat( npy.msg)
		# tkinsert( base.txt, "end", nl2)
		cat( nl2)
		# tkinsert( base.txt, "end", msg2b)
		cat( msg2b)
		# tkinsert( base.txt, "end", nl2)
		cat( nl2)
		}
	if( !rl.only) {
# 		tkinsert( base.txt, "end", nl2)
		cat( nl2)
# 		tkinsert( base.txt, "end", msg2c)
		cat( msg2c)
# 		tkinsert( base.txt, "end", nl2)
		cat( nl2)
		cat( nl2)
# 		tkinsert( base.txt, "end", nl2)
		}
	if( !xi.only) {
		# tkinsert( base.txt, "end", msg4)
		cat( msg4)
		# tkinsert( base.txt, "end", nl2)
		cat( nl2)
		# tkinsert( base.txt, "end", msg5)
		cat( msg5)
		# tkinsert( base.txt, "end", nl2)
		cat( nl2)
		}
	if( !rl.only) {
                # tkinsert( base.txt, "end", msg6)
		cat( msg6)
                # tkinsert( base.txt, "end", nl2)
		cat( nl2)
                # tkinsert( base.txt, "end", msg7)
		cat( msg7)
		# tkinsert( base.txt, "end", nl1)
		cat( nl2)
		}
		} # end of if else 'gpd.fit' present stmt
	# tkdestroy( base)
        # tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of submit fcn

endprog <- function() {
	tkdestroy( base)
	}

gpdprofhelp <- function() {
	# tkconfigure( base.txt, state="normal")
	msg1 <- paste("Estimates confidence intervals for m-year return level and shape parameter (xi)",
	"for fits to the GP distribution using the profile likelihood functions.",
	"", "Uses the R function splinefun to estimate the profile likelihood function and",
	"then uses a bisection search algorithm to find the intersection of this function",
	"with the horizontal line at 0.5c, where c is the (1-alpha)% quantile of a",
	"Chi-square (with df=1) distribution.", " ",
	"For more information please see Coles (2001) and the help file gpd.prof and",
	"gpd.profxi; the ismev package functions that find the profile likelihoods.", 
	"If an NA is returned, then the function failed to find the upcrossing.  In such",
	"an event, try using gpd.prof (or gpd.profxi) from the command lines.", sep="\n")
	# tkinsert(base.txt, "end", msg1)
	# tkconfigure( base.txt, state="disabled")
	cat( msg1)
	cat("\n", "See the help files for the \'ismev\' function \'gpd.prof\' and for the ",
		"\n", "\'extRemes\' function \'gpd.parameterCI\' for more help.\n")
	cat( "(e.g., \'help( gpd.prof)\')\n")
	help( gpd.prof)
	invisible()
	}

#####################
# Frame/button setup.
#####################

base <- tktoplevel()
tkwm.title( base, "Estimated confidence limits for m-year GPD return level and shape parameter (xi)")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
data.frm <- tkframe( top.frm, borderwidth=2, relief="groove")
fit.frm <- tkframe( top.frm, borderwidth=2, relief="groove")
extras.frm <- tkframe( base, borderwidth=2, relief="flat")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# Top frame to select data object.

data.listbox <- tklistbox( data.frm,
			yscrollcommand=function(...) tkset( data.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

data.scroll <- tkscrollbar( data.frm, orient="vert",
			command=function(...) tkyview( data.listbox, ...))

temp <- ls( all=TRUE, name=".GlobalEnv")
full.list <- character(0)
is.nothing <- TRUE
for( i in 1:length( temp)) {
	if( is.null( class( get( temp[i])))) next
	if( (class( get( temp[i]))[1] == "extRemesDataObject")) {
		tkinsert( data.listbox, "end", paste( temp[i]))
		full.list <- c( full.list, temp[i])
		is.nothing <- FALSE
		}
	} # end of for i loop

tkpack( tklabel( data.frm, text="Data Object", padx=4), side="top")
tkpack( data.listbox, side="left")
tkpack( data.scroll, side="right", fill="y")

# Place bindings on data listbox to update fit listbox.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)
tkpack( data.frm, fit.frm, side="left")

# Middle frame for choosing which fit to plot.
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
tkpack( data.frm, fit.frm, fill="y", side="left")

# Middle frame to enter arguments for 'gpd.prof' fcn.

# Frame for m-year return level.
m.frm <- tkframe( extras.frm, borderwidth=2, relief="groove")
m.entry <- tkentry( m.frm, textvariable=m.value, width=5)

# Frame for number of points per year.
# npy.frm <- tkframe( extras.frm, borderwidth=2, relief="groove")
# npy.entry <- tkentry( npy.frm, textvariable=npy.value, width=3)

# Frame for 'rl.xlow' and 'rl.xup' args.
rl.range.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")

rl.xlow.frm <- tkframe( rl.range.frm, borderwidth=2, relief="flat")
rl.xlow.entry <- tkentry( rl.xlow.frm, textvariable=rl.xlow.value, width=5)

rl.xup.frm <- tkframe( rl.range.frm, borderwidth=2, relief="flat")
rl.xup.entry <- tkentry( rl.xup.frm, textvariable=rl.xup.value, width=5)

# Frame for 'xi.xlow' and 'xi.xup' args.
xi.range.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")

xi.xlow.frm <- tkframe( xi.range.frm, borderwidth=2, relief="flat")
xi.xlow.entry <- tkentry( xi.xlow.frm, textvariable=xi.xlow.value, width=5)

xi.xup.frm <- tkframe( xi.range.frm, borderwidth=2, relief="flat")
xi.xup.entry <- tkentry( xi.xup.frm, textvariable=xi.xup.value, width=5)

conf.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
conf.entry <- tkentry( conf.frm, textvariable=conf.value, width=5)

nint.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
nint.entry <- tkentry( nint.frm, textvariable=nint.value, width=5)

tkpack( tklabel( m.frm, text="m-year return level", padx=4), m.entry,
	side="left", anchor="w")
# tkpack( tklabel( npy.frm, text="Number of obs. per year", padx=4), npy.entry, side="left", anchor="e")
# tkpack( m.frm, npy.frm, side="left")
tkpack( m.frm)

do.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
rl.button <- tkcheckbutton(do.frm, text="Return Level", variable=do.rl)
tkpack( rl.button, side="left")
xi.button <- tkcheckbutton( do.frm, text="Shape Parameter (xi)", variable=do.xi)
tkpack( xi.button, side="left")
tkpack( do.frm)

tkpack( tklabel( rl.xlow.frm, text="Lower limit", padx=4), rl.xlow.entry,
 	side="left")
tkpack( tklabel( rl.xup.frm, text="Upper limit", padx=4), rl.xup.entry,
 	side="left")
tkpack( rl.xup.frm, side="bottom")
tkpack( rl.xlow.frm, side="bottom")

tkpack( tklabel( xi.xlow.frm, text="Lower limit", padx=4), xi.xlow.entry,
        side="left")
tkpack( tklabel( xi.xup.frm, text="Upper limit", padx=4), xi.xup.entry,
        side="left")
tkpack( xi.xup.frm, side="bottom")
tkpack( xi.xlow.frm, side="bottom")

# Pack together the search ranges for both return level and shape parameter.
tkpack( tklabel( rl.range.frm, text="Return Level Search Range", padx=4),
	tklabel( rl.range.frm, text="(leave blank to try to find automatically)", padx=4), side="top")
tkpack( tklabel( xi.range.frm, text="Shape Parameter (xi) Search Range", padx=4),
	tklabel( xi.range.frm, text="(leave blank to try to find automatically)", padx=4), side="top")
tkpack( rl.range.frm, xi.range.frm, side="left")

tkpack( tklabel( conf.frm, text="Confidence Value", padx=4), conf.entry,
	side="left", fill="x", anchor="w")
tkpack( conf.frm)

tkpack( tklabel( nint.frm, text="nint", padx=4), nint.entry,
	side="left", fill="x")
tkpack( nint.frm)

# create check button for plotting profile likelihoods (or not)

makeplot.but <- tkcheckbutton(mid.frm,text="Plot profile likelihoods",variable=makeplot)
tkpack( makeplot.but, side="left")

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=gpdprofhelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", gpdprofhelp)

tkpack( top.frm, side="top")
tkpack( extras.frm, mid.frm, side="top", fill="x")
tkpack( bot.frm, side="bottom")
} # end of fcn
