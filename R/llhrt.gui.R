llhrt.gui <- function( base.txt) {

alpha.val <- tclVar("0.05")

# Refresh fcn 
refresh <- function() {
	tkdelete( fit1.listbox, 0.0, "end")
	tkdelete( fit2.listbox, 0.0, "end")
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd <- get( full.list[ data.select])
                } else stop("llhrt.gui: Must load a data object!")
	models.fit <- names( dd$models)
	for( i in 1:length( models.fit)) {
		tkinsert( fit1.listbox, "end", paste( models.fit[i]))
		tkinsert( fit2.listbox, "end", paste( models.fit[i]))
		}
	invisible()
} # end of refresh fcn.

submit <- function() {
	alpha <- as.numeric( tclvalue( alpha.val))
	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd <- get( full.list[ data.select])
                } else stop("llhrt.gui: Must load a data object!")
	fit1.select <- as.numeric( tkcurselection( fit1.listbox))+1
	fit2.select <- as.numeric( tkcurselection( fit2.listbox))+1
	m0 <- dd$models[[ fit1.select]]
	n0 <- length( m0$mle)
	m1 <- dd$models[[ fit2.select]]
	n1 <- length( m1$mle)
	df <- abs( n1 - n0)
	if( n0 > n1) {
		m2 <- m0
		m3 <- m1
		m0 <- m3
		m1 <- m2
		} # end of if n0 > n1 stmt
	out <- deviancestat(	l1=m0$nllh,
				l2=m1$nllh,
				v=df,
				alpha=alpha)
	tkconfigure( base.txt, state="normal")
	nl1 <- paste(" ", "**************", " ", sep="\n")
	nl2 <- paste(" ", " ", sep="\n")
	msg1 <- paste("Likelihood-ratio test statistic for models: M0 = ", names( dd$models)[fit1.select],
			" and M1 = ", names( dd$models)[fit2.select], " is: ", sep="")
	if( abs( out$DS) > out$c.alpha)
	msg2 <- paste( round( abs( out$DS), digits=4), " > ",
				round( out$c.alpha, digits=4),
				"= 1 - ", alpha, " quantile of",
				" a Chi-square with ", df,
				" degrees of freedom.", sep="")
	else msg2 <- paste( round( abs( out$DS), digits=4), " <= ",
				round( out$c.alpha, digits=4),
				" = 1 - ", alpha, " quantile of ",
				"a Chi-square with ", df,
				" degrees of freedom.", sep="")
	msg3 <- paste(" p-value = ", round( out$p.value, digits=6), sep="")
	tkinsert( base.txt, "end", nl1)
	tkinsert( base.txt, "end", msg1)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg2)
	tkinsert( base.txt, "end", nl1)
	tkinsert( base.txt, "end", msg3)
	tkinsert( base.txt, "end", nl1)
	tkyview.moveto( base.txt, 1.0)
	tkconfigure( base.txt, state="disabled")
	invisible()
} # end of submit fcn.

devhelp <- function() {
	tkconfigure( base.txt, state="normal")
	nl1 <- paste(" ", "**************", " ", sep="\n")
        nl2 <- paste(" ", " ", sep="\n")
	tkinsert( base.txt, "end", nl1)
	h1 <- paste( "Simply computes the likelihood-ratio test", "D=2*log(M1/M0)",
			"where M0 contained in M1 are the likelihood functions.",
			"Also computed is the 1-alpha quantile of",
			"a Chi-squared distribution with degrees of freedom",
			"equal to the difference in the number of parameters of M0 and M1.", sep="\n")
	tkinsert( base.txt, "end", h1)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", nl1)
	tkyview.moveto( base.txt, 1.0)
	tkconfigure( base.txt, state="disabled")
	} # end of devhelp fcn

endprog <- function() {
	tkdestroy( base)
	}
# Function to compare two fits M0 and M1, where M0 is contained in M1.

#####################
# Frame/button setup.
#####################

base <- tktoplevel()
tkwm.title( base, "Likelihood-ratio test for comparing nested fits")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
d.frm <- tkframe( top.frm, borderwidth=2, relief="groove")
alpha.frm <- tkframe( top.frm, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

data.listbox <- tklistbox( d.frm,
                        yscrollcommand=function(...) tkset( data.scroll, ...),
                        selectmode="single",
                        width=20,
                        height=5,
                        exportselection=0)

data.scroll <- tkscrollbar( d.frm, orient="vert",
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

tkpack( tklabel( d.frm, text="Data Object:  ", padx=4), side="top")
tkpack( data.listbox, data.scroll,  side="left", fill="y")

# Place bindings on data listbox to update fit listbox.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# alpha frame
alpha.entry <- tkentry( alpha.frm, textvariable=alpha.val, width=5)
tkpack( tklabel( alpha.frm, text="significance level (alpha)", padx=4), alpha.entry, side="left")
tkpack( d.frm, alpha.frm, side="left")

# Middle frame for choosing which fits to compare.
midleft.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
fit1.listbox <- tklistbox( midleft.frm,
			yscrollcommand=function(...) tkset( fit1.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

fit1.scroll <- tkscrollbar( midleft.frm, orient="vert",
			command=function(...) tkyview( fit1.listbox, ...))
tkinsert( fit1.listbox, "end", "")

tkpack( tklabel( midleft.frm, text="Select base fit (M0): ", padx=4), side="top")
tkpack( fit1.listbox, fit1.scroll, side="left", fill="y")

midright.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
fit2.listbox <- tklistbox( midright.frm,
                        yscrollcommand=function(...) tkset( fit2.scroll, ...),
                        selectmode="single",
                        width=20,
                        height=5,
                        exportselection=0)

fit2.scroll <- tkscrollbar( midright.frm, orient="vert",
                        command=function(...) tkyview( fit2.listbox, ...))
tkinsert( fit2.listbox, "end", "")

tkpack( tklabel( midright.frm, text="Select comparison fit (M1): ", padx=4), side="top")
tkpack( fit2.listbox, fit2.scroll, side="left", fill="y")

tkpack( midleft.frm, midright.frm, side="left", fill="x")

# Bottom frame for execution and cancellation.

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=devhelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings on buttons.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", devhelp)

tkpack( top.frm, mid.frm, bot.frm, side="top", fill="x")
invisible()


} # end of fcn
