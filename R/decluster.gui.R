decluster.gui <- function(base.txt) {

# This function provides a gui interface for declustering a dataset.
# The gui will list all objects of class
# "ev.data" and the column names of the user selected object.
# After declustering the selected data, will return a new column
# to the data.  
# Declustered data will keep the same column name as the original
# clustered column, but with a ".dc" extension.

# Set the tcl variables

threshold.value <- tclVar("")
r.value <- tclVar("1")
add.plot.value <- tclVar(0)

# Internal functions

refresh <- function() {

# When data is selected, this function fills the lists for the columns of
# the data set so that the user can select which column(s) to decluster.

	tkdelete( col.listbox, 0.0, "end")
	tkdelete( clustby.listbox, 0.0, "end")

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else dd <- .ev

	for( i in 1:ncol( dd$data)) {
		tkinsert( col.listbox, "end", paste( colnames( dd$data)[i])) 
		tkinsert( clustby.listbox, "end", paste( colnames( dd$data)[i]))
		}
	invisible()
	} # end of refresh fcn

submit <- function() {
# Function invoked when the "ok" button is pressed.  Actually declusters
# the data.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	M <- dim( dd$data)[1]
	} else dd <- .ev

# Grab data to decluster.
cols.selected <- character(0)
# Gather column names to add decluster to end.
cnames <- colnames( dd$data)
temp <- as.numeric( tkcurselection( col.listbox)) + 1
# Make sure a column has been selected.
if( is.na( temp)) return()
cols.selected <- cnames[temp]

# Grab column to decluster by...
temp2 <- as.numeric( tkcurselection( clustby.listbox))+1
if( length( temp2) > 0) {
	cluster.by.col <- cnames[temp2]
	cluster.by.val <- dd$data[, cluster.by.col]
} else cluster.by.val <- NULL

r.val <- as.numeric( tclvalue( r.value))
tmp.data <- dd$data[ ,cols.selected]

# Draw a plot with horizontal line through threshold (if desired).
if( tclvalue( add.plot.value)==1) plot( 1:length(tmp.data), tmp.data, xaxt="n", xlab="obs", ylab=cols.selected, pch=".")
	if( !is.na( as.numeric( tclvalue( threshold.value)))) {
		threshold.val <- as.numeric( tclvalue( threshold.value))
		thresh.name <- as.character( threshold.val)
if( is.null( cluster.by.val)) newnames <-
	paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", sep="")
else newnames <-
	paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", "by",
							cnames[temp2], sep="")
	} else { 
		threshold.val <- get( tclvalue( threshold.value))
		thresh.name <- tclvalue( threshold.value)
if( is.null( cluster.by.val)) newnames <-
        paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", sep="")
else newnames <-
	paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", "by",
							cnames[temp2], sep="")
		}

	for( i in 1:length( cnames)) {
        if( newnames == cnames[i]) {
warn.msg <- paste(" ", "***************", "Warning: ",
"This declustering procedure appears to have already been performed.",
"No declustering applied.  Maybe change column name.", "***************", " ",
sep="\n")
                tkconfigure( base.txt, state="normal")
                tkinsert( base.txt, "end", warn.msg)
                tkconfigure( base.txt, state="disabled")
                stop("Decluster already exists.")
                } # end of if stmt
        } # end of for i loop

#	ind <- tmp.data > threshold.val
#	NN <- sum( ind)
	out <- dclust( xdat=tmp.data, u=threshold.val, r=r.val, cluster.by=cluster.by.val)
	ncluster <- out$ncluster
	nexc <- sum( tmp.data > threshold.val, na.rm=TRUE)
	theta.r <- ncluster/nexc
	Ferro <- extremalindex( tmp.data, threshold.val)
	tmp.dc <- out$xdat.dc
#c(out$xdat.dc,
# rep( min( min( tmp.data, na.rm=TRUE), threshold.val-1), M-ncluster))
if( tclvalue( add.plot.value)==1) {
	abline( h=threshold.val)
abline( v=(1:length(tmp.data))[diff( out$clust)>=1]+0.5, lty=2, col="red")
	}
cnames <- c( cnames, newnames)
dd$data <- cbind( dd$data, tmp.dc)
colnames( dd$data) <- cnames
assign( full.list[ data.select], dd, pos=".GlobalEnv")

tkconfigure( base.txt, state="normal")
nl1 <- paste(" ", "**********************", " ", sep="\n")
nl2 <- paste(" ", " ", sep="\n")
msg1 <- paste( "declustering performed for:", sep="")
msg2 <- paste( cols.selected, " and assigned to ", newnames, sep="")
if( length( threshold.val) == 1) msg3 <- paste( ncluster, " clusters using threshold of ", threshold.val, " and r = ", r.val, sep="")
else msg3 <- paste( ncluster, " clusters using threshold of ", thresh.name, " and r = ", r.val, sep="")
msg4 <- paste("Extremal index (theta) estimated from runs declustering: ",
		round( theta.r, digits=5), sep="")
msg5 <- paste("Extremal index (theta) estimated from intervals estimator: ",
		round( Ferro$theta, digits=5), sep="")
# msg5b <- paste("95% CI for intervals estimator (from bootstrapping): (",
# 		round( Ferro$CI.theta[1], digits=4), ",",
# 		round( Ferro$CI.theta[2], digits=4), ")", sep="")
msg6 <- paste("Estimated run length (Ferro and Segers (2003)): ",
			Ferro$run.length, sep="")
msg7 <- Ferro$msg
tkinsert( base.txt, "end", nl1)
tkinsert( base.txt, "end", msg1)
tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg2)
tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg3)
tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg4)
tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg5)
tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg5b)
# tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg6)
tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", msg7)
tkdestroy( base)
tkconfigure( base.txt, state="disabled")
invisible()
	} # end of submit fcn

dchelp <- function() {
	tkconfigure( base.txt, state="normal")
	nl1 <- paste(" ", "*******************", " ", sep="\n")
help.msg <- paste( " ", "Runs declustering", "",
"This is a simple function that takes a data set X and declusters",
"X based on a given threshold and the number of subsequent obs that",
"fall below the threshold.  That is, once an observation exceeds a", 
"threshold (say u) a cluster begins.  After r observations fall below",
"u\, that cluster terminates and a new one begins.  The",
"maximum of each of these clusters is then returned to the data object",
"with a new data column terminating with a .u[u]r[r]dc extension.  The rest of",
"the new column is filled with the lesser of the minimum of the original data",
"or one less than the chosen threshold for dimensional compatibility.",
"","Also displays estimates for the extremal index based on both n_c/N,",
"where N is the number of exceedances of the threshold and n_c are the number",
"of clusters found from runs declustering, and on the intervals estimator",
"due to Ferro and Segers (2003) (see the help file for extremalindex)", "",
"See Coles (2001) for more information on runs declustering.", sep="\n")
	tkinsert( base.txt, "end", nl1)
	tkinsert( base.txt, "end", help.msg)
	tkinsert( base.txt, "end", nl1)
	tkconfigure( base.txt, state="disabled")
	invisible()
	} # end of lthelp

endprog <- function() {
	tkdestroy( base)
	}

#####################
# Frame/button setup
#####################

base <- tktoplevel()
tkwm.title( base, "Declustering: Run Length")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
data.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
clustby.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
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

# leftmid.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")
col.listbox <- tklistbox( data.frm,
			yscrollcommand=function(...) tkset( col.scroll, ...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

col.scroll <- tkscrollbar( data.frm, orient="vert",
			command=function(...) tkyview( col.listbox, ...))

if( is.nothing) {
for( i in 1:ncol( .ev$data))
	tkinsert( col.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
	} else tkinsert( col.listbox, "end", "")

tkpack( tklabel( data.frm, text="Variable to Decluster", padx=4), side="top")
# tkpack( col.listbox, side="left")
# tkpack( col.scroll, side="right")
tkpack( col.listbox, col.scroll, side="left", fill="y")

# cluster by frame...
clustby.listbox <- tklistbox( clustby.frm,
                        yscrollcommand=function(...) tkset(clustby.scroll, ...),
                        selectmode="single",
                        width=20,
                        height=5,
                        exportselection=0)
clustby.scroll <- tkscrollbar( clustby.frm, orient="vert",
                        command=function(...) tkyview( clustby.listbox, ...))
if( is.nothing) {
for( i in 1:ncol( .ev$data))
        tkinsert( clustby.listbox, "end", paste( colnames( dd$data)[i]))
# end of for i loop
        } else tkinsert( col.listbox, "end", "")
tkpack( tklabel( clustby.frm, text="Decluster by", padx=4), side="top")
tkpack( clustby.listbox, clustby.scroll, side="left", fill="y")

tkpack( data.frm, clustby.frm, side="left", fill="x")

# Select a threshold.
rightmid.frm <- tkframe( mid.frm, borderwidth=2, relief="groove")

threshold.frm <- tkframe( rightmid.frm, borderwidth=2, relief="flat")
threshold.entry <- tkentry( threshold.frm, textvariable=threshold.value,
			width=10)
tkpack( threshold.entry, tklabel( threshold.frm, text="Threshold(s)", padx=4), side="right")

# Select r.
r.frm <- tkframe( rightmid.frm, borderwidth=2, relief="groove")
r.entry <- tkentry( r.frm, textvariable=r.value, width=4)
tkpack( r.entry, tklabel( r.frm, text="Run Length (r)", padx=4), side="right")

# Add plot checkbutton...
add.plot.checkbutton <- tkcheckbutton( rightmid.frm, text="Plot data",
				variable=add.plot.value)
tkpack( threshold.frm, r.frm, add.plot.checkbutton, side="top")
tkpack( rightmid.frm, fill="x")

ok.but <- tkbutton( bot.frm, text="OK", command=submit)
cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)

help.but <- tkbutton( bot.frm, text="Help", command=dchelp)

tkpack( ok.but, cancel.but, side="left")
tkpack( help.but, side="right")

# place bindings for return key.
tkbind( ok.but, "<Return>", submit)
tkbind( cancel.but, "<Return>", endprog)
tkbind( help.but, "<Return>", dchelp)

tkpack( top.frm, fill="x")
tkpack( mid.frm, fill="x")
tkpack( bot.frm, side="bottom")
} # end of decluster.gui fcn
