decluster.gui <- function(base.txt) {

# This function provides a gui interface for declustering a dataset.
# The gui will list all objects of class
# "extRemesDataObject" and the column names of the user selected object.
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
		} else dd <- extRemesData

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
	dd.cmd <- paste( "dd <- get( \"", full.list[ data.select], "\")", sep="")
	# M <- dim( dd$data)[1]
	} else dd.cmd <- "dd <- extRemesData"
eval( parse( text=dd.cmd))
write( dd.cmd, file="extRemes.log", append=TRUE)

M.cmd <- "M <- dim( dd[[\"data\"]])[1]"
eval( parse( text=M.cmd))
write( M.cmd, file="extRemes.log", append=TRUE)

# Grab data to decluster.
cols.selected.cmd <- "cols.selected <- character(0)"
eval( parse( text=cols.selected.cmd))
write( cols.selected.cmd, file="extRemes.log", append=TRUE)

# Gather column names to add decluster to end.
cnames.cmd <- "cnames <- colnames( dd[[\"data\"]])"
eval( parse( text=cnames.cmd))
write( cnames.cmd, file="extRemes.log", append=TRUE)

temp <- as.numeric( tkcurselection( col.listbox)) + 1
cols.selected.cmd <- paste( "cols.selected <- c( cols.selected, \"", cnames[temp], "\")", sep="")
eval( parse( text=cols.selected.cmd))
write( cols.selected.cmd, file="extRemes.log", append=TRUE)

# Make sure a column has been selected.
if( is.na( temp)) return()
# for( i in 1:length( temp)) {
# 	cols.selected.cmd <- paste( "cols.selected <- c( cols.selected, ", cnames[temp[i]], sep="")
# 	eval( parse( text=cols.selected.cmd))
# 	write( cols.selected.cmd, file="extRemes.log", append=TRUE)
# 	} # end of for 'i' loop.

# Grab column to decluster by...
temp2 <- as.numeric( tkcurselection( clustby.listbox))+1
if( length( temp2) > 0) {
	cluster.by.col.cmd <- paste( "cluster.by.col <- cnames[", temp2, "]", sep="")
	eval( parse( text=cluster.by.col.cmd))
	write( cluster.by.col.cmd, file="extRemes.log", append=TRUE) 
	cluster.by.val.cmd <- "cluster.by.val <- dd[[\"data\"]][, cluster.by.col]"
} else cluster.by.val.cmd <- "cluster.by.val <- NULL"
eval( parse( text=cluster.by.val.cmd))
write( cluster.by.val.cmd, file="extRemes.log", append=TRUE)

r.val <- as.numeric( tclvalue( r.value))
tmp.data.cmd <- "tmp.data <- dd[[\"data\"]][ ,cols.selected]"
# tmp.data.cmd <- paste( "tmp.data <- ", full.list[ data.select], "$data[,cols.selected]", sep="")
eval( parse( text=tmp.data.cmd))
write( tmp.data.cmd, file="extRemes.log", append=TRUE)

# Draw a plot with horizontal line through threshold (if desired).
if( tclvalue( add.plot.value)==1) {
	plotCMD <- paste( "plot( 1:", length(tmp.data),
			", tmp.data, xaxt=\"n\", xlab=\"obs\", ylab=cols.selected, pch=\".\")", sep="")
	eval( parse( text=plotCMD))
	write( plotCMD, file="extRemes.log", append=TRUE)
	}
# Obtain the threshold values (is it a number or a name?).
if( !is.na( as.numeric( tclvalue( threshold.value)))) {
	threshold.val <- as.numeric( tclvalue( threshold.value))
	thresh.name <- as.character( threshold.val)
	if( is.null( cluster.by.val)) newnames <- paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", sep="")
	else newnames <- paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", "by", cnames[temp2], sep="")
} else { 
	threshold.val <- get( tclvalue( threshold.value))
	thresh.name <- tclvalue( threshold.value)
	if( is.null( cluster.by.val)) newnames <- paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", sep="")
	else newnames <- paste( cnames[temp], ".u",thresh.name, "r", r.val, "dc", "by", cnames[temp2], sep="")
	} # end of if else 'threshold.value' is a number or a name stmts.

	## Make sure this new decluster doesn't delete an existing column.
	## The assumption is that if the column exists, then the procedure
	## has already been performed, but this may not be the case.
	for( i in 1:length( cnames)) {
        if( newnames == cnames[i]) {
		warn.msg <- paste(" ", "***************", "Warning: ",
				"This declustering procedure appears to have already been performed.",
				"No declustering applied.  Maybe change column name.", "***************", " ", sep="\n")
                # tkconfigure( base.txt, state="normal")
                # tkinsert( base.txt, "end", warn.msg)
                # tkconfigure( base.txt, state="disabled")
                stop(warn.msg)
                } # end of if stmt
        } # end of for i loop

#	ind <- tmp.data > threshold.val
#	NN <- sum( ind)
	# out <- dclust( xdat=tmp.data, u=threshold.val, r=r.val, cluster.by=cluster.by.val)
	print( paste( "Declustering ...", sep=""))
	# print( paste( "This may take a long time for large datasets!", sep=""))
	if( !is.null( cluster.by.val) ) out.cmd <- paste( "out <- dclust( xdat=tmp.data, u=", threshold.val, ", r=", r.val,
	 		", cluster.by=cluster.by.val)", sep="")
	else out.cmd <- paste( "out <- decluster.runs( z=tmp.data > ", threshold.val, ", r=", r.val, ")", sep="")
	eval( parse( text=out.cmd))
	write( out.cmd, file="extRemes.log", append=TRUE)
	if( !is.null( cluster.by.val)) ncluster.cmd <- "ncluster <- out[[\"ncluster\"]]"
	else ncluster.cmd <- "ncluster <- out[[\"nc\"]]"
	eval( parse( text=ncluster.cmd))
	write( ncluster.cmd, file="extRemes.log", append=TRUE)
	nexc.cmd <- paste( "nexc <- sum( tmp.data > ", threshold.val, ", na.rm=TRUE)", sep="")
	eval( parse( text=nexc.cmd))
	write( nexc.cmd, file="extRemes.log", append=TRUE)
	thetar.cmd <- "theta.r <- ncluster/nexc"
	eval( parse( text=thetar.cmd))
	write( thetar.cmd, file="extRemes.log", append=TRUE)
	# out2.cmd <- paste( "out2 <- extremalindex( tmp.data, ", threshold.val, ")", sep="")
	# cat("\n", "Estimating the extremal index ... (please stand by).\n")
	# out2.cmd <- paste("out2 <- eiAnalyze( x=tmp.data, thresholds=", threshold.val, ")", sep="")
	# eval( parse( text=out2.cmd))
	# write( out2.cmd, file="extRemes.log", append=TRUE)
	if( !is.null( cluster.by.val)) {
		tmp.dc.cmd <- "tmp.dc <- out[[\"xdat.dc\"]]"
		eval( parse( text=tmp.dc.cmd))
		write( tmp.dc.cmd, file="extRemes.log", append=TRUE)
	} else {
		tmp.dc.cmd <- "tmp <- as.numeric( tapply( tmp.data[out[[\"s\"]] ], out[[\"cluster\"]], max))"
		eval( parse( text=tmp.dc.cmd))
		write( tmp.dc.cmd, file="extRemes.log", append=TRUE)
		tmp.dc.cmd <- paste("tmp.dc <- rep( min(0,", threshold.val, "-1), length( tmp.data))", sep="")
		eval( parse( text=tmp.dc.cmd))
		write( tmp.dc.cmd, file="extRemes.log", append=TRUE)
		# clust.ind.cmd <- "clust.ind <- numeric(length( tmp.data))+NA"
		# eval( parse( text=clust.ind.cmd))
		# write( clust.ind.cmd, file="extRemes.log", append=TRUE)
		CMD <- paste("xdatu <- tmp.data[ tmp.data > ", threshold.val, "]", sep="")
		eval( parse( text=CMD))
		write( CMD, file="extRemes.log", append=TRUE)
		CMD <- "s <- out[[\"s\"]]"
		eval( parse( text=CMD))
		write( CMD, file="extRemes.log", append=TRUE)
		CMD <- "c <- out[[\"cluster\"]]"
                eval( parse( text=CMD))
		write( CMD, file="extRemes.log", append=TRUE)
		# for( i in 1:max(out$cluster)) {
#   CMD <- paste( "for( i in 1:max( c)) tmp.dc[ min( s[ xdatu[i==c] == max( xdatu[i==c])])] <- unique( max(xdatu[i==c]))",
#			sep="")
CMD <- "for( i in 1:max( c)) tmp.dc[ min(s[ (i==c) & (xdatu == unique( max( xdatu[i==c]))) ])] <- unique( max( xdatu[i==c]))"
		eval( parse( text=CMD))
		write( CMD, file="extRemes.log", append=TRUE)
		# 	} # end of for 'i' loop.
	} # end of ifelse 'cluster.by.val' stmts.

#c(out$xdat.dc,
# rep( min( min( tmp.data, na.rm=TRUE), threshold.val-1), M-ncluster))
if( tclvalue( add.plot.value)==1) {
	abline.cmd1 <- paste( "abline( h=", threshold.val, ")", sep="")
	eval( parse( text=abline.cmd1))
	write( abline.cmd1, file="extRemes.log", append=TRUE)
	abline.cmd2 <- paste( "abline( v=(1:", length(tmp.data), ")[diff( out[[\"clust\"]]) >= 1]+0.5, lty=2, col=\"red\")",
				sep="")
	eval( parse( text=abline.cmd2))
	write( abline.cmd2, file="extRemes.log", append=TRUE)
	}
cnames.cmd <- paste( "cnames <- c( cnames, \"", newnames, "\")", sep="")
eval( parse( text=cnames.cmd))
write( cnames.cmd, file="extRemes.log", append=TRUE)
# cnames.msg <- character(0)
# for( i in 1:length( cnames)) {
# 	if( i==1) cnames.msg <- paste( cnames.msg, "\"", cnames[i], "\"", sep="")
# 	else cnames.msg <- paste( cnames.msg, ", ", "\"", cnames[i], "\"", sep="")
# 	} # end of for 'i' loop.
# cnames.msg <- paste("cnames <- c( ", cnames.msg, ")", sep="")
# write( cnames.msg, file="extRemes.log", append=TRUE)
dd.cmd <- "dd[[\"data\"]] <- cbind( dd[[\"data\"]], tmp.dc)"
eval( parse( text=dd.cmd))
write( dd.cmd, file="extRemes.log", append=TRUE)
colnames.cmd <- "colnames( dd[[\"data\"]]) <- cnames"
eval( parse( text=colnames.cmd))
write( colnames.cmd, file="extRemes.log", append=TRUE)
assignCMD <- paste( "assign( \"", full.list[ data.select], "\", dd, pos=\".GlobalEnv\")", sep="")
eval( parse( text=assignCMD))
write( assignCMD, file="extRemes.log", append=TRUE)
# colnames.cmd <- paste( "colnames( ", full.list[ data.select], "$data) <- cnames", sep="")

# tkconfigure( base.txt, state="normal")
# nl1 <- paste(" ", "**********************", " ", sep="\n")
# nl2 <- paste(" ", " ", sep="\n")
msg1 <- paste( "declustering performed for:", sep="")
msg2 <- paste( cols.selected, " and assigned to ", newnames, sep="")
if( length( threshold.val) == 1) msg3 <- paste( out$nc, " clusters using threshold of ", threshold.val, " and r = ", r.val, sep="")
else msg3 <- paste( out$nc, " clusters using threshold of ", thresh.name, " and r = ", r.val, sep="")
# msg4 <- paste("Extremal index (theta) estimated from runs declustering: ", round( theta.r, digits=5), sep="")
# msg5 <- paste("Extremal index (theta) estimated from intervals estimator: ", round( out2$ei, digits=5), sep="")
# msg5b <- paste("95% CI for intervals estimator (from bootstrapping): (", round( out2$ci[,1], digits=4), ",",
 # 		round( out2$ci[,2], digits=4), ")", sep="")
# msg6 <- paste("Estimated run length (Ferro and Segers (2003)): ", out2$run.length, sep="")
# msg7 <- out2$msg
# tkinsert( base.txt, "end", nl1)
# tkinsert( base.txt, "end", msg1)
print(msg1)
# tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg2)
print( msg2)
# tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg3)
print( msg3)
# tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg4)
# print( msg4)
# tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg5)
# print( msg5)
# tkinsert( base.txt, "end", nl2)
# # tkinsert( base.txt, "end", msg5b)
# print( msg5b)
# # tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg6)
# print( msg6)
# tkinsert( base.txt, "end", nl2)
# tkinsert( base.txt, "end", msg7)
# print( msg7)
tkdestroy( base)
# tkconfigure( base.txt, state="disabled")
invisible()
	} # end of submit fcn

dchelp <- function() {
	# tkconfigure( base.txt, state="normal")
	nl1 <- paste("*******************", sep="")
help.msg <- paste( " ", "Runs declustering", "",
"This is a simple function that takes a data set X and declusters",
"X based on a given threshold and the number of subsequent obs that",
"fall below the threshold.  That is, once an observation exceeds a", 
"threshold (say u) a cluster begins.  After r observations fall below",
"u, that cluster terminates and a new one begins.  The",
"maximum of each of these clusters is then returned to the data object",
"with a new data column terminating with a .u[u]r[r]dc extension.  The rest of",
"the new column is filled with the lesser of the minimum of the original data",
"or one less than the chosen threshold for dimensional compatibility.",
"","Also displays estimates for the extremal index based on both n_c/N,",
"where N is the number of exceedances of the threshold and n_c are the number",
"of clusters found from runs declustering, and on the intervals estimator",
"from Ferro and Segers (2003) (see the help file for extremalindex)", "",
"See Coles (2001) for more information on runs declustering.",
"See the help file for \'dclust\' (i.e., \'help( dclust)\') as this is the primary ",
"function utilized.", " ", sep="\n")
	# tkinsert( base.txt, "end", nl1)
	# tkinsert( base.txt, "end", help.msg)
	# tkinsert( base.txt, "end", nl1)
	# tkconfigure( base.txt, state="disabled")
	cat( nl1)
	cat( help.msg)
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
        if( (class(get( temp[i]))[1] == "extRemesDataObject")) {
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
for( i in 1:ncol( extRemesData$data))
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
for( i in 1:ncol( extRemesData$data))
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
