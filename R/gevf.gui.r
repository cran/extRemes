"gevf.gui" <-
function(base.txt) {

#
# This function provides a gui for Coles
# gev.fit function and helpers
#
#



#  Set the tcl variables
plot.diags<-tclVar(0)
mu.link<-tclVar("identity")
sig.link<-tclVar("identity")
gam.link<-tclVar("identity")

#########################################
# internal functions
#########################################

refresh <- function() {

# when a data object is chosen, this function will reset the various lists
# to have the correct covariates, etc... (at least in theory)

	tkdelete( resp.listbox, 0.0, "end")
	tkdelete( mu.covlist, 0.0, "end")
	tkdelete( sig.covlist, 0.0, "end")
	tkdelete( gam.covlist, 0.0, "end")

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
		} else dd <- extRemesData

	for( i in 1:ncol(dd$data))
        	tkinsert( resp.listbox, "end",
			paste( colnames( dd$data)[i]))
		# end of for i loop

	for (i in 1:ncol(dd$data))
        	tkinsert(mu.covlist,"end",
			paste(colnames(dd$data)[i]))
		# end of for i loop

	for (i in 1:ncol(dd$data))
        	tkinsert(sig.covlist,"end",
			paste(colnames(dd$data)[i]))
		# end of for i loop

	for (i in 1:ncol(dd$data))
        	tkinsert(gam.covlist,"end",
			paste(colnames(dd$data)[i]))
		# end of for i loop
	} # end of refresh fcn

redolists<-function() {
# When a response variable is selected, this function eliminates it
#  as a covariate option from the other lists

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
	} else dd <- extRemesData

resp.name <-
	colnames(dd$data)[as.numeric(tkcurselection(resp.listbox))+1] 

    # put the correct eligible covariates in the other list boxes
    tkdelete(mu.covlist, 0.0,"end")
    tkdelete(sig.covlist,0.0,"end")
    tkdelete(gam.covlist,0.0,"end")

	for (i in colnames(dd$data)) {
		if (i != resp.name) {
        		tkinsert(mu.covlist,"end",i)
        		tkinsert(sig.covlist,"end",i)
        		tkinsert(gam.covlist,"end",i)
      		} # end of if i != resp.name stmt
 
	} # end of for i loop    
} # end of redolists fcn

submit <- function() {
    #
    # The meat of this program.  Actually fits the gev to the data.
    #

	if( !is.nothing) {
                data.select <- as.numeric( tkcurselection( data.listbox))+1
                dd.cmd <- paste("dd <- get( \"", full.list[ data.select], "\")", sep="")
                } else dd.cmd <- "dd <- extRemesData"
	eval( parse( text=dd.cmd))
	write( dd.cmd, file="extRemes.log", append=TRUE)

    # names of the covariates used (if any) 
    cov.names.cmd <- "cov.names<-character(0)"
	eval( parse( text=cov.names.cmd))
	write( cov.names.cmd, file="extRemes.log", append=TRUE)

    resp.select<-as.numeric(tkcurselection(resp.listbox))+1
 
    # make sure that a response was selected
    if (is.na(resp.select))
      return()


	# tkconfigure(base.txt,state="normal")
	nl1 <- paste("  ", "************", "   ", "   ", sep="\n")
	nl2 <- paste( "   ", " ", sep="\n")
	# tkinsert( base.txt, "end", nl2)
	cat( nl2)
	# tkinsert( base.txt, "end", nl1)
	cat( nl1)
	# tkinsert( base.txt,"end",paste("GEV fit \n"))
	cat( paste("GEV fit \n"))
	# tkinsert(base.txt,"end",paste("-----------------------------------\n"))
	cat( paste("-----------------------------------\n"))
	# tkinsert(base.txt,"end",paste("Response variable:",
 	 #            colnames( dd$data)[ resp.select], "\n"))
	cat( paste("Response variable:", colnames( dd$data)[ resp.select], "\n"))
	# tkinsert( base.txt, "end", nl2)
	cat( nl2)

    # process the covariates and link functions
	covs.cmd <- "covs <- NULL"
	eval( parse( text=covs.cmd))
	write( covs.cmd, file="extRemes.log", append=TRUE)
	cur.cov.cols<-0
	# eval( parse( text=cur.cov.cols.cmd))
	# write( cur.cov.cols.cmd, file="extRemes.log", append=TRUE)

	# do the mu
	mu.cov.cols.cmd <- "mu.cov.cols <- NULL"
	eval( parse( text=mu.cov.cols.cmd))
	write( mu.cov.cols.cmd, file="extRemes.log", append=TRUE)

	if( tclvalue( tkcurselection(mu.covlist)) !="") {

      # get the right covariates
     # temp.cols <- as.numeric( strsplit(tkcurselection(mu.covlist)," ")[[1]])
temp.cols <- as.numeric( unlist( strsplit( tclvalue( tkcurselection(mu.covlist)), " ")))
	cov.selected.cmd <- "cov.selected <- character(0)"
	eval( parse( text=cov.selected.cmd))
	write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      for (i in temp.cols) {
        cov.selected.cmd <- paste( "cov.selected <- c( cov.selected, \"", tclvalue( tkget( mu.covlist, i)), "\")", sep="")
	eval( parse( text=cov.selected.cmd))
	write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      }
print(  paste( "cov.selected = ", cov.selected, sep=""))
      # match the covariate names to the cols of 'dd'
 
      dat.cols.cmd <- "dat.cols<-numeric(0)"
	eval( parse( text=dat.cols.cmd))
	write( dat.cols.cmd, file="extRemes.log", append=TRUE) 
      for (j in 1:length(colnames(dd$data))) {
        for (i in cov.selected) {
          if (i == colnames(dd$data)[j]) {
            dat.cols.cmd <- paste("dat.cols<-c(dat.cols,", j, ")", sep="")
		eval( parse( text=dat.cols.cmd))
		write( dat.cols.cmd, file="extRemes.log", append=TRUE)
          } 
        }
      }
 
      # covs <- cbind(covs,as.matrix(dd$data[,dat.cols]))
	covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
	eval( parse( text=covs.cmd))
	# covs.cmd <- paste( "covs <- cbind(covs, as.matrix(", full.list[ data.select], "$data[,dat.cols]))", sep="")
	write( covs.cmd, file="extRemes.log", append=TRUE)
      mu.cov.cols.cmd <- paste( "mu.cov.cols <- ", cur.cov.cols+1, ":", length(dat.cols)+cur.cov.cols, sep="")
	eval( parse( text=mu.cov.cols.cmd))
	write( mu.cov.cols.cmd, file="extRemes.log", append=TRUE)
      cur.cov.cols<-cur.cov.cols+length(dat.cols)
      # cov.names <- c( cov.names, colnames( dd$data)[ dat.cols])
	cov.names.cmd <- "cov.names <- c( cov.names, colnames( dd[[\"data\"]])[ dat.cols])"
	eval( parse( text=cov.names.cmd))
# cov.names.cmd <- paste( "cov.names <- c( cov.names, colnames( ", full.list[ data.select], "$data[ dat.cols]))", sep="")
	write( cov.names.cmd, file="extRemes.log", append=TRUE)
    }



    # do the sigma 
   
    sig.cov.cols.cmd <- "sig.cov.cols <- NULL"
	eval( parse( text=sig.cov.cols.cmd))
	write( sig.cov.cols.cmd, file="extRemes.log", append=TRUE)
    if (tclvalue( tkcurselection(sig.covlist)) !="") {

     # get the right covariates
     # temp.cols<-as.numeric(strsplit(tkcurselection(sig.covlist)," ")[[1]])
temp.cols <- as.numeric( unlist( strsplit( tclvalue( tkcurselection(sig.covlist)), " ")))
      cov.selected.cmd <- "cov.selected<-character(0)"
	eval( parse( text=cov.selected.cmd))
	write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      for (i in temp.cols) {
        cov.selected.cmd <- paste( "cov.selected <- c( cov.selected, \"", tclvalue( tkget(sig.covlist,i)), "\")", sep="")
	eval( parse( text=cov.selected.cmd))
	write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      }
 
      # match the covariate names to the cols of 'dd'
 
      dat.cols.cmd <- "dat.cols<-numeric(0)"
	eval( parse( text=dat.cols.cmd))
	write( dat.cols.cmd, file="extRemes.log", append=TRUE)
      for (j in 1:length(colnames(dd$data))) {
        for (i in cov.selected) {
          if (i == colnames(dd$data)[j]) {
            dat.cols.cmd <- paste( "dat.cols<-c(dat.cols,", j, ")", sep="")
		eval( parse( text=dat.cols.cmd))
        write( dat.cols.cmd, file="extRemes.log", append=TRUE)
          }
        }
      }


      # covs <- cbind(covs,as.matrix(dd$data[,dat.cols]))
	covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
	eval( parse( text=covs.cmd))
	# covs.cmd <- paste( "covs <- cbind(covs, as.matrix( ", full.list[ data.select], "$data[, dat.cols]))", sep="")
	write( covs.cmd, file="extRemes.log", append=TRUE)
      sig.cov.cols.cmd <- paste( "sig.cov.cols<-", (cur.cov.cols+1), ":", (length(dat.cols)+cur.cov.cols), sep="")
	eval( parse( text=sig.cov.cols.cmd))
	write( sig.cov.cols.cmd, file="extRemes.log", append=TRUE)
      cur.cov.cols<-cur.cov.cols+length(dat.cols)
# cov.names<-c(cov.names,colnames(dd$data)[dat.cols])
# cov.names.cmd <- paste( "cov.names<-c(cov.names, colnames( ", full.list[ data.select], "$data)[dat.cols])", sep="")
	cov.names.cmd <- "cov.names <- c( cov.names, colnames( dd[[\"data\"]])[dat.cols])"
	eval( parse( text=cov.names.cmd))
	write( cov.names.cmd, file="extRemes.log", append=TRUE)
    }


    # do the gamma
    
    gam.cov.cols.cmd <- "gam.cov.cols<-NULL"
	eval( parse( text=gam.cov.cols.cmd))
	write( gam.cov.cols.cmd, file="extRemes.log", append=TRUE)
    if (tclvalue( tkcurselection(gam.covlist)) !="") {

     # get the right covariates
#      temp.cols<-as.numeric(strsplit(tkcurselection(gam.covlist)," ")[[1]])
	temp.cols <- as.numeric( unlist( strsplit( tclvalue( tkcurselection(gam.covlist)), " ")))
      cov.selected.cmd <- "cov.selected<-character(0)"
	eval( parse( text=cov.selected.cmd))
	write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      for (i in temp.cols) {
        cov.selected.cmd <- paste( "cov.selected <- c( cov.selected, \"", tclvalue( tkget( gam.covlist,i)), "\")", sep="")
	eval( parse( text=cov.selected.cmd))
        write( cov.selected.cmd, file="extRemes.log", append=TRUE)
      }
 
      # match the covariate names to the cols of 'dd'
 
      dat.cols.cmd <- "dat.cols<-numeric(0)"
	eval( parse( text=dat.cols.cmd))
	write( dat.cols.cmd, file="extRemes.log", append=TRUE)
      for (j in 1:length(colnames(dd$data))) {
        for (i in cov.selected) {
          if (i == colnames(dd$data)[j]) {
            dat.cols.cmd <- paste("dat.cols<-c(dat.cols,", j, ")", sep="")
		eval( parse( text=dat.cols.cmd))
        	write( dat.cols.cmd, file="extRemes.log", append=TRUE)
          }
        }
      }

# covs <- cbind(covs,as.matrix(dd$data[,dat.cols]))
# covs.cmd <- paste( "covs <- cbind(covs, as.matrix( ", full.list[ data.select], "$data[, dat.cols]))", sep="")
	covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
	eval( parse( text=covs.cmd))
	write( covs.cmd, file="extRemes.log", append=TRUE)
      gam.cov.cols.cmd <- paste( "gam.cov.cols<-", (cur.cov.cols+1), ":", (length(dat.cols)+cur.cov.cols), sep="")
	eval( parse( text=gam.cov.cols.cmd))
	write( gam.cov.cols.cmd, file="extRemes.log", append=TRUE)
      cur.cov.cols<-cur.cov.cols+length(dat.cols)
# cov.names<-c(cov.names,colnames(dd$data)[dat.cols])
# cov.names.cmd <- paste( " cov.names<-c(cov.names, colnames( ", full.list[ data.select], "$data)[ dat.cols])", sep="")
	cov.names.cmd <- "cov.names <- c( cov.names, colnames( dd[[\"data\"]])[dat.cols])"
	eval( parse( text=cov.names.cmd))
	write( cov.names.cmd, file="extRemes.log", append=TRUE)
    }


    # process the link functions for each
    if (tclvalue(mu.link) =="identity") m.linker.cmd <- "m.linker<-identity"
    else m.linker.cmd <- "m.linker<-exp"
	eval( parse( text=m.linker.cmd))
        write( m.linker.cmd, file="extRemes.log", append=TRUE)
 

    if (tclvalue(sig.link) =="identity") sig.linker.cmd <- "sig.linker<-identity"
    else sig.linker.cmd <- " sig.linker<-exp"
	eval( parse( text=sig.linker.cmd))
	write( sig.linker.cmd, file="extRemes.log", append=TRUE)

    if (tclvalue(gam.link) =="identity") gam.linker.cmd <- "gam.linker<-identity"
    else gam.linker.cmd <- "gam.linker<-exp"
	eval( parse( text=gam.linker.cmd))
        write( gam.linker.cmd, file="extRemes.log", append=TRUE)

method.list <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
method.select <- as.numeric( tkcurselection( method.listbox))+1
if( length( method.select) == 0) {
		cat( paste( "No optimization method selected.  Using \"BFGS\"",
			"(use \'help( optim)\' for more details)"), sep="\n")
		method.value <- "BFGS"
} else method.value <- method.list[ method.select]

	number.of.models <- length( dd$models)
	names.of.models <- names( dd$models)
	if( is.null( names.of.models)) names.of.models <- character(0)
	jj <- 0
if( number.of.models > 0) for( i in 1:number.of.models) if( class( dd$models[[i]]) == "gev.fit") jj <- jj + 1
	names.of.models <- c( names.of.models, paste( "gev.fit", jj+1, sep=""))

	# fit the GEV
# xdata.cmd <- paste( "xdata <- ", full.list[ data.select], "$data[,", resp.select, "]", sep="")
xdata.cmd <- paste( "xdata <- dd[[\"data\"]][, ", resp.select, "]", sep="")
eval( parse( text=xdata.cmd))
write( xdata.cmd, file="extRemes.log", append=TRUE)
# xdata <- dd$data[, resp.select]
	cmd <- paste( "dd[[\"models\"]][[\"gev.fit", jj+1, "\"]] <- ",
			"gev.fit( xdat=xdata, ydat=covs, mul=mu.cov.cols, mulink=m.linker, sigl=sig.cov.cols, ",
			"siglink=sig.linker, shl=gam.cov.cols, shlink=gam.linker, method=\"", method.value, "\")", sep="")
	eval( parse( text=cmd))
	write( cmd, file="extRemes.log", append=TRUE)
	# dd$models[[number.of.models+1]] <- eval( parse( text=cmd))
	# cmd <- paste( full.list[ data.select], "$models$gev.fit", jj+1, " <- ", cmd, sep="")
	# write( cmd, file="extRemes.log", append=TRUE)
	# names( dd$models) <- names.of.models
	# class( dd$models[[number.of.models+1]]) <- "gev.fit"
	# class.cmd <- paste( "class( ", full.list[ data.select], "$models$gev.fit", jj+1, ") <- \"gev.fit\"", sep="")
	classCMD <- paste( "class( dd[[\"models\"]][[", number.of.models+1, "]]) <- \"gev.fit\"", sep="")
	eval( parse( text=classCMD))
	write( classCMD, file="extRemes.log", append=TRUE)

if (is.null(dd$models[[number.of.models+1]])) {
# failure to fit 
	# tkconfigure( base.txt, state="normal")
      fail.str<-paste(" ", "Fit failed.", " ", sep="\n")
	cat( fail.str)
      # tkinsert(base.txt,"end",fail.str)
      tkyview.moveto(base.txt,1.0)
	# tkconfigure( base.txt, state="disabled")
} else {

#	if( is.nothing) assign( "extRemesData", dd, pos=".GlobalEnv")
#	else assign( full.list[ data.select], dd, pos=".GlobalEnv")
	# end of if else is.nothing stmt

      # print the output
	# fit.obj <- dd$models[[number.of.models+1]]
	# fit.obj.cmd <- paste( "fit.obj <- ", full.list[ data.select], "$models$gev.fit", jj+1, sep="")
	fit.obj.cmd <- paste( "fit.obj <- dd[[\"models\"]][[", number.of.models+1, "]]", sep="")
	eval( parse( text=fit.obj.cmd))
	# write( fit.obj.cmd, file="extRemes.log", append=TRUE)
      links<-c(tclvalue(mu.link),tclvalue(sig.link),tclvalue(gam.link))

# Print some informative output to the main gui window.
      # tkconfigure(base.txt,state="normal")
 	nl2 <- paste( "   ", "   ", sep="\n")
	cat( nl2)
# 	tkinsert( base.txt, "end", nl2)

	# Print info about convergence of 'optim' function.
	if( fit.obj$conv == 0) CONV.msg <- paste("Convergence successfull!")
	else if( fit.obj$conv == 1) CONV.msg <- paste("Iteration limit exceeded.", "Did not convergence.", sep="\n")
	else if( fit.obj$conv == 51 | fit.obj$conv == 52) CONV.msg <- paste( fit.obj$message)
	else CONV.msg <- paste("Convergence: ", fit.obj$conv, " (See help for optim for more info).", sep="")

# 	tkinsert( base.txt, "end", CONV.msg)
# 	tkinsert( base.txt, "end", nl2)
	cat( "\n", CONV.msg)
	c1 <- round( cbind( fit.obj$mle, fit.obj$se), digits=5)
	# c1.cmd <- "c1 <- round( cbind( fit.obj[[\"mle\"]], fit.obj[[\"se\"]]), digits=5)"
	# eval( parse( text=c1.cmd))
	# write( c1.cmd, file="extRemes.log", append=TRUE)
	colnames( c1) <- c( "MLE", "Stand. Err.")
	# colnamesCMD <- "colnames( c1) <- c( \"MLE\", \"Stand. Err.\")"
	# eval( parse( text=colnamesCMD))
	# write( colnamesCMD, file="extRemes.log", append=TRUE)
	rnames <- c( paste( "MU: (", links[1], ")", sep=""))
	# rnames.cmd <- paste( "rnames <- c( paste( \"MU: (\"", links[1], "\")\", sep=\"\"))", sep="")
	# eval( parse( text=rnames.cmd))
	# write( rnames.cmd, file="extRemes.log", append=TRUE)
	if( !is.null( fit.obj$model[[1]]))
		rnames <- c(rnames, paste( cov.names[ fit.obj$model[[1]]],
				": (", links[1], ")", sep=""))

	rnames <- c(rnames, paste("SIGMA: (", links[2], ")", sep = ""))
	if( !is.null( fit.obj$model[[2]]))
		rnames <- c( rnames, paste( cov.names[ fit.obj$model[[2]]],
				": (", links[2], ")", sep=""))
	rnames <- c(rnames, paste("Xi: (", links[3], ")", sep = ""))
	if( !is.null( fit.obj$model[[3]]))
		rnames <- c(rnames, paste( cov.names[ fit.obj$model[[3]]],
				": (", links[3], ")", sep=""))
	rownames( c1) <- rnames
	# Try to assign parameter names to the fitted object for summary purposes.
	dd$models[[number.of.models+1]]$parameter.names <- rnames
	dd$models[[number.of.models+1]]$summary1 <- c1
 if( is.nothing) assignCMD <- "assign( \"extRemesData\", dd, pos=\".GlobalEnv\")"
        else assignCMD <- paste( "assign( \"", full.list[ data.select], "\", dd, pos=\".GlobalEnv\")", sep="")
eval( parse( text=assignCMD))
write( assignCMD, file="extRemes.log", append=TRUE)

# print( summary( dd$models[[number.of.models+1]]))
# summaryCMD <- paste( "summary( ", full.list[ data.select], "$models$gev.fit", jj+1, ")", sep="")
	summaryCMD <- paste( "print( summary( dd[[\"models\"]][[", number.of.models+1, "]]))", sep="")
	eval( parse( text=summaryCMD))
	write( summaryCMD, file="extRemes.log", append=TRUE)

	# tkinsert( base.txt, "end", paste( "			",
	# 				colnames( c1)[1], "		",
	# 				colnames( c1)[2]))
	# tkinsert( base.txt, "end", nl2)
#	for( i in 1:dim( c1)[1]) {
#		tkinsert( base.txt, "end",
#			paste( rownames( c1)[i], "	", c1[i,1],
#					"	", c1[i,2], sep="  "))
#		tkinsert( base.txt, "end", nl2)
#		} # end of for i loop
#	tkinsert( base.txt, "end", nl2)

#      nllh.str <- paste( "\n Negative log likelihood:",
#			round(dd$models[[number.of.models+1]]$nllh,4),"\n")

#      tkinsert( base.txt, "end", nllh.str)
#      tkinsert( base.txt,"end", nl1)
	final.msg <- paste("Model name: ", names.of.models[number.of.models+1], sep="")
	cat( final.msg)
#	tkinsert( base.txt, "end", final.msg)
#      tkyview.moveto( base.txt, 1.0)

      # plot diagnostics if requested

      if (tclvalue(plot.diags)==1) {
	write( fit.obj.cmd, file="extRemes.log", append=TRUE)
	plotCMD <- "plot( fit.obj)"
	eval( parse( text=plotCMD))
	write( plotCMD, file="extRemes.log", append=TRUE)
	# plot( dd$models[[number.of.models+1]])
      }

    }
 
    tkdestroy(base)
    # tkconfigure(base.txt,state="disabled")
} # end of submit fcn

gevfithelp <- function() {
	cat( "\n", "Invokes the \'ismev\' function \'gev.fit\'.\n")
	cat( "Use \'help( gev.fit)\' for more help.\n")
	help( gev.fit)
	} # end of gevfithelp fcn

  endprog<-function() {

    tkdestroy(base)
  }

#################################
# Frame/button setup
#################################


base<-tktoplevel()
tkwm.title(base,"Fit Generalized Extreme Value Distribution")

data.frm <- tkframe( base, borderwidth=2, relief="groove")
top.frm <- tkframe(base,borderwidth=2,relief="groove")
bot.frm <- tkframe(base,borderwidth=2,relief="groove")
mu.frm <- tkframe(base,borderwidth=2,relief="groove")
sig.frm <- tkframe(base,borderwidth=2,relief="groove")
gam.frm <- tkframe(base,borderwidth=2,relief="groove")
method.frm <- tkframe( base, borderwidth=2, relief="groove")

# Choose which data object to use (set the listbox to contain all objects of
# class "extRemesDataObject").

data.listbox <- tklistbox(data.frm,
			yscrollcommand=function(...) tkset(data.scroll,...),
			selectmode="single",
			width=20,
			height=5,
			exportselection=0)

data.scroll <- tkscrollbar( data.frm, orient="vert",
			command=function(...)tkyview(data.listbox,...))
# initialize variables for data list.
# 'temp' is list of everything in global environment.
# 'full.list' will be list of all objects in '.GlobalEnv' of class "extRemesDataObject".
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

tkpack( tklabel( data.frm, text="Data Object", padx=4), side="left")
tkpack( data.listbox, side="left")
tkpack( data.scroll, side="right", fill="y")
tkpack( data.frm)

# place binding on data.listbox to reflect the chosen data from the list.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# top frame for response variable

top.r <- tkframe(top.frm,borderwidth=2)
top.l <- tkframe(top.frm,borderwidth=2)
resp.listbox <-
	tklistbox(top.l,yscrollcommand=function(...)tkset(resp.scroll,...),
			selectmode="single",width=35,height=4,exportselection=0)
resp.scroll <- tkscrollbar(top.l,orient="vert",
			command=function(...)tkyview(resp.listbox,...))

if( is.nothing) {
for( i in 1:ncol(extRemesData$data)) 
	tkinsert( resp.listbox, "end", paste(colnames(extRemesData$data)[i]))  
# end of for i loop
	} else tkinsert( resp.listbox, "end", "")

tkpack(tklabel(top.l,text="Response:",padx=4), side="left")
tkpack(resp.listbox,side="left")
tkpack(resp.scroll,side="right",fill="y")

plot.but<- tkcheckbutton(top.r,text="Plot diagnostics",variable=plot.diags)
tkpack(plot.but,side="right")
tkpack(top.l,top.r,side="left")

# place binding on resp.listbox to eliminate the response from the 
# lists of covs.
# tkbind( resp.listbox, "<Button-1>", "")
tkbind( resp.listbox, "<ButtonRelease-1>", redolists)


# mu frame

mu.l <- tkframe(mu.frm,borderwidth=2)
mu.r <- tkframe(mu.frm,borderwidth=2)
mu.covlist <- tklistbox(mu.l,yscrollcommand=function(...)tkset(mu.covscr,...),
		selectmode="multiple",width=15,height=4,exportselection=0)
mu.covscr <- tkscrollbar(mu.l,orient="vert",
		command=function(...)tkyview(mu.covlist,...)) 

if( is.nothing) {
for (i in 1:ncol(extRemesData$data))
	tkinsert( mu.covlist, "end", paste( colnames( extRemesData$data)[i]))
# end of for i loop
	} else tkinsert( mu.covlist, "end", "")

tkpack(tklabel(mu.l,text="Location parameter (mu):",padx=4), side="left")
tkpack(mu.covlist,side="left")
tkpack(mu.covscr,side="right",fill="y")
# tkpack(mu.l,side="left")

tkpack(tklabel(mu.r,text="Link:"),side="left")
for (i in c("identity","log")) {
	tmp<-tkradiobutton(mu.r,text=i,value=i,variable=mu.link)
	tkpack(tmp,anchor="w")
} # end of for i loop

tkpack(mu.r,side="right")
tkpack(mu.l,side="right")

# sigma frame
 
sig.l <- tkframe(sig.frm,borderwidth=2)
sig.r <- tkframe(sig.frm,borderwidth=2)
sig.covlist <-
	tklistbox(sig.l,yscrollcommand=function(...)tkset(sig.covscr,...),
		selectmode="multiple",width=15,height=4,exportselection=0)
sig.covscr <- tkscrollbar(sig.l,orient="vert",
		command=function(...)tkyview(sig.covlist,...))

if( is.nothing) { 
for (i in 1:ncol(extRemesData$data)) 
	tkinsert(sig.covlist,"end",paste(colnames(extRemesData$data)[i]))
# end of for i loop
	} else tkinsert( sig.covlist, "end", "")

tkpack(tklabel(sig.l,text="Scale parameter (sigma):",padx=4), side="left")
tkpack(sig.covlist,side="left")
tkpack(sig.covscr,side="right",fill="y")
 
tkpack(tklabel(sig.r,text="Link:"),side="left")

for (i in c("identity","log")) {
	tmp <- tkradiobutton(sig.r,text=i,value=i,variable=sig.link)
	tkpack(tmp,anchor="w")
} # end of for i loop
 
tkpack(sig.r,side="right")
tkpack(sig.l,side="right") 

# gamma frame
 
gam.l <- tkframe(gam.frm,borderwidth=2)
gam.r <- tkframe(gam.frm,borderwidth=2)
gam.covlist <-
	tklistbox(gam.l,yscrollcommand=function(...)tkset(gam.covscr,...),
		selectmode="multiple",width=15,height=4,exportselection=0)
gam.covscr <- tkscrollbar(gam.l,orient="vert",
		command=function(...)tkyview(gam.covlist,...))

if( is.nothing) { 
for (i in 1:ncol(extRemesData$data))
	tkinsert(gam.covlist,"end",paste(colnames(extRemesData$data)[i]))
# end of for i loop
	} else tkinsert( gam.covlist, "end", "")

tkpack(tklabel(gam.l,text="Shape parameter (xi):",padx=4), side="left")
tkpack(gam.covlist,side="left")
tkpack(gam.covscr,side="right",fill="y")
 
tkpack(tklabel(gam.r,text="Link:"),side="left")
for (i in c("identity","log")) {
	tmp <- tkradiobutton(gam.r,text=i,value=i,variable=gam.link)
	tkpack(tmp,anchor="w")
} # end of for i loop
 
tkpack(gam.r,side="right")
tkpack(gam.l,side="right")

# Method frame.

method.listbox <- tklistbox( method.frm,
			yscrollcommand=function(...)tkset(methodscr,...),
			selectmode="single",
			width=50,
			height=1,
			exportselection=0)

methodscr <- tkscrollbar( method.frm, orient="vert",
		command=function(...)tkyview(method.listbox, ...))

tkinsert( method.listbox, "end", paste( "Nelder-Mead"))
tkinsert( method.listbox, "end", paste( "BFGS quasi-Newton"))
tkinsert( method.listbox, "end", paste( "Conjugate Gradients"))
tkinsert( method.listbox, "end", paste( "L-BFGS-B"))
tkinsert( method.listbox, "end", paste( "Simulated Annealing (Belisle 1992)"))

tkpack( tklabel( method.frm, text="Optimization Method", padx=4), side="left")
tkpack( method.listbox, methodscr, side="left") 
tkpack( method.frm)

# bottom frame
sub.but <- tkbutton( bot.frm, text="OK", command=submit)
tkbind( sub.but, "<Return>", submit)
quit.but <- tkbutton(bot.frm,text="Cancel",command=endprog)
tkbind( quit.but, "<Return>", endprog)
help.but <- tkbutton( bot.frm, text="Help", command=gevfithelp)
tkbind( help.but, "<Return>", gevfithelp)
 
tkpack( sub.but, quit.but, side="left")
tkpack( help.but, side="right")

tkpack(top.frm,side="top")
tkpack(mu.frm,fill="x")
tkpack(sig.frm,fill="x")
tkpack(gam.frm,fill="x")
tkpack(bot.frm,side="bottom")


} # end of gevf.gui fcn
