"gpdf.gui" <-
function( base.txt) {

#
# This function provides a gui for Stuart Coles
# 'gpd.fit' function and helpers
#



#  Set the tcl variables
plot.diags<-tclVar(0)
threshold.value <- tclVar("")
npy <- tclVar("365.25")
sig.link<-tclVar("identity")
gam.link<-tclVar("identity")
save.as.value <- tclVar("")
maxit.value <- tclVar("10000")

#########################################
# internal functions
#########################################

refresh <- function() {

# when a data object is chosen, this function will reset the various lists
# to have the correct covariates, etc... (at least in theory)

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	} else dd <- .ev

	tkdelete( resp.listbox, 0.0, "end")
	tkdelete( sig.covlist, 0.0, "end")
	tkdelete( gam.covlist, 0.0, "end")

	for( i in 1:ncol(dd$data))
        	tkinsert( resp.listbox, "end",
			paste( colnames( dd$data)[i]))
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
#  as a covariate option from the other lists.

if( !is.nothing) {
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ data.select])
	} else dd <- .ev

dd2 <- dd$data[, as.numeric( tkcurselection( resp.listbox))+1]

resp.name <-
	colnames(dd$data)[as.numeric(tkcurselection(resp.listbox))+1] 

    # put the correct eligible covariates in the other list boxes
    tkdelete(sig.covlist,0.0,"end")
    tkdelete(gam.covlist,0.0,"end")

	for (i in colnames(dd$data)) {
		if (i != resp.name) {
        		tkinsert(sig.covlist,"end",i)
        		tkinsert(gam.covlist,"end",i)
      		} # end of if i != resp.name stmt
 
	} # end of for i loop    
} # end of redolists fcn

submit <- function() {
    #
    # The meat of this program.  Actually fits the gpd to the data.
    #

    # names of the covariates used (if any) 
    cov.names<-character(0) 

	if( !is.nothing) {
		data.select <- as.numeric( tkcurselection( data.listbox))+1
		dd <- get( full.list[ data.select])
	} else dd <- .ev

    resp.select<-as.numeric(tkcurselection(resp.listbox))+1
 
    # make sure that a response was selected
    if (is.na(resp.select))
      return()


    tkconfigure(base.txt,state="normal")
    tkinsert(base.txt,"end",paste("GPD fit \n"))
    tkinsert(base.txt,"end",paste("-----------------------------------\n"))
    tkinsert(base.txt,"end",paste("Response variable:",
             colnames( dd$data)[resp.select],"\n"))

    # process the covariates and link functions
	covs <- NULL
	cur.cov.cols <- 0

# do the sigma 
   
    sig.cov.cols<-NULL 
    if (tclvalue( tkcurselection(sig.covlist)) !="") {

     # get the right covariates
     # temp.cols<-as.numeric(strsplit(tkcurselection(sig.covlist)," ")[[1]])
temp.cols <- 
	as.numeric( unlist( strsplit( tclvalue( tkcurselection(sig.covlist)), " ")))
      cov.selected<-character(0)
      for (i in temp.cols) {
        cov.selected<-c(cov.selected, tclvalue( tkget(sig.covlist,i)))
      }
 
      # match the covariate names to the cols of  dd
 
      dat.cols<-numeric(0)
      for (j in 1:length(colnames( dd$data))) {
        for (i in cov.selected) {
          if (i == colnames( dd$data)[j]) {
            dat.cols<-c(dat.cols,j)
          }
        }
      }


      covs <- cbind(covs,as.matrix( dd$data[,dat.cols]))
      sig.cov.cols<-(cur.cov.cols+1):(length(dat.cols)+cur.cov.cols)
      cur.cov.cols<-cur.cov.cols+length(dat.cols)
      cov.names<-c(cov.names,colnames( dd$data)[dat.cols])
    }


    # do the gamma
    
    gam.cov.cols<-NULL
    if (tclvalue( tkcurselection(gam.covlist)) !="") {

     # get the right covariates
     #  temp.cols<-as.numeric(strsplit(tkcurselection(gam.covlist)," ")[[1]])
	temp.cols <- as.numeric( unlist( strsplit( tclvalue( tkcurselection(gam.covlist)), " ")))
      cov.selected<-character(0)
      for (i in temp.cols) {
        cov.selected<-c(cov.selected, tclvalue( tkget(gam.covlist,i)))
      }
 
      # match the covariate names to the cols of  dd
 
      dat.cols<-numeric(0)
      for (j in 1:length(colnames( dd$data))) {
        for (i in cov.selected) {
          if (i == colnames( dd$data)[j]) {
            dat.cols<-c(dat.cols,j)
          }
        }
      }

      covs <- cbind(covs,as.matrix( dd$data[,dat.cols]))
      gam.cov.cols<-(cur.cov.cols+1):(length(dat.cols)+cur.cov.cols)
      cur.cov.cols<-cur.cov.cols+length(dat.cols)
      cov.names<-c(cov.names,colnames( dd$data)[dat.cols])
    }


    # process the link functions for each
    if (tclvalue(sig.link) =="identity") {
      sig.linker<-identity
    }
    else {
      sig.linker<-exp
    }

    if (tclvalue(gam.link) =="identity") {
      gam.linker<-identity
    }
    else {
      gam.linker<-exp
    }

method.list <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
method.select <- as.numeric( tkcurselection( method.listbox))+1
method.value <- method.list[ method.select]

maxit.val <- as.numeric( tclvalue( maxit.value))

number.of.models <- length( dd$models)
names.of.models <- names( dd$models)
if( is.null( names.of.models)) names.of.models <- character(0)
jj <- 0
if( number.of.models > 0) for( i in 1:number.of.models) if( class( dd$models[[i]]) == "gpd.fit") jj <- jj+1
names.of.models <- c( names.of.models, paste( "gpd.fit", jj+1, sep=""))

# fit the GPD
dd$models[[number.of.models+1]] <-
	gpd.fit(	xdat=dd$data[,resp.select],
			threshold=as.numeric( tclvalue( threshold.value)),
			npy=as.numeric( tclvalue( npy)),
			ydat=covs,
			sigl=sig.cov.cols,
			siglink=sig.linker,
			shl=gam.cov.cols,
			shlink=gam.linker,
			method=method.value,
			maxit=maxit.val)
names( dd$models) <- names.of.models
class( dd$models[[number.of.models+1]]) <- "gpd.fit"

if( is.null( dd$models[[number.of.models+1]])) {
      # failure to fit

      fail.str<-paste(" ", "Fit failed.", " ", sep="\n")
      tkinsert(base.txt,"end",fail.str)
      tkyview.moveto(base.txt,1.0)

    }
    else {

#	if( is.nothing) assign( ".ev", dd, pos=".GlobalEnv")
#	else assign( full.list[ data.select], dd, pos=".GlobalEnv")

      # print the output

	fit.obj <- dd$models[[number.of.models+1]]

      links<-c( tclvalue(sig.link), tclvalue(gam.link))

# Print some informative output to the main gui window.
	tkconfigure( base.txt, state="normal")
	nl1 <- paste( " ", "**********", " ", sep="\n")
        nl2 <- paste( "   ", "   ", sep="\n")
	tkinsert( base.txt, "end", nl1)
        tkinsert( base.txt, "end", nl2)

        # Print info about convergence of 'optim' function.
        if( fit.obj$conv == 0)
                CONV.msg <- paste("Convergence successfull!")
        else if( fit.obj$conv == 1)
                CONV.msg <- paste("Iteration limit exceeded.",
                                        "Did not convergence.", sep="\n")
        else if( fit.obj$conv == 51 | fit.obj$conv == 52)
                CONV.msg <- paste( fit.obj$message)
	else CONV.msg <- paste("Convergence code: ", fit.obj$conv, " (See help file for optim for more info)",
					sep="")

        tkinsert( base.txt, "end", CONV.msg)
        tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", nl2)
        Thresh.msg <- paste( paste("Threshold = ", fit.obj$threshold, sep=""),
                                paste("Number of exceedances = ", fit.obj$nexc, sep=""),
                                paste("Exceedance rate (per year) = ", fit.obj$rate*fit.obj$npy, sep=""), sep="\n")
        tkinsert( base.txt, "end", Thresh.msg)
        tkinsert( base.txt, "end", nl2)
        tkinsert( base.txt, "end", nl2)
        c1 <- cbind( fit.obj$mle, fit.obj$se)
        colnames( c1) <- c( "MLE", "Std. Err.")
	# rnames <- c( paste("SIGMA: (", links[1], ")	", sep = ""))
	if( tclvalue( sig.link)=="log") rnames <- c( paste( "log Scale: ", sep=""))
	else rnames <- c( paste( "Scale (sigma): ", sep=""))
        if( !is.null( fit.obj$model[[1]]))
                rnames <- c( rnames, paste( cov.names[ fit.obj$model[[1]]], sep=""))
			#	": (", links[1], ")	", sep=""))
	if( tclvalue( gam.link) == "log") rnames <- c(rnames, paste("log Shape: ", sep=""))
	else rnames <- c( rnames, paste("Shape (xi): ", sep=""))
	# rnames <- c(rnames, paste("Xi: (", links[2], ")	", sep = ""))
        if( !is.null( fit.obj$model[[2]]))
                rnames <- c(rnames, paste( cov.names[ fit.obj$model[[2]]], sep=""))
			#	": (", links[2], ")     ", sep=""))
        rownames( c1) <- rnames
	dd$models[[number.of.models+1]]$parameter.names <- rnames
	dd$models[[number.of.models+1]]$summary1 <- c1

	 if( is.nothing) assign( ".ev", dd, pos=".GlobalEnv")
        else assign( full.list[ data.select], dd, pos=".GlobalEnv")

        tkinsert( base.txt, "end", paste( "			",
			colnames( c1)[1], "		",
			colnames( c1)[2]))
        tkinsert( base.txt, "end", nl2)
        for( i in 1:dim( c1)[1]) {
                tkinsert( base.txt, "end",
                        paste( rownames( c1)[i], " ",
				round( c1[i,1], digits=5),
				"	",
				round( c1[i,2], digits=5), sep=""))
                tkinsert( base.txt, "end", nl2)
                } # end of for i loop
        tkinsert( base.txt, "end", nl2)

      nllh.str <- paste( "\n Negative log likelihood:",
                        round(dd$models[[number.of.models+1]]$nllh,4),"\n")
      tkinsert( base.txt, "end", nllh.str)
      tkinsert( base.txt,"end", nl1)
	final.msg <- paste(" ", "Saved as: ", names.of.models[number.of.models+1], " ", sep="\n")
	tkinsert( base.txt, "end", final.msg)
      tkyview.moveto( base.txt, 1.0)

      # plot diagnostics if requested

      if (tclvalue(plot.diags)==1) {
	plot( dd$models[[number.of.models+1]])
      }

    }
 
    tkdestroy(base)
    tkconfigure(base.txt,state="disabled")
} # end of submit fcn

gpdfithelp <- function() {
	help( gpd.fit)
	} # end of gpdfithelp fcn

  endprog<-function() {
	tkdestroy(base)
  }

#################################
# Frame/button setup
#################################


base<-tktoplevel()
tkwm.title(base,"Fit Generalized Pareto Distribution")

data.frm <- tkframe( base, borderwidth=2, relief="groove")
top.frm <- tkframe(base,borderwidth=2,relief="groove")
bot.frm <- tkframe(base,borderwidth=2,relief="groove")
args.frm <- tkframe( base, borderwidth=2, relief="groove")
threshold.frm <- tkframe( args.frm, borderwidth=2, relief="groove")
npy.frm <- tkframe( args.frm, borderwidth=2, relief="groove")
optim.frm <- tkframe( base, borderwidth=2, relief="groove")
method.frm <- tkframe( optim.frm, borderwidth=2, relief="flat")
maxit.frm <- tkframe( optim.frm, borderwidth=2, relief="flat")

# Choose which data object to use (set the listbox to contain all objects of
# class "ev.data").

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
# 'full.list' will be list of all objects in '.GlobalEnv' of class "ev.data".
temp <- ls(all=TRUE, name=".GlobalEnv")
is.nothing <- TRUE
full.list <- character(0)
for( i in 1:length( temp)) {
	if( is.null( class( get( temp[i])))) next
	if( (class( get( temp[i]))[1] == "ev.data")) {
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
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# top frame for response variable

top.r <- tkframe(top.frm,borderwidth=2)
top.l <- tkframe(top.frm,borderwidth=2)
resp.listbox <-
	tklistbox(top.l,yscrollcommand=function(...)tkset(resp.scroll,...),
			selectmode="single",width=15,height=4,exportselection=0)
resp.scroll <- tkscrollbar(top.l,orient="vert",
			command=function(...)tkyview(resp.listbox,...))
if( is.nothing) {
for( i in 1:ncol( .ev$data)) 
	tkinsert( resp.listbox, "end", paste(colnames( .ev$data)[i]))  
# end of for i loop
	} else tkinsert( resp.listbox, "end", "")

tkpack(tklabel(top.l,text="Response:",padx=4), side="top")
tkpack(resp.listbox, resp.scroll, side="left", fill="y")

# place binding on resp.listbox to eliminate the response from the 
# lists of covs.
tkbind(resp.listbox,"<ButtonRelease-1>",redolists)

# threshold frame

threshold.entry <- tkentry( threshold.frm, textvariable=threshold.value,
						width=5)

tkpack( tklabel( threshold.frm, text="Threshold", padx=4),
		threshold.entry, side="left")

# npy frame

# npy.value <- tkscale(	npy.frm,
# 			variable=npy,
# 			tickinterval=182,
# 			length=250,
# 			from=0,
# 			to=365,
# 			label="Number of obs per year",
# 			orient="horizontal")
npy.entry <- tkentry( npy.frm, textvariable=npy, width=6)
tkpack( tklabel( npy.frm, text="Number of obs per year", padx=4),
	npy.entry, side="left")
tkpack( threshold.frm, npy.frm, side="left", fill="both")

param.frm <- tkframe( top.r, borderwidth=2, relief="groove")
sig.frm <- tkframe( param.frm, borderwidth=2, relief="groove")
gam.frm <- tkframe( param.frm, borderwidth=2, relief="groove")

# sigma frame
 
sig.l <- tkframe(sig.frm,borderwidth=2)
sig.r <- tkframe(sig.frm,borderwidth=2)
sig.covlist <-
	tklistbox(sig.l,yscrollcommand=function(...)tkset(sig.covscr,...),
		selectmode="multiple",width=15,height=4,exportselection=0)
sig.covscr <- tkscrollbar(sig.l,orient="vert",
		command=function(...)tkyview(sig.covlist,...))

if( is.nothing) { 
for (i in 1:ncol( .ev$data)) 
	tkinsert(sig.covlist,"end",paste(colnames( .ev$data)[i]))
# end of for i loop
	} else tkinsert( sig.covlist, "end", "")

tkpack(sig.covlist,side="left")
tkpack(sig.covscr,side="right",fill="y")
 
tkpack(tklabel(sig.r,text="Link:"),side="left")

for (i in c("identity","log")) {
	tmp <- tkradiobutton(sig.r,text=i,value=i,variable=sig.link)
	tkpack(tmp,anchor="w")
} # end of for i loop
 
tkpack( sig.l, side="top") 
tkpack( sig.r, side="top", before=sig.l)
tkpack( tklabel( sig.frm, text="Scale parameter (sigma):", padx=4), side="top", before=sig.r)

# gamma frame
 
gam.l <- tkframe( gam.frm, borderwidth=2)
gam.r <- tkframe( gam.frm, borderwidth=2)
gam.covlist <-
	tklistbox(gam.l,yscrollcommand=function(...)tkset(gam.covscr,...),
		selectmode="multiple",width=15,height=4,exportselection=0)
gam.covscr <- tkscrollbar(gam.l,orient="vert",
		command=function(...)tkyview(gam.covlist,...))

if( is.nothing) { 
for (i in 1:ncol( .ev$data))
	tkinsert(gam.covlist,"end",paste(colnames( .ev$data)[i]))
# end of for i loop
	} else tkinsert( gam.covlist, "end", "")

tkpack( gam.covlist, side="left")
tkpack( gam.covscr, side="right", fill="y")
 
tkpack( tklabel( gam.r, text="Link:"), side="left")
for (i in c("identity","log")) {
	tmp <- tkradiobutton(gam.r,text=i,value=i,variable=gam.link)
	tkpack(tmp,anchor="w")
} # end of for i loop
 
tkpack( gam.l, side="top")
tkpack( gam.r, side="top", before=gam.l)
tkpack( tklabel( gam.frm, text="Shape parameter (xi):",padx=4), side="top", before=gam.r)

plot.but<- tkcheckbutton(top.r,text="Plot diagnostics",variable=plot.diags)
tkpack(plot.but,side="right")

tkpack(top.l,top.r,side="left")

# method frame

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

tkpack( tklabel( method.frm, text="Method", padx=4), side="left")
tkpack( method.listbox, methodscr, side="left")

# maxit frame

maxit.entry <- tkentry( maxit.frm, textvariable=maxit.value, width=6)
tkpack( tklabel( maxit.frm, text="Max iterations", padx=4), maxit.entry,
		side="left")
tkpack( method.frm, maxit.frm, side="top")

# bottom frame
ok.but <- tkbutton(bot.frm,text="OK",command=submit)  
quit.but <- tkbutton(bot.frm,text="Cancel",command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=gpdfithelp)

tkpack( ok.but, quit.but, side="left")
tkpack( help.but, side="right")

# place bindings on "OK", "Cancel" and "Help" buttons so that user can hit
# the return key to execute them.
tkbind( ok.but, "<Return>", submit)
tkbind( quit.but, "<Return>", endprog)
tkbind( help.but, "<Return>", gpdfithelp)

tkpack( top.frm, side="top", fill="x")
tkpack( optim.frm, args.frm, side="top", fill="both")
tkpack( sig.frm, gam.frm, side="left")
tkpack( param.frm, side="top", fill="x")
tkpack(bot.frm, side="top", fill="x")

} # end of gpd.gui fcn
