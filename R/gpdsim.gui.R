gpdsim.gui <- function(base.txt) {

# Internal functions.
 
generate <- function() {
	.evTemp <- list()
#	xi <- parse(text=tclvalue(xi))[[1]]
#	sigma <- parse(text=tclvalue(sigma))[[1]]
	sigma <- as.numeric( tclvalue( sigma))
	xi <- as.numeric( tclvalue( xi))
	size <- parse(text=tclvalue(size))[[1]]
	     u.val <- as.numeric( tclvalue( u.value))
 
	gpd.sim <- gen.gpd(n=size, sigma=sigma, xi=xi, u=u.val)
	plot( gpd.sim)
	gpd.sim <- cbind(1:size, gpd.sim)
	colnames( gpd.sim) <- c("obs","gpd.sim")
	.evTemp$data <- data.frame(gpd.sim)
	.evTemp$name <- "GEV Simulated"
	.evTemp$params <- c(sigma,xi)
	.evTemp$generated <- TRUE
	class( .evTemp) <- "ev.data"

# Make sure data set has been generated and save if necessary.

	if( is.null( .evTemp$generated)) {
		msg <- paste(" ", "Must first generate data!", " ", sep="\n")
		tkconfigure( base.txt, state="normal")	
		tkinsert( base.txt, "end", msg)
		tkconfigure( base.txt, state="disabled")
		invisible()
		} else {
	save.as <- tclvalue( save.as.value)
	if( save.as != "") assign( save.as, .evTemp, pos=".GlobalEnv")

	tkconfigure( base.txt, state="normal")
	mess<-paste("GPD simulated data generated.\n")
	mess<-paste(mess,"Parameters:\n")
	tkinsert(base.txt,"end",mess)
	mess <- paste("  Scale (sigma):",.evTemp$params[1],
			"  Shape (xi):",.evTemp$params[2], "\n\n")
	tkinsert(base.txt,"end",mess)
	tkyview.moveto(base.txt,1.0)
	tkconfigure( base.txt, state="disabled")
	tkdestroy(tt)
		} # end of if else not generated stmt
	} # end of generate fcn

	gpdsimhelp <- function() {
		help( gen.gev)
		} # end of gpdsimhelp fcn
endprog <- function() {
	tkdestroy( tt)
	}

    tt <- tktoplevel()
    tkwm.title(tt, "Simulate GPD Data")
    gev.sim<-numeric(10)

# Initialize tcl variables. 
	xi <- tclVar("0.2")
	sigma <- tclVar("1")
	size <- tclVar(50)
	save.as.value <- tclVar("")
	u.value <- tclVar("0")

	spec.frm<-tkframe(tt,borderwidth=2)
	left.frm<-tkframe(spec.frm)

    # left frame (1 of 2)
    frame1<- tkframe(left.frm,relief="groove",borderwidth=2)   
    sigma.frm <- tkframe(frame1)
	xi.frm <- tkframe(frame1)
	u.frm <- tkframe(frame1)
    xi.entry <- tkentry(xi.frm, textvariable=xi, width=4)
    sigma.entry <- tkentry(sigma.frm, textvariable=sigma, width=4)
	u.entry <- tkentry( u.frm,textvariable=u.value,width=4)

    tkpack( tklabel( frame1, text="GPD parameters", pady=10, padx=10))
    tkpack( tklabel( sigma.frm, text="Scale parameter (sigma):", padx=4), sigma.entry, side="left")
    tkpack( tklabel( xi.frm, text="Shape parameter (xi)   :", padx=4), xi.entry, side="left")
	tkpack( tklabel( u.frm, text="Threshold:", padx=4),u.entry,side="left")
    tkpack(sigma.frm, xi.frm, u.frm,side="top", anchor="w")
    # tkpack(sigma.frm,side="left")
    # tkpack(xi.frm,side="bottom")


    # left frame (2 of 2)
    frame3 <- tkframe(left.frm,relief="groove", borderwidth=2)
    size.entry<-tkentry(frame3,textvariable=size,width=4) 
    tkpack(tklabel(frame3, text="Sample Size:",padx=3,pady=10),size.entry,side="left")

    # right frame
	frame2 <- tkframe( spec.frm, relief="groove", borderwidth=2)
	generate.but <- tkbutton( frame2, text="Generate", command=generate)
	save.as.entry <- tkentry( frame2, textvariable=save.as.value, width=20)

# tkpack( tklabel( frame2, text ="Command"),pady=10,padx=10)
	tkpack( tklabel( frame2, text="Save As", padx=4), save.as.entry,
			anchor="w", fill="x")
	tkpack( generate.but, anchor="w",fill="x") 

# Separate frame for help and cancel buttons.
rightbot.frm <- tkframe( tt, borderwidth=2, relief="groove")

        cancel.but <- tkbutton( rightbot.frm, text="Cancel",
                                command=endprog)
        help.but <- tkbutton( rightbot.frm, text="Help", command=gpdsimhelp)
	tkpack( cancel.but, help.but, side="left")

# place bindings on buttons so that 'Return' key executes them.
	tkbind( generate.but, "<Return>", generate)
	tkbind( cancel.but, "<Return>", endprog)
	tkbind( help.but, "<Return>", gpdsimhelp)

    # tkpack(frame1, frame3, fill="x")
   
    tkpack(frame1, fill="x")
    tkpack(frame3, fill="x") 
    tkpack(left.frm, frame2,side="left",anchor="n",fill="x",fill="y")
    tkpack(spec.frm)
	tkpack( rightbot.frm, fill="x", anchor="e") 
} # end of fcn
