"load.data" <-
function(base.txt) {
#
# This function loads the data set using tkfilefind to locate it.
# 

############################
# Internal functions
############################

readit <- function() {
	# This is the name for the new data object in R.
	save.name <- tclvalue( sname)
	extRemesData.cmd <- "extRemesData <- list()"
	eval( parse( text=extRemesData.cmd))
	write( extRemesData.cmd, file="extRemes.log", append=TRUE)

	# Data is actually read into R here.
	hh <- as.logical(as.numeric(tclvalue(head))) 
	if( tclvalue( file.type)=="common") {
		extRemesData.cmd <- paste( "read.table( \"", file.name, "\", header=", hh,
					", sep=\"", tclvalue(delimiter), "\")", sep="")
		# extRemesData$data <- read.table(file.name, header=hh, sep=tclvalue(delimiter))
		extRemesData$data <- eval( parse( text=extRemesData.cmd))
		extRemesData.cmd <- paste( "extRemesData$data <- ", extRemesData.cmd, sep="")
		write( extRemesData.cmd, file="extRemes.log", append=TRUE)
	} else {
		extRemesData.cmd <- paste( "extRemesData[[\"data\"]] <- source( \"", file.name, "\")$value", sep="")
		# extRemesData$data <- source( file.name)$value
		eval( parse( text=extRemesData.cmd))
                write( extRemesData.cmd, file="extRemes.log", append=TRUE)
	}

	if( is.null( dim( extRemesData$data))) {
		nl <- length( extRemesData$data)
		# extRemesData$data <- cbind( 1:nl, extRemesData$data)
		extRemesData.cmd <- paste( "extRemesData[[\"data\"]] <- cbind( 1:", nl, ", extRemesData[[\"data\"]])",
						sep="")
		eval( parse( text=extRemesData.cmd))
                write( extRemesData.cmd, file="extRemes.log", append=TRUE)
		if( is.null( colnames( extRemesData$data))) {
			colnames.cmd <- "colnames( extRemesData[[\"data\"]]) <- c(\"obs\", \"value\")"
			# colnames( extRemesData$data) <- c("obs", "value")
			eval( parse( text=colnames.cmd))
			write( colnames.cmd, file="extRemes.log", append=TRUE)
		} else {
			colnames.cmd <- "colnames( extRemesData[[\"data\"]])[1] <- c(\"obs\", \"value\")"
			# colnames( extRemesData$data)[1] <- "obs"
			eval( parse( text=colnames.cmd))
                        write( colnames.cmd, file="extRemes.log", append=TRUE)
		}
	} else {
		nc <- dim( extRemesData$data)[2]
		if( is.null( colnames( extRemesData$data))) {
			colnames.cmd <- paste( "colnames( extRemesData[[\"data\"]]) <- paste( \"V\", 1:", nc, ", sep=\"\")",
						sep="")
			# colnames( extRemesData$data) <- as.character(1:nc)
			eval( parse( text=colnames.cmd))
                        write( colnames.cmd, file="extRemes.log", append=TRUE)
			}
		}
	class.cmd <- "class( extRemesData) <- \"extRemesDataObject\""
	eval( parse( text=class.cmd))
	write( class.cmd, file="extRemes.log", append=TRUE)
	# class( extRemesData) <- "extRemesDataObject"

	extRemesData.cmd <- paste( "extRemesData$name <- strsplit( \"", file.name, "\", \"/\")[[1]][ ",
					length(strsplit(file.name,"/")[[1]]), "]", sep="")
	# extRemesData$name <- strsplit(file.name,"/")[[1]][length(strsplit(file.name,"/")[[1]])] 
	eval( parse( text=extRemesData.cmd))
	write( extRemesData.cmd, file="extRemes.log", append=TRUE)

	extRemesData.cmd <- paste( "extRemesData$file.path <- \"", file.name, "\"", sep="")
	# extRemesData$file.path <- file.name
	eval( parse( text=extRemesData.cmd))
        write( extRemesData.cmd, file="extRemes.log", append=TRUE)
	if( save.name == "") {
		save.name <- "extRemesData"
		saveit <- FALSE
	} else saveit <- TRUE

	# Assign data object the value of 'save.name' in R--default is "extRemesData".
	assignCMD <- paste( "assign( \"", save.name, "\", extRemesData, pos=\".GlobalEnv\")", sep="")
	# assign( save.name, extRemesData, pos=".GlobalEnv")
	eval( parse( text=assignCMD))
	write( assignCMD, file="extRemes.log", append=TRUE)
	extRemesData$default.ldata <- save.name
	print( paste( "Successfully opened file: ", extRemesData$name, sep=""))
	# mess <- paste("  ", "*****", "Successfully opened file", extRemesData$name, "  ", "*****", "  ", sep="\n")
	# tkconfigure(base.txt,state="normal")
	# tkinsert(base.txt,"end",mess)
	# Print a sample of the data to the screen (first and last 3 rows).
	# print( extRemesData$data[1:3,])
	# print( paste("...", sep=""))
	# print( extRemesData$data[(dim( extRemesData$data)[1]-2):dim( extRemesData$data)[1],])
	msg.cmd <- "print( stats2( extRemesData[[\"data\"]]))"
	eval( parse( text=msg.cmd))
	write( msg.cmd, file="extRemes.log", append=TRUE)
	if( saveit) {
                cat("\n", "Saving workspace (may take a few moments for large workspaces) ...\n")
                saveCMD <- "save.image()"
                eval( parse( text=saveCMD))
                write( saveCMD, file="extRemes.log", append=TRUE)
		cat( "\n", "Workspace saved.\n")
        }
# 	if( dim( extRemesData$data)[1] > 6) {
# 		n <- dim( extRemesData$data)[1]
# 		m <- dim( extRemesData$data)[2]
# 		datanames <- character(0)
# 		datasample1 <- datanames
# 		datasample2 <- datanames
# 		datasample3 <- datanames
# 		ellipses <- datanames
# 		datasample4 <- datanames
# 		datasample5 <- datanames
# 		datasample6 <- datanames
# 		
# 		temp1 <- extRemesData$data[1,]
# 		temp2 <- extRemesData$data[2,]
# 		temp3 <- extRemesData$data[3,]
# 		temp.ellipses <- rep("...", m)
# 		tempnl2 <- extRemesData$data[n-2,]
# 		tempnl1 <- extRemesData$data[n-1,]
# 		tempn	<- extRemesData$data[n,]
# 		for( i in 1:m) {
# 		datanames <- paste( datanames, colnames( extRemesData$data)[i],
# 					sep="	")
# 		datasample1 <- paste( datasample1, temp1[i], sep="	")
# 		datasample2 <- paste( datasample2, temp2[i], sep="	")
# 		datasample3 <- paste( datasample3, temp3[i], sep="	")
# 		ellipses <- paste( ellipses, temp.ellipses[i], sep="	")
# 		datasample4 <- paste( datasample4, tempnl2[i], sep="      ")
#                 datasample5 <- paste( datasample5, tempnl1[i], sep="      ")
#                 datasample6 <- paste( datasample6, tempn[i], sep="      ")
# 			} # end of for i loop.
# 		nl <- paste(" ", " ", sep="\n")
# 		tkinsert( base.txt, "end", datanames)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample1)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample2)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample3)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", ellipses)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample4)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample5)
# 		tkinsert( base.txt, "end", nl)
# 		tkinsert( base.txt, "end", datasample6)
# 		}
# 	tkyview.moveto(base.txt,1.0)
# 	tkconfigure(base.txt,state="disabled")
	tkdestroy( tt)
} # end of readit fcn

    endprog <-function() {
      tkdestroy(tt)
    }


###########################################

# get the filename
# file.name <-tkfilefind()
file.name <- tclvalue( tkgetOpenFile()) 
 
# make sure that a file was selected
if (!is.null(file.name)) {
	tt<-tktoplevel()
	tkwm.title(tt,"Read File")
	delimiter <- tclVar("")
	head <- tclVar("0")
	sname <- tclVar("")
	file.type <- tclVar("common") # Other choice is 'Rsrc' (R source).

################################
#  Frame/button setup
################################


	spec.frm <- tkframe(tt, borderwidth=2)
	
	ftype.frm <- tkframe( spec.frm, relief="groove", borderwidth=2)
	tkpack( tklabel( ftype.frm, text="File Type", padx=4), side="top")
	types <- c("Common", "R source")
	types2 <- c("common", "Rsrc")
	for( i in 1:2) {
		tmp <- tkradiobutton( ftype.frm, text=types[i],
				value=types2[i], variable=file.type)
		tkpack( tmp, anchor="w")
		}

	left.frm <- tkframe(spec.frm,relief="groove",borderwidth=2)
	sep.entry <- tkentry(left.frm,textvariable=delimiter, width=1)
	tkpack(tklabel(left.frm, text="Delimiter:", padx=0), sep.entry, 
			anchor="w")
	
	right.frm <- tkframe(spec.frm, relief="groove", borderwidth=2)
	header.cbut <- tkcheckbutton(right.frm, text="Header", variable=head) 
	tkpack(header.cbut, anchor="w")

	tkpack( ftype.frm, side="left", fill="y")
	tkpack(left.frm,side="left",fill="y")
	tkpack(right.frm, side="left",fill="y")
	
	txt.frm<-tkframe(tt,relief="groove",borderwidth=2)
	txt<-tktext(txt.frm,bg="white",font="courier")
	scr.txt<-tkscrollbar(txt.frm,command=function(...)tkyview(txt,...))
	tkconfigure(txt,yscrollcommand=function(...)tkset(scr.txt,...))
	tkpack(txt,side="left",fill="both",expand=TRUE)
	tkpack(scr.txt,side="right",fill="y")
# File is read in here preliminarily.  This took a tremendous amount of time.
# Thus it has been removed.
# data.file<-tkcmd("open",file.name)
#	tkinsert(txt,"end",tkcmd("read",data.file))
# 	tkcmd("close",data.file)
	tkconfigure(txt,state="disabled")
   
# left.frm <- tkframe(spec.frm,relief="groove",borderwidth=2)
#        sep.entry <- tkentry(left.frm,textvariable=delimiter, width=1)
#        tkpack(tklabel(left.frm, text="Separator:", padx=0), sep.entry,
#                        anchor="w")

	save.frm <- tkframe(spec.frm, relief="groove", borderwidth=2)
	save.entry <- tkentry(save.frm, textvariable=sname, width=20) 
	tkpack( tklabel( save.frm, text="Save As (in R)", padx=0), save.entry,
			anchor="w")
	tkpack( save.frm, side="left", fill="y")

	sub.but <- tkbutton(spec.frm,text="OK",command=readit)
	# Place binding on 'sub.but' so that user may simply hit return key.
	# However, only works if user "TABS" over to the 'sub.but' first.
	tkbind( sub.but, "<Return>", readit)

	quit.but <- tkbutton(spec.frm,text="Cancel", command=endprog)
	tkbind( quit.but, "<Return>", endprog)
	tkpack(save.entry, sub.but, quit.but,fill="x", anchor="n")         
	tkpack(spec.frm)
  
	tkpack(txt.frm)
	tkwait.window(tt) 

	} # end of if file actually read stmt
}
