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
	.ev <- list()

	# Data is actually read into R here.
	hh <- as.logical(as.numeric(tclvalue(head))) 
	if( tclvalue( file.type)=="common")
		.ev$data <- read.table(file.name, header=hh, sep=tclvalue(delimiter))
	else .ev$data <- source( file.name)$value

	if( is.null( dim( .ev$data))) {
		.ev$data <- cbind(1:length( .ev$data),.ev$data)
		if( is.null( colnames( .ev$data))) colnames( .ev$data) <- c("obs", "value")
		else colnames( .ev$data)[1] <- "obs"
	} else {
		nc <- dim( .ev$data)[2]
		if( is.null( colnames( .ev$data))) colnames( .ev$data) <- as.character(1:nc)
		}
	class( .ev) <- "ev.data"
	.ev$name <- strsplit(file.name,"/")[[1]][length(strsplit(file.name,"/")[[1]])] 
	.ev$file.path <- file.name
	if( save.name == "") save.name <- ".ev"

	# Assign data object the value of 'save.name' in R--default is ".ev".
	assign( save.name, .ev, pos=".GlobalEnv")
	.ev$default.ldata <- save.name

	mess <- paste("  ", "*****", "Successfully opened file", .ev$name,
		"  ", "*****", "  ", sep="\n")
	tkconfigure(base.txt,state="normal")
	tkinsert(base.txt,"end",mess)
	# Print a sample of the data to the screen (first and last 3 rows).
	if( dim( .ev$data)[1] > 6) {
		n <- dim( .ev$data)[1]
		m <- dim( .ev$data)[2]
		datanames <- character(0)
		datasample1 <- datanames
		datasample2 <- datanames
		datasample3 <- datanames
		ellipses <- datanames
		datasample4 <- datanames
		datasample5 <- datanames
		datasample6 <- datanames
		
		temp1 <- .ev$data[1,]
		temp2 <- .ev$data[2,]
		temp3 <- .ev$data[3,]
		temp.ellipses <- rep("...", m)
		tempnl2 <- .ev$data[n-2,]
		tempnl1 <- .ev$data[n-1,]
		tempn	<- .ev$data[n,]
		for( i in 1:m) {
		datanames <- paste( datanames, colnames( .ev$data)[i],
					sep="	")
		datasample1 <- paste( datasample1, temp1[i], sep="	")
		datasample2 <- paste( datasample2, temp2[i], sep="	")
		datasample3 <- paste( datasample3, temp3[i], sep="	")
		ellipses <- paste( ellipses, temp.ellipses[i], sep="	")
		datasample4 <- paste( datasample4, tempnl2[i], sep="      ")
                datasample5 <- paste( datasample5, tempnl1[i], sep="      ")
                datasample6 <- paste( datasample6, tempn[i], sep="      ")
			} # end of for i loop.
		nl <- paste(" ", " ", sep="\n")
		tkinsert( base.txt, "end", datanames)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample1)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample2)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample3)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", ellipses)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample4)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample5)
		tkinsert( base.txt, "end", nl)
		tkinsert( base.txt, "end", datasample6)
		}
	tkyview.moveto(base.txt,1.0)
	tkconfigure(base.txt,state="disabled")
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
