"plotTS.gui" <-
function( base.txt) {

#
# very basic plotting routine
#


########################################
# Internal functions
########################################

refresh <- function() {
	tkdelete( x.list, 0.0, "end")
	tkdelete( y.list, 0.0, "end")

	dd.select <- as.numeric( tkcurselection( data.listbox))+1
	dd <- get( full.list[ dd.select])

	for( i in 1:ncol(dd$data))
		tkinsert( x.list,"end", paste( colnames( dd$data)[i]))

	for( i in 1:ncol( dd$data))
		tkinsert( y.list, "end", paste( colnames( dd$data)[i]))

	invisible()
	} # end of refresh function

submit<-function() {

# Main function called when 'OK' button is pressed.
# Actually plots the data.

	dd.select <- as.numeric( tkcurselection( data.listbox))+1
        dd <- get( full.list[ dd.select])

	x.select<-as.numeric(tkcurselection(x.list))+1
	y.select<-as.numeric(tkcurselection(y.list))+1

	# if nothing was actually selected, end 
	if( length( x.select)==0 & length(y.select)==0) {
				tkconfigure( base.txt, state="normal")
				nl1 <- paste(" ", "**********", " ", sep="\n")
				msg <- paste( "Must select a variable to plot!")
				tkinsert( base.txt, "end", nl1)
				tkinsert( base.txt, "end", msg)
				tkinsert( base.txt, "end", nl1)
				tkconfigure( base.txt, state="disabled")
				return()
	} else if( length(x.select)==0) {
		plot(	dd$data[,y.select],
			xlab=colnames(dd$data)[y.select], type="l")
      
} else if( length(y.select)==0) {
		plot(	dd$data[,x.select],
			xlab=colnames(dd$data)[x.select], type="l")
} else {
        plot(	dd$data[,x.select],
		dd$data[,y.select],
		xlab=colnames(dd$data)[x.select],
		ylab=colnames(dd$data)[y.select], type="l")
	} # end of if else stmts.
	tkdestroy( base)
	invisible()
} # end of submit fcn

endprog <- function() {
	tkdestroy(base)
	invisible()
}

plothelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg <- paste(" ", "*********", " ", "Simple 2-D plot of the data.",
			"Use command line for more advanced plotting.",
			" ", "*********", " ", sep="\n")
	tkinsert( base.txt, "end", help.msg)
	tkconfigure( base.txt, state="disabled")
	help( plot)
	invisible()
	} # end of plothelp fcn

###################################################
#  Frame/button setup
###################################################


base<-tktoplevel()
tkwm.title(base,"Plot Time Series")

top.frm <- tkframe( base, borderwidth=2, relief="groove")
left.frm <- tkframe( base, borderwidth=2, relief="groove")
right.frm <- tkframe( base, borderwidth=2, relief="groove")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# set up list for which data set.

data.listbox <- tklistbox( top.frm,
                        yscrollcommand=function(...) tkset(data.scroll, ...),
                        selectmode="single",
                        width=20,
                        height=5,
                        exportselection=0)

data.scroll <- tkscrollbar( top.frm, orient="vert",
                        command=function(...) tkyview( data.listbox, ...))

temp <- ls(all=TRUE, name=".GlobalEnv")
is.nothing <- TRUE
full.list <- character(0)
for( i in 1:length( temp)) {
        if( is.null( class( get( temp[i])))) next
        if( (class(get( temp[i]))[1] == "ev.data")) {
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

 
# set up the list for xaxis selection 
xframe<-tkframe(left.frm,borderwidth=2)

x.list <- tklistbox( xframe,
		yscrollcommand=function(...)tkset(x.scroll,...),
		selectmode="single",
		width=15,
		height=2,
		exportselection=0)

x.scroll <- tkscrollbar( xframe, orient="vert",
			command=function(...)tkyview(x.list,...))


tkinsert(x.list,"end", "")
 
  
  tkpack(tklabel(xframe,text="x-axis variable:",padx=4),side="left")
  tkpack(x.list,side="left")
  tkpack(x.scroll,side="right",fill="y")
  tkpack(xframe)


 # set up the list for yaxis selection
  yframe<-tkframe(right.frm,borderwidth=2)
  y.list<-tklistbox(yframe,yscrollcommand=function(...)tkset(y.scroll,...),
			selectmode="single",width=15,height=2,exportselection=0)
  y.scroll<-tkscrollbar(yframe,orient="vert",command=function(...)tkyview(y.list,...))
 
tkinsert( y.list, "end", "")
 
  tkpack(tklabel(yframe,text="y-axis variable:",padx=4),side="left")
  tkpack(y.list,side="left")
  tkpack(y.scroll,side="right",fill="y")
  tkpack(yframe)

  # make the bottom frame

sub.but <- tkbutton( bot.frm, text="OK", command=submit)
quit.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
help.but <- tkbutton( bot.frm, text="Help", command=plothelp)

tkpack( sub.but, quit.but, side="left")
tkpack( help.but, side="right")
tkpack( left.frm, right.frm, side="top")
tkpack( bot.frm, side="bottom")

}
