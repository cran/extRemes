"poisson.gui" <-
function(base.txt) {

  # fits a poisson distribution with
  # a trend if desired
diags.value <- tclVar(0)

########################################
#  Internal functions
########################################

submit <- function() {

	# Obtain selected data.
	data.select <- as.numeric( tkcurselection( data.listbox))+1
        if( is.nothing) dd <- .ev
        else dd <- get( full.list[ data.select])

    # fit the poisson distribution

    resp.select<-as.numeric(tkcurselection(resp.listbox))+1

    if (is.na(resp.select))
      return()

    tkconfigure(base.txt,state="normal")
    tkinsert(base.txt,"end","\nPoisson Fit\n")
    tkinsert(base.txt,"end","-------------------------\n")


    if (tclvalue( tkcurselection(trend.list)) =="") {
      # no trend component indicated
    
      gsum <- sum( dd$data[,resp.select])
	m1 <- length( dd$data[,resp.select])
      lambda <- gsum/m1
	sigma2 <- var( dd$data[,resp.select], na.rm=TRUE)
	chisqtest <- (m1-1)*sigma2/lambda
	pval <- pchisq( chisqtest, df=m1-1, lower.tail=FALSE)
	nl1 <- paste("**********", " ", sep="\n")
	tkinsert( base.txt, "end", nl1)
	nl2 <- paste(" ", " ", sep="\n")	
      mess <- paste("  Lambda: ",round(lambda,3), sep="")
	tkinsert( base.txt, "end", nl2)
      tkinsert( base.txt, "end", mess)
	
	mess2 <- paste(" Variance: ", round( sigma2,3), sep="")
	tkinsert( base.txt, "end", mess2)
	tkinsert( base.txt, "end", nl2)
	mess3 <- paste(" Chi-square statistic: ", round( chisqtest, 3), " with ", m1-1, " degrees of freedom", sep="")
	tkinsert( base.txt, "end", mess3)
	tkinsert( base.txt, "end", nl2)
	mess4 <- paste(" p-value for Chi-square test of equality of mean and variance: ", round( pval, 3), sep="")
	tkinsert( base.txt, "end", mess4)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", nl1)
    }
    else {
      # trend component indicated

      trend.select <- as.numeric( tkcurselection( trend.list))+1 
      mess <- paste("Trend variable: ",
		colnames( dd$data)[trend.select],"\n")
      tkinsert( base.txt, "end", mess)
      mess <- paste( "Response variable: ", colnames(dd$data)[resp.select],"\n\n")
      tkinsert( base.txt, "end", mess)
     
      response<-dd$data[,resp.select]
	cnames <- colnames( dd$data)[-resp.select]
	trend<- dd$data[,cnames[trend.select]]

	# Save fit in 'models' component of list.
number.of.models <- length( dd$models)
names.of.models <- names( dd$models)
if( is.null( names.of.models)) names.of.models <- character(0)
jj <- 0
if( number.of.models > 0) for( i in 1:number.of.models) if( class( dd$models[[i]])[1] == "glm") jj <- jj+1
names.of.models <- c( names.of.models, paste( "poisson.fit", jj+1, sep=""))

# fit the Poisson distribution.
	mod.fit <- glm( response~trend, family=poisson())
	# Store the fit in the 'models' component of 'ev.data' object.
	dd$models[[number.of.models+1]] <- mod.fit
	if( tclvalue( diags.value) == 1) {
		plot( mod.fit$residuals, type="l")
		abline(h=0, lty=2)
		}
names( dd$models) <- names.of.models
	nl1 <- paste(" ", "**********", " ", sep="\n")
	nl2 <- paste( " ", " ", sep="\n")
#	msg1 <- paste( "Call: ", deparse( mod.fit$call), sep="")
no.of.pars <- length( mod.fit$coef)
summ.fit <- summary( mod.fit)$coef
	tkinsert( base.txt, "end", nl2)
tkinsert( base.txt, "end", paste(".........", " "))
# tkinsert( base.txt, "end", paste(  "Estimate", "    ", "Std. Error", "    ",
#	"z-value", "    ", "P(>|z|)", sep=""))
tkinsert( base.txt, "end", paste(  "Estimate", "    ", "Std. Error", sep=""))
tkinsert( base.txt, "end", nl2)
coef.names <- c("Intercept", cnames[trend.select])
nchar1 <- nchar( coef.names[2])
if( nchar1 < 9) for( j in 1:(9-nchar1)) coef.names[2] <- paste( coef.names[2], ".", sep="")
else if( nchar1 > 9) for( j in 1:(nchar1-9)) coef.names[1] <- paste( coef.names[1], ".", sep="")

for( i in 1:no.of.pars) {
	tkinsert( base.txt, "end", paste( coef.names[i], " ", sep=""))
#	summ.msg <- paste( round( summ.fit[i,1], digits=6), "    ",
#			round( summ.fit[i,2], digits=6), "    ",
#			round( summ.fit[i,3], digits=6), "    ",
#			round( summ.fit[i,4], digits=6),  sep="")
	summ.msg <- paste( round( summ.fit[i,1], digits=6), "    ",
			round( summ.fit[i,2], digits=6), "    ", sep="")
	tkinsert( base.txt, "end", summ.msg)
tkinsert( base.txt, "end", nl2)
	} # end of for i loop

msg3 <- paste( "Residual degrees of freedom: ", mod.fit$df.residual, sep="")
lr <- mod.fit$null.deviance - mod.fit$deviance
pval <- pchisq( lr, 1, lower.tail=FALSE)
msg4 <- paste( "negative log-likelihood (proportional): ",
		round( mod.fit$deviance, digits=4), sep="")
msg5 <- paste( "likelihood-ratio (against null model): ", round( lr, digits=4), 
		" (p-value = ", round( pval, digits=4), ")", sep="")

# msg4 <- paste( "Null deviance: ", round( mod.fit$null.deviance, digits=4),
#		sep="")
# msg5 <- paste( "Residual deviance: ",
#		round( mod.fit$deviance, digits=4), sep="")
# msg6 <- paste( "AIC: ", round( mod.fit$aic, digits=2), sep="")
	
#	tkinsert( base.txt, "end", nl1)
#	tkinsert( base.txt, "end", msg1)
#	tkinsert( base.txt, "end", nl2)
#	tkinsert( base.txt, "end", msg2)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg3)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg4)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", msg5)
	tkinsert( base.txt, "end", nl2)
	tkinsert( base.txt, "end", nl1)
#	tkinsert( base.txt, "end", msg6)
#	tkinsert( base.txt, "end", nl1)
	
    }
	tkyview.moveto(base.txt,1.0) 
    tkdestroy(base)
    tkconfigure(base.txt,state="disabled") 
	 if( is.nothing) assign( ".ev", dd, pos=".GlobalEnv")
        else assign( full.list[ data.select], dd, pos=".GlobalEnv")
} # end of submit fcn

refresh <- function() {
	tkdelete( resp.listbox, 0.0, "end")
	tkdelete( trend.list, 0.0, "end")
	data.select <- as.numeric( tkcurselection( data.listbox))+1
	if( is.nothing) dd <- .ev
	else dd <- get( full.list[ data.select])
	
	for( i in colnames( dd$data)) {
		tkinsert( resp.listbox, "end", i)
		tkinsert( trend.list, "end", i)
		} # end of for i loop
	invisible()
	} # end of refresh fcn

redolists <- function() {

data.select <- as.numeric( tkcurselection( data.listbox))+1
        if( is.nothing) dd <- .ev
        else dd <- get( full.list[ data.select])
	
    resp.name<-colnames(dd$data)[as.numeric(tkcurselection(resp.listbox))+1]
 
    # put the correct eligible covariates in the other list boxes
    tkdelete(trend.list, 0.0,"end")
 
    for (i in colnames(dd$data)) {
      if (i != resp.name) {
        tkinsert(trend.list,"end",i)
      }
    }
} # end of redolists fcn

poissonhelp <- function() {
	tkconfigure( base.txt, state="normal")
	help.msg <- paste( " ",
"Uses the glm function.  See the help file for this function.", " ", sep="\n")
	tkinsert( base.txt, "end", help.msg)
	tkconfigure( base.txt, state="disabled")

	help( glm)
	invisible()
	} # end of poissonhelp fcn

endprog <- function() {
    tkdestroy(base)

  }

##################################
# Frame/ button setup
##################################


base<-tktoplevel()
tkwm.title(base,"Fit Poisson Distribution")

data.frm <- tkframe( base, borderwidth=2, relief="groove") 
top.frm <- tkframe( base, borderwidth=2, relief="groove")
mid.frm <- tkframe( base, borderwidth=2, relief="groove")
trend.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
diag.frm <- tkframe( mid.frm, borderwidth=2, relief="flat")
bot.frm <- tkframe( base, borderwidth=2, relief="groove")

# data frame to select a data object.

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
	if( (class( get( temp[i]))[1] == "ev.data")) {
		tkinsert( data.listbox, "end", paste( temp[i]))
		full.list <- c( full.list, temp[i])
		is.nothing <- FALSE
		} # end of if class stmt
	} # end of for i loop

tkpack( tklabel( data.frm, text="Data Object:  ", padx=4), side="left")
tkpack( data.listbox, data.scroll, side="left", fill="y")
tkpack( data.frm, fill="x")

# place bindings on data.listbox in order to update response info.
tkbind( data.listbox, "<Button-1>", "")
tkbind( data.listbox, "<ButtonRelease-1>", refresh)

# top frame for response variable
 
top.l <- tkframe( top.frm,borderwidth=2)
resp.listbox <- tklistbox( top.l,
			yscrollcommand=function(...)tkset(resp.scroll,...),
			selectmode="single",
			width=15,
			height=6,
			exportselection=0)
resp.scroll <- tkscrollbar( top.l,orient="vert",
			command=function(...)tkyview(resp.listbox,...))

if( is.nothing) { 
  for (i in 1:ncol(.ev$data)) {
    tkinsert(resp.listbox,"end",paste(colnames(.ev$data)[i]))
  }
	} else tkinsert( resp.listbox, "end", "")

 
  tkpack(tklabel(top.l,text="Response:      ",padx=4), side="left")
  tkpack(resp.listbox, resp.scroll, side="left", fill="y")

  tkpack(top.l,side="left",fill="x") 

  # place binding on resp.listbox to eliminate the
  # response from the lists of covs
tkbind( resp.listbox, "<Button-1>", "")
  tkbind(resp.listbox,"<ButtonRelease-1>",redolists)

  # choose the trend variable
  trend.l<-tkframe( trend.frm, borderwidth=2)
  trend.list<-tklistbox(trend.l,yscrollcommand=function(...)tkset(trend.covscr,...),selectmode="multiple",width=15,height=6,exportselection=0)
  trend.covscr <- tkscrollbar(trend.l,orient="vert",command=function(...)tkyview(trend.list,...))

if( is.nothing) {
  for (i in 1:ncol(.ev$data)) {
    tkinsert(trend.list,"end",paste(colnames(.ev$data)[i]))
  }
	} else tkinsert( trend.list, "end", "")

  tkpack(tklabel(trend.l,text="Covariate (log link):",padx=4), side="left")
  tkpack(trend.list,side="left")
  tkpack(trend.covscr,side="right",fill="y")
 
  tkpack(trend.l,side="right")

# plot diagnostics frame
diags.button <- tkcheckbutton( diag.frm, text="plot diagnostics",
				variable=diags.value)
tkpack( diags.button)
tkpack( trend.frm, diag.frm, side="left")

  # bottom frame
	ok.but <- tkbutton( bot.frm, text="OK", command=submit)
	cancel.but <- tkbutton( bot.frm, text="Cancel", command=endprog)
	help.but <- tkbutton( bot.frm, text="Help", command=poissonhelp)
 
	tkpack( ok.but, cancel.but, side="left")
	tkpack( help.but, side="right")

# place bindings on buttons.
	tkbind( ok.but, "<Return>", submit)
	tkbind( cancel.but, "<Return>", endprog)
	tkbind( help.but, "<Return>", poissonhelp)

  tkpack(top.frm, fill="x")
  tkpack(mid.frm)
  tkpack(bot.frm)
}
