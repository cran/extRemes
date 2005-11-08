"extremes.gui" <-
function () 
{
ev.dataexists <- function() {
	tmp1 <- ls( all=TRUE, pos=".GlobalEnv")
	n <- length( tmp1)
# If a data object of class "extRemesDataObject" exists, will return 1.
# Otherwise, will return 0.
data.exists <- FALSE
	for( i in 1:n) {
		if( is.null( class( get( tmp1[i])))) next
		if( class( get( tmp1[i]))[1] == "extRemesDataObject") {
			data.exists <- TRUE
			break
			}
		} # end of for i loop
if( !data.exists) {
	mess <- paste("Must load or simulate a data set. \n\n")
	tkconfigure(txt, state = "normal")
	tkinsert(txt, "end", mess)
	tkyview.moveto(txt, 1)
	tkconfigure(txt, state = "disabled")
	return(0)
	} else return(1)
} # end of ev.dataexists fcn

	savework <- function() {
		cat( "\n", "Saving R workspace ...\n")
		cat( "This may take a few moments if the workspace is large.\n") 
		saveCMD <- "save.image()"
		eval( parse( text=saveCMD))
		write( saveCMD, file="extRemes.log", append=TRUE)
		cat( "\n", "Workspace saved.\n")
		invisible()
	}

    endprog <- function() {
        tkdestroy(base)
    }

    readdata <- function() {
        load.data(txt)
    } # end of readdata fcn

    viewdata <- function() {
        if (ev.dataexists()) 
            view.data(txt)
    } # end of viewdata fcn

    gevsimdata <- function() {
        gevsim.gui(txt)
        tkyview.moveto(txt, 1)
    } # end of gevsimdata fcn

gpdsimdata <- function() {
        gpdsim.gui(txt)
        tkyview.moveto(txt, 1)
    } # end of gpdsimdata fcn

	decluster <- function() {
		if( ev.dataexists()) {
                        decluster.gui(txt)
                        tkyview.moveto(txt, 1)
                        }
		} # end of the decluster fcn
	
	negtrans <- function() {
		if( ev.dataexists()) {
                        negtrans.gui(txt)
                        tkyview.moveto(txt, 1)
                	}
		} # end of negtrans fcn

	logtrans <- function() {
		if( ev.dataexists()) {
			logtrans.gui(txt)
			tkyview.moveto(txt, 1)
		}
	} # end of logtrans function

	logdr <- function() {
		if( ev.dataexists()) {
			logdr.gui( txt)
			tkyview.moveto( txt, 1)
		}
	} # end of logdr function
	
	affinetrans <- function() {
		if( ev.dataexists()) {
                        affine.gui( txt)
                        tkyview.moveto( txt, 1)
			}
	} # end of affinetrans fcn
	
	indicatortrans <- function() {
                if( ev.dataexists()) {
                        indicatorTransform.gui( txt)
                        tkyview.moveto( txt, 1)
                        }
        } # end of indicatortrans fcn

	trigtrans <- function() {
		if( ev.dataexists()) {
			trigtrans.gui(txt)
			tkyview.moveto( txt, 1)
			}
	} # end of trigtrans fcn

	DataSummary <- function() {
		if( ev.dataexists()) {
			DataSummaryGUI(txt)
			tkyview.moveto( txt, 1)
			}
		} # end of DataSummary fcn.

	scrubber <- function() {
		if( ev.dataexists()) {
                        scrubber.gui(txt)
                        tkyview.moveto( txt, 1)
                        }
	} # end of scrubber function

	clearlogfile <- function() {
		clearlog(txt)
	} # end of clearlogfile function

    gevfit <- function() {
        if (ev.dataexists()) {
            gevf.gui(txt)
            tkyview.moveto(txt, 1)
        }
    } # end of gevfit fcn

	rlargfit <- function() {
		if (ev.dataexists()) {
            rlargf.gui(txt)
            tkyview.moveto(txt, 1)
		}
	} # end of rlargfit fcn

    poissonfit <- function() {
        if (ev.dataexists()) {
            poisson.gui(txt)
            tkyview.moveto(txt, 1)
        }
    } # end of poissonfit fcn

	gpdfit <- function() {
		if( ev.dataexists()) {
			gpdf.gui(txt)
			tkyview.moveto( txt, 1)
			}
	} # end of gpdfit fcn

	ppfit <- function() {
		if( ev.dataexists()) {
                        ppfit.gui(txt)
                        tkyview.moveto( txt, 1)
                        }
	} # end of ppfit fcn

	extremalind <- function() {
		if( ev.dataexists()) {
			extremalind.gui(txt)
			tkyview.moveto( txt, 1)
			}
		} # end of extremalind fcn

	deviancecomparison <- function() {
		if( ev.dataexists()) {
			llhrt.gui(txt)
			tkyview.moveto( txt, 1)
			}
		} # end of deviancecomparison fcn

	plotdata <- function() {
		if( ev.dataexists()) {
		scatterplot.gui( txt)
		tkyview.moveto( txt, 1)
		}
	} # end of plotdata fcn

#	plotTS <- function() {
#                if( ev.dataexists()) {
#                plotTS.gui( txt)
#                tkyview.moveto( txt, 1)
#                }
#        } # end of plotTS fcn

	mrlplot <- function() {
		if( ev.dataexists()) {
			mrlplot.gui( txt)
			tkyview.moveto( txt, 1)
			}
		} # end of mrlplot fcn

	gpdfitrange <- function() {
		if( ev.dataexists()) {
			gpdfitrange.gui( txt)
			tkyview.moveto( txt, 1)
			}
		} # end of gpdfitrange fcn

	ppfitrange <- function() {
		if( ev.dataexists()) {
                        ppfitrange.gui( txt)
                        tkyview.moveto( txt, 1)
			}
		} # end of ppfitrange fcn

	fitdiag <- function() {
		if( ev.dataexists()) {
                        fitdiag.gui( txt)
                        tkyview.moveto( txt, 1)
                        }
		} # end of fitdiag fcn
	
	histplot <- function() {
		if( ev.dataexists()) {
			histogram.gui( txt)
			tkyview.moveto( txt, 1)
			}
		} # end of histplot fcn

	rlplot <- function() {
                if( ev.dataexists()) {
                        rlplot.gui( txt)
                        tkyview.moveto( txt, 1)
                        }
                } # end of rlplot fcn

	gevparamCI <- function() {
		if( ev.dataexists()) {
			gevparamCI.gui( txt)
			tkyview.moveto( txt, 1)
			}
		} # end of gevparamCI fcn

	gpdparamCI <- function() {
                if( ev.dataexists()) {
                        gpdparamCI.gui( txt)
                        tkyview.moveto( txt, 1)
                        }
                } # end of gpdparamCI fcn

	fitsummary <- function() {
		if( ev.dataexists()) {
		fitsummary.gui( txt)
		tkyview.moveto( txt, 1)
		}
	} # end of fitsummary fcn

    base <- tktoplevel()
    tkwm.title(base, "Extremes Toolkit: version 1.51")
    top.frm <- tkframe(base, borderwidth = 2)
    bottom.frm <- tkframe(base, borderwidth = 2)

# Menu buttons...

# File menu...
fmenu.but <- tkmenubutton(top.frm, text = "File", relief = "raised", 
        borderwidth = 2)
	file.menu <- tkmenu(fmenu.but)
	tkconfigure(fmenu.but, menu = file.menu)
	tkadd(file.menu, "command", label = "Read Data", command = readdata)
	SimMenu <- tkmenu( file.menu, tearoff=FALSE)
	tkadd(SimMenu, "command", label = "Generalized Extreme Value (GEV)", command=gevsimdata)
	tkadd(SimMenu, "command", label="Generalized Pareto (GP)", command=gpdsimdata)
	tkadd( file.menu, "cascade", label="Simulate Data", menu=SimMenu)

#	tkadd( file.menu, "separator")
#	tkadd(file.menu, "command", label = "View Data", command = viewdata)
	tkadd( file.menu, "separator")
	tkadd( file.menu, "command", label="Decluster", command=decluster)
	TransMenu <- tkmenu( file.menu, tearoff=FALSE)
	tkadd( TransMenu, "command", label="Negative", command=negtrans)
	tkadd( TransMenu, "command", label="Logarithm", command=logtrans)
	tkadd( TransMenu, "command", label="Log Daily Returns", command=logdr)
	tkadd( TransMenu, "command", label="Affine Transformation",
							command=affinetrans)
	tkadd( TransMenu, "command", label="Indicator Transformation",
                                                        command=indicatortrans)
	tkadd( TransMenu, "command", label="Trigonometric Transformation",
							command=trigtrans)
	tkadd( file.menu, "cascade", label="Transform Data", menu=TransMenu)

	tkadd( file.menu, "separator")
	tkadd( file.menu, "command", label="Data Summary", command=DataSummary)
	tkadd( file.menu, "separator")
	tkadd( file.menu, "command", label="Scrubber", command=scrubber)
	tkadd( file.menu, "command", label="Clear log file", command=clearlogfile)
	tkadd( file.menu, "separator")
	tkadd( file.menu, "command", label="Save", command=savework)
	tkadd( file.menu, "separator")
	tkadd(file.menu, "command", label = "Exit", command = endprog)

# Plot menu...

pmenu.but <- tkmenubutton( top.frm, text="Plot", relief="raised", borderwidth=2)
plot.menu <- tkmenu( pmenu.but)
tkconfigure( pmenu.but, menu=plot.menu)
tkadd( plot.menu, "command", label = "Scatter Plot", command = plotdata)
# tkadd( plot.menu, "command", label = "Time Series", command = plotTS)
tkadd( plot.menu, "command", label="Mean Residual Life Plot", command=mrlplot)
tkadd( plot.menu, "separator")
tkadd( plot.menu, "command", label="Fit threshold ranges (GPD)", command=gpdfitrange)
tkadd( plot.menu, "command", label="Fit threshold ranges (PP)", command=ppfitrange)
tkadd( plot.menu, "separator")
# DiagMenu <- tkmenu( plot.menu, tearoff=FALSE)
# tkadd( DiagMenu, "command", label="GEV Fit Diagnostics", command=gevdiag)
# tkadd( DiagMenu, "command", label="GPD Fit Diagnostics", command=gpddiag)
# tkadd( DiagMenu, "separator")
tkadd( plot.menu, "command", label="Fit Diagnostics", command=fitdiag)
tkadd( plot.menu, "command", label="Fit with Histogram", command=histplot)
tkadd( plot.menu, "command", label="Return Level Plot", command=rlplot)

# Analyze menu...
amenu.but <- tkmenubutton(top.frm, text = "Analyze", relief = "raised", 
	        borderwidth = 2)
	ana.menu <- tkmenu(amenu.but)
	tkconfigure(amenu.but, menu = ana.menu)
	tkadd( ana.menu, "command",
		label="Generalized Extreme Value (GEV) Distribution ",
		command=gevfit)
	tkadd( ana.menu, "command", label="r-th Largest Order Statistics Model",
		command=rlargfit)
	tkadd( ana.menu, "command", label="Poisson Distribution",
		command=poissonfit)
	tkadd( ana.menu, "command",
		label="Generalized Pareto Distribution (GPD)",
				command=gpdfit)
	tkadd( ana.menu, "command", label="Point Process Model", command=ppfit)

	tkadd( ana.menu, "separator")

	CImenu <- tkmenu( ana.menu, tearoff=FALSE)
	tkadd( CImenu, "command", label="GEV fit", command=gevparamCI)
	tkadd( CImenu, "command", label="GPD fit", command=gpdparamCI)
	tkadd( ana.menu, "cascade", label="Parameter Confidence Intervals", menu=CImenu)

	tkadd( ana.menu, "command", label="Likelihood-ratio test", command=deviancecomparison)
	tkadd( ana.menu, "separator")
	tkadd( ana.menu, "command", label="Fit Summary", command=fitsummary)
	tkadd( ana.menu, "separator")
	tkadd( ana.menu, "command", label="Extremal Index", command=extremalind)
	tkpack(top.frm, side = "top", fill = "x")
	tkpack(fmenu.but, pmenu.but, amenu.but, side = "left")

# Base frame...  (for text messages)
	# txt <- tktext(bottom.frm, bg = "white", font = "courier")
	txt <- tktext(bottom.frm, bg = "gray", font = "courier")
	# scr <- tkscrollbar(bottom.frm, repeatinterval = 5,
	# 			command = function(...) tkyview(txt, ...))
	# tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
	tkconfigure(txt, state = "disabled")
	tkpack(txt, side = "left", fill = "both")
	# tkpack(scr, side = "right", fill = "y")
	tkpack(bottom.frm, side = "bottom")
	write.extRemesMainMessage( txt=txt)
} # end of 'extremes.gui' function
