rlargf.gui <- function (base.txt) {
first.time <- TRUE
    plot.diags <- tclVar(0)
    r.value <- tclVar("")
    mu.link <- tclVar("identity")
    sig.link <- tclVar("identity")
    gam.link <- tclVar("identity")
    refresh <- function() {
        tkdelete(resp.listbox, 0, "end")
        tkdelete(mu.covlist, 0, "end")
        tkdelete(sig.covlist, 0, "end")
        tkdelete(gam.covlist, 0, "end")
        if (!is.nothing) {
            data.select <- as.numeric(tkcurselection(data.listbox)) + 
                1
            dd <- get(full.list[data.select])
        }
        else dd <- extRemesData
        for (i in 1:ncol(dd$data)) tkinsert(resp.listbox, 
            "end", paste(colnames(dd$data)[i]))
        for (i in 1:ncol(dd$data)) tkinsert(mu.covlist, 
            "end", paste(colnames(dd$data)[i]))
        for (i in 1:ncol(dd$data)) tkinsert(sig.covlist, 
            "end", paste(colnames(dd$data)[i]))
        for (i in 1:ncol(dd$data)) tkinsert(gam.covlist, 
            "end", paste(colnames(dd$data)[i]))
    }
    redolists <- function() {
        if (!is.nothing) {
		if( first.time) data.select <- numeric(0)
		data.select <- c(data.select, as.numeric(tkcurselection(data.listbox)) + 1)
            dd <- get(full.list[data.select])
        }
        else dd <- extRemesData
        resp.name <- colnames(dd$data)[as.numeric(tkcurselection(resp.listbox)) + 
            1]
        tkdelete(mu.covlist, 0, "end")
        tkdelete(sig.covlist, 0, "end")
        tkdelete(gam.covlist, 0, "end")
        for (i in colnames(dd$data)) {
            if (i != resp.name) {
                tkinsert(mu.covlist, "end", i)
                tkinsert(sig.covlist, "end", i)
                tkinsert(gam.covlist, "end", i)
	first.time <- FALSE
            }
        }
    }
    submit <- function() {
        if (!is.nothing) {
            data.select <- as.numeric(tkcurselection(data.listbox)) + 1
            dd.cmd <- paste( "dd <- get( \"", full.list[data.select], "\")", sep="")
        } else dd.cmd <- "dd <- extRemesData"
	eval( parse( text=dd.cmd))
	write( dd.cmd, file="extRemes.log", append=TRUE)

        cov.names.cmd <- "cov.names <- character(0)"
	eval( parse( text=cov.names.cmd))
	write( cov.names.cmd, file="extRemes.log", append=TRUE)

        resp.select <- as.numeric(tkcurselection(resp.listbox)) + 1
        if (is.na(resp.select)) return()
        # tkconfigure(base.txt, state = "normal")
        nl1 <- paste("  ", "************", "   ", "   ", sep = "\n")
        nl2 <- paste("   ", " ", sep = "\n")
        # tkinsert(base.txt, "end", nl2)
	cat( nl2)
	cat( nl1)
        # tkinsert(base.txt, "end", nl1)
        # tkinsert(base.txt, "end", paste("r-th largest model fit \n"))
	cat( paste("r-th largest model fit \n"))
        # tkinsert(base.txt, "end", paste("-----------------------------------\n"))
	cat( paste("-----------------------------------\n"))
        # tkinsert(base.txt, "end", paste("Response variable:", 
         #    colnames(dd$data)[resp.select], "\n"))
	cat( paste("Response variable:", colnames(dd$data)[resp.select], "\n"))
        # tkinsert(base.txt, "end", nl2)
        # covs.cmd <- "covs <- NULL"
	# eval( parse( text=covs.cmd))
	# write( covs.cmd, file="extRemes.log", append=TRUE)
        cur.cov.cols.cmd <- "cur.cov.cols <- 0"
	eval( parse( text=cur.cov.cols.cmd))
	# write( cur.cov.cols.cmd, file="extRemes.log", append=TRUE)
	mu.cov.cols.cmd <- "mu.cov.cols <- NULL"
        eval( parse( text=mu.cov.cols.cmd))
        write( mu.cov.cols.cmd, file="extRemes.log", append=TRUE)
        if (tclvalue(tkcurselection(mu.covlist)) != "") {
            temp.cols <- as.numeric(strsplit(tkcurselection(mu.covlist), " ")[[1]])
            cov.selected.cmd <- "cov.selected <- character(0)"
		eval( parse( text=cov.selected.cmd))
		write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            for (i in temp.cols) {
                cov.selected.cmd <- paste( "cov.selected <- c(cov.selected, \"", tclvalue(tkget(mu.covlist, i)), "\")",
						sep="")
		eval( parse( text=cov.selected.cmd))
                write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            }
            print(paste("cov.selected = ", cov.selected, sep = ""))
            dat.cols.cmd <- "dat.cols <- numeric(0)"
		eval( parse( text=dat.cols.cmd))
		write( dat.cols.cmd, file="extRemes.log", append=TRUE)
            for (j in 1:length(colnames(dd$data))) {
                for (i in cov.selected) {
                  if (i == colnames(dd$data)[j]) {
                    dat.cols.cmd <- paste( "dat.cols <- c(dat.cols, ", j, ")", sep="")
			eval( parse( text=dat.cols.cmd))
			write( dat.cols.cmd, file="extRemes.log", append=TRUE)
                  }
                }
            }
	covs.cmd <- "covs <- NULL"
	eval( parse( text=covs.cmd))
	write( covs.cmd, file="extRemes.log", append=TRUE)
	# covs <- cbind(covs, as.matrix(dd$data[, dat.cols]))
	# covs.cmd <- paste( "covs <- cbind( covs, as.matrix( ", full.list[ data.select], "$data[, dat.cols]))", sep="")
	covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
	eval( parse( text=covs.cmd))
	write( covs.cmd, file="extRemes.log", append=TRUE)
        mu.cov.cols.cmd <- paste( "mu.cov.cols <- ", cur.cov.cols + 1, ":", length(dat.cols) + cur.cov.cols, sep="")
	eval( parse( text=mu.cov.cols.cmd))
        write( mu.cov.cols.cmd, file="extRemes.log", append=TRUE)
	cur.cov.cols <- cur.cov.cols + length(dat.cols)
	# cov.names <- c(cov.names, colnames(dd$data)[dat.cols])
	# cov.names.cmd <- paste( "cov.names <- c( cov.names, colnames( ", full.list[ data.select], "$data)[dat.cols])",
	# 			sep="")
	cov.names.cmd <- "cov.names <- c( cov.names, colnames( dd[[\"data\"]])[ dat.cols])"
	eval( parse( text=cov.names.cmd))
        write( cov.names.cmd, file="extRemes.log", append=TRUE)
        }
        sig.cov.cols.cmd <- "sig.cov.cols <- NULL"
	eval( parse( text=sig.cov.cols.cmd))
	write( sig.cov.cols.cmd, file="extRemes.log", append=TRUE)
        if (tclvalue(tkcurselection(sig.covlist)) != "") {
            temp.cols <- as.numeric(strsplit(tkcurselection(sig.covlist), " ")[[1]])
            cov.selected.cmd <- "cov.selected <- character(0)"
		eval( parse( text=cov.selected.cmd))
		write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            for (i in temp.cols) {
                cov.selected.cmd <- paste( "cov.selected <- c(cov.selected, \"", tclvalue(tkget(sig.covlist, i)), "\")",
					sep="")
		eval( parse( text=cov.selected.cmd))
                write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            }
            dat.cols.cmd <- "dat.cols <- numeric(0)"
		eval( parse( text=dat.cols.cmd))
		write( dat.cols.cmd, file="extRemes.log", append=TRUE)
            for (j in 1:length(colnames(dd$data))) {
                for (i in cov.selected) {
                  if (i == colnames(dd$data)[j]) {
			dat.cols.cmd <- paste( "dat.cols <- c(dat.cols, ", j, ")", sep="")
			eval( parse( text=dat.cols.cmd))
			write( dat.cols.cmd, file="extRemes.log", append=TRUE)
                  }
                }
            }
            # covs <- cbind(covs, as.matrix(dd$data[, dat.cols]))
	# 	covs.cmd <- paste( "covs <- cbind( covs, as.matrix( ", full.list[ data.select], "$data[, dat.cols]))",
	# 				sep="")
		covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
		eval( parse( text=covs.cmd))
		write( covs.cmd, file="extRemes.log", append=TRUE)
            sig.cov.cols.cmd <- paste( "sig.cov.cols <- ", cur.cov.cols + 1, ":", length(dat.cols) + cur.cov.cols, sep="")
		eval( parse( text=sig.cov.cols.cmd))
		write( sig.cov.cols.cmd, file="extRemes.log", append=TRUE)
            cur.cov.cols <- cur.cov.cols + length(dat.cols)
#            cov.names <- c(cov.names, colnames(dd$data)[dat.cols])
# cov.names.cmd <- paste( "cov.names <- c( cov.names, colnames( ", full.list[ data.select], "$data)[dat.cols])", sep="")
		cov.names.cmd <- "cov.names <- c( cov.names, colnames( dd[[\"data\"]])[ dat.cols])"
		eval( parse( text=cov.names.cmd))
		write( cov.names.cmd, file="extRemes.log", append=TRUE)
        }
        gam.cov.cols.cmd <- "gam.cov.cols <- NULL"
	eval( parse( text=gam.cov.cols.cmd))
	write( gam.cov.cols.cmd, file="extRemes.log", append=TRUE)
        if (tclvalue(tkcurselection(gam.covlist)) != "") {
            temp.cols <- as.numeric(strsplit(tkcurselection(gam.covlist), " ")[[1]])
            cov.selected.cmd <- "cov.selected <- character(0)"
		eval( parse( text=cov.selected.cmd))
		write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            for (i in temp.cols) {
	cov.selected.cmd <- paste( "cov.selected <- c(cov.selected, \"", tclvalue(tkget(gam.covlist, i)), "\")", sep="")
		eval( parse( text=cov.selected.cmd))
                write( cov.selected.cmd, file="extRemes.log", append=TRUE)
            }
            dat.cols.cmd <- "dat.cols <- numeric(0)"
		eval( parse( text=dat.cols.cmd))
		write( dat.cols.cmd, file="extRemes.log", append=TRUE)
            for (j in 1:length(colnames(dd$data))) {
                for (i in cov.selected) {
                  if (i == colnames(dd$data)[j]) {
                    dat.cols.cmd <- paste( "dat.cols <- c(dat.cols, ", j, ")", sep="")
			eval( parse( text=dat.cols.cmd))
                write( dat.cols.cmd, file="extRemes.log", append=TRUE)
                  }
                }
            }
            # covs <- cbind(covs, as.matrix(dd$data[, dat.cols]))
	# 	covs.cmd <- paste( "covs <- cbind( covs, as.matrix( ", full.list[ data.select], "$data[, dat.cols]))",
         #                                sep="")
		covs.cmd <- "covs <- cbind( covs, as.matrix( dd[[\"data\"]][, dat.cols]))"
		eval( parse( text=covs.cmd))
                write( covs.cmd, file="extRemes.log", append=TRUE)
            gam.cov.cols.cmd <- paste( "gam.cov.cols <- ", cur.cov.cols + 1, ":", length(dat.cols) + cur.cov.cols, sep="")
		eval( parse( text=gam.cov.cols.cmd))
		write( gam.cov.cols.cmd, file="extRemes.log", append=TRUE)
            cur.cov.cols <- cur.cov.cols + length(dat.cols)
            # cov.names <- c(cov.names, colnames(dd$data)[dat.cols])
# cov.names.cmd <- paste( "cov.names <- c( cov.names, colnames( ", full.list[ data.select], "$data)[dat.cols])", sep="")
		cov.names.cmd <- "cov.names <- c( cov.names, colnames(dd[[\"data\"]][ dat.cols])"
		eval( parse( text=cov.names.cmd))
                write( cov.names.cmd, file="extRemes.log", append=TRUE)
        }
        if (tclvalue(mu.link) == "identity") {
            mlinker.cmd <- "m.linker <- identity"
        } else {
            mlinker.cmd <- "m.linker <- exp"
        }
        if (tclvalue(sig.link) == "identity") {
            sig.linker.cmd <- "sig.linker <- identity"
        } else {
            sig.linker.cmd <- "sig.linker <- exp"
        }
        if (tclvalue(gam.link) == "identity") {
            gam.linker.cmd <- "gam.linker <- identity"
        }
        else {
            gam.linker.cmd <- "gam.linker <- exp"
        }
	eval( parse( text=mlinker.cmd))
	write( mlinker.cmd, file="extRemes.log", append=TRUE)
	eval( parse( text=sig.linker.cmd))
	write( sig.linker.cmd, file="extRemes.log", append=TRUE)
	eval( parse( text=gam.linker.cmd))
	write( gam.linker.cmd, file="extRemes.log", append=TRUE)

        method.list <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
        method.select <- as.numeric(tkcurselection(method.listbox)) + 1
	if( length( method.select) == 0) {
                cat( paste( "No optimization method selected.  Using \"Nelder-Mead\"",
                        "(use \'help( optim)\' for more details)"), sep="\n")
                method.value.cmd <- "method.value <- \"Nelder-Mead\""
} else method.value.cmd <- paste( "method.value <- \"", method.list[method.select], "\"", sep="")
	eval( parse( text=method.value.cmd))
	write( method.value.cmd, file="extRemes.log", append=TRUE)
	r.val <- tclvalue( r.value)
	if( r.val == "") r.val <- length( resp.select)
	else r.val <- as.numeric( r.val)

number.of.models <- length( dd$models)
names.of.models <- names( dd$models)
if( is.null( names.of.models)) names.of.models <- character(0)
jj <- 0
if( number.of.models > 0) for( i in 1:number.of.models) if( class( dd$models[[i]]) == "rlarg.fit") jj <- jj+1
names.of.models <- c( names.of.models, paste( "rlarg.fit", jj+1, sep=""))

# dd$models[[number.of.models+1]] <- rlarg.fit(dd$data[, resp.select], r=r.val,
           # ydat = covs, mul = mu.cov.cols, mulink = m.linker, 
           # sigl = sig.cov.cols, siglink = sig.linker, shl = gam.cov.cols, 
           # shlink = gam.linker, method = method.value)
	if( length( resp.select) > 1) {
		respos <- character(0)
		for( i in 1:(length( resp.select)-1)) respos <- paste( respos, resp.select[i], ", ", sep="")
		respos <- paste( "c( ", respos, resp.select[ length( resp.select)], ")", sep="")
	} else respos <- resp.select 
fitCMD <- paste( "dd[[\"models\"]][[\"rlarg.fit", jj+1, "\"]] <- fit <- rlarg.fit( dd[[\"data\"]][, ",
	respos, "], r=", r.val,
	", ydat = covs, mul = ", ifelse( is.null( mu.cov.cols), "NULL", "mu.cov.cols"), ", mulink = m.linker, ",
	"sigl = ", ifelse( is.null( sig.cov.cols), "NULL", "sig.cov.cols"), ", siglink = sig.linker, ",
	"shl = ", ifelse( is.null( gam.cov.cols), "NULL", "gam.cov.cols"), ", shlink = gam.linker, method = method.value)",
	sep="")
	eval( parse( text=fitCMD))
	write( fitCMD, file="extRemes.log", append=TRUE)
	# eval( parse( text=fitCMD))
# dd$models[[number.of.models+1]] <- fit
# classCMD <- "class( fit) <- \"rlarg.fit\""
classCMD <- paste( "class( dd[[\"models\"]][[\"rlarg.fit", jj+1, "\"]]) <- \"rlarg.fit\"", sep="")
eval( parse( text=classCMD))
write( classCMD, file="extRemes.log", append=TRUE)

if (is.null(dd$models[[number.of.models+1]])) {
            fail.str <- paste(" ", "Fit failed.", " ", sep="\n") 
            # tkinsert(base.txt, "end", fail.str)
		cat( fail.str)
            tkyview.moveto(base.txt, 1)
        }
        else {
            # fit <- dd$models[[number.of.models+1]]
            links <- c(tclvalue(mu.link), tclvalue(sig.link), tclvalue(gam.link))
            # tkconfigure(base.txt, state = "normal")
            nl2 <- paste("   ", "   ", sep = "\n")
            # tkinsert(base.txt, "end", nl2)
		cat( nl2)
            if (fit$conv == 0) CONV.msg <- paste("Convergence successfull!")
            else if (fit$conv == 1) CONV.msg <- paste("Iteration limit exceeded.", "Did not convergence.", sep = "\n")
            else if (fit$conv == 51 | fit$conv == 52) CONV.msg <- paste(fit$message)
	else CONV.msg <- paste("Convergence: ", fit$conv, " (See help for optim for more info).", sep="")
            # tkinsert(base.txt, "end", CONV.msg)
		cat( CONV.msg)
            # tkinsert(base.txt, "end", nl2)
		cat( nl2)
            c1 <- round(cbind(fit$mle, fit$se), digits = 5)
            colnames(c1) <- c("MLE", "Stand. Err.")
            rnames <- c(paste("MU:\t(", links[1], ")", sep = ""))
            if (!is.null(fit$model[[1]])) 
                rnames <- c(rnames, paste(cov.names[fit$model[[1]]], 
                  ": (", links[1], ")", sep = ""))
            rnames <- c(rnames, paste("SIGMA: (", links[2], ")", 
                sep = ""))
            if (!is.null(fit$model[[2]])) 
                rnames <- c(rnames, paste(cov.names[fit$model[[2]]], 
                  ": (", links[2], ")", sep = ""))
            rnames <- c(rnames, paste("Xi: (", links[3], ")", 
                sep = ""))
            if (!is.null(fit$model[[3]])) 
                rnames <- c(rnames, paste(cov.names[fit$model[[3]]], 
                  ": (", links[3], ")", sep = ""))
            rownames(c1) <- rnames
# Try to assign parameter names to the fitted object for summary purposes.
        dd$models[[number.of.models+1]]$parameter.names <- rnames
	dd$models[[number.of.models+1]]$summary1 <- c1
 if( is.nothing) assignCMD <- "assign( \"extRemesData\", dd, pos=\".GlobalEnv\")"
        else assignCMD <- paste( "assign( \"", full.list[data.select], "\", dd, pos = \".GlobalEnv\")", sep="")
               eval( parse( text=assignCMD))
               write( assignCMD, file="extRemes.log", append=TRUE)

            # tkinsert(base.txt, "end", paste("\t\t\t", colnames(c1)[1], "\t\t", colnames(c1)[2]))
		cat( paste("\t\t\t", colnames(c1)[1], "\t\t", colnames(c1)[2]))
            # tkinsert(base.txt, "end", nl2)
		cat( nl2)
            for (i in 1:dim(c1)[1]) {
                # tkinsert(base.txt, "end", paste(rownames(c1)[i], "\t", c1[i, 1], "\t", c1[i, 2], sep = "  "))
		cat( paste(rownames(c1)[i], "\t", c1[i, 1], "\t", c1[i, 2], sep = "  "))
                # tkinsert(base.txt, "end", nl2)
		cat( nl2)
            }
            # tkinsert(base.txt, "end", nl2)
		cat( nl2)
            nllh.str <- paste("\n Negative log likelihood:", 
                round(dd$models[[number.of.models+1]]$nllh, 4), "\n")
            # tkinsert(base.txt, "end", nllh.str)
		cat( nllh.str)
            # tkinsert(base.txt, "end", nl1)
		cat( nl1)
            tkyview.moveto(base.txt, 1)
            if (tclvalue(plot.diags) == 1) {
		# plotCMD <- "plot( fit)"
		plotCMD <- paste( "plot( dd[[\"models\"]][[\"rlarg.fit", jj+1, "\"]])", sep="")
		eval( parse( text=plotCMD))
		write( plotCMD, file="extRemes.log", append=TRUE)
		# plot( dd$models[[number.of.models+1]])
            }
        }

        tkdestroy(base)
        # tkconfigure(base.txt, state = "disabled")
    }
    rlarghelp <- function() {
	cat("\n", "Invokes the \'ismev\' function \'rlarg.fit\'.\n")
	cat( "Use \'help( rlarg.fit)\' for more help.\n")
        help(rlarg.fit)
    }
    endprog <- function() {
        tkdestroy(base)
    }
    base <- tktoplevel()
    tkwm.title(base, "Fit r-th largest order statistic model")
    data.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    top.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    bot.frm <- tkframe(base, borderwidth = 2, relief = "groove")
	 r.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    mu.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    sig.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    gam.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    method.frm <- tkframe(base, borderwidth = 2, relief = "groove")
    data.listbox <- tklistbox(data.frm, yscrollcommand = function(...) tkset(data.scroll, 
        ...), selectmode = "single", width = 20, height = 5, 
        exportselection = 0)
    data.scroll <- tkscrollbar(data.frm, orient = "vert", command = function(...) tkyview(data.listbox, 
        ...))
    temp <- ls(all = TRUE, name = ".GlobalEnv")
    full.list <- character(0)
    is.nothing <- TRUE
    for (i in 1:length(temp)) {
        if (is.null(class(get(temp[i])))) 
            next
        if ((class(get(temp[i]))[1] == "extRemesDataObject")) {
            tkinsert(data.listbox, "end", paste(temp[i]))
            full.list <- c(full.list, temp[i])
            is.nothing <- FALSE
        }
    }
    tkpack(tklabel(data.frm, text = "Data Object", padx = 4), 
        side = "left")
    tkpack(data.listbox, side = "left")
    tkpack(data.scroll, side = "right", fill = "y")
    tkpack(data.frm)
    tkbind(data.listbox, "<Button-1>", "")
    tkbind(data.listbox, "<ButtonRelease-1>", refresh)

    top.r <- tkframe(top.frm, borderwidth = 2)
    top.l <- tkframe(top.frm, borderwidth = 2)

resp.listbox <-
        tklistbox(top.l,yscrollcommand=function(...)tkset(resp.scroll,...),
                        selectmode="multiple",width=35,height=4,exportselection=0)
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

    plot.but <- tkcheckbutton(top.r, text = "Plot diagnostics", 
        variable = plot.diags)
    tkpack(plot.but, side = "right")
    tkpack(top.l, top.r, side = "left")
    tkbind(resp.listbox, "<ButtonRelease-1>", redolists)

# r frame
    r.entry <- tkentry(r.frm, textvariable = r.value, width = 2)
    tkpack(tklabel(r.frm, text = "r", padx = 4), r.entry, side = "left")

# mu frame
    mu.l <- tkframe(mu.frm, borderwidth = 2)
    mu.r <- tkframe(mu.frm, borderwidth = 2)
    mu.covlist <- tklistbox(mu.l, yscrollcommand = function(...) tkset(mu.covscr, 
        ...), selectmode = "multiple", width = 15, height = 4, 
        exportselection = 0)
    mu.covscr <- tkscrollbar(mu.l, orient = "vert", command = function(...) tkyview(mu.covlist, 
        ...))
    if (is.nothing) {
        for (i in 1:ncol(extRemesData$data)) tkinsert(mu.covlist, 
            "end", paste(colnames(extRemesData$data)[i]))
    }
    else tkinsert(mu.covlist, "end", "")
    tkpack(tklabel(mu.l, text = "Location parameter (mu):", padx = 4), side = "left")
    tkpack(mu.covlist, side = "left")
    tkpack(mu.covscr, side = "right", fill = "y")
    tkpack(tklabel(mu.r, text = "Link:"), side = "left")
    for (i in c("identity", "log")) {
        tmp <- tkradiobutton(mu.r, text = i, value = i, variable = mu.link)
        tkpack(tmp, anchor = "w")
    }
    tkpack(mu.r, side = "right")
    tkpack(mu.l, side = "right")
    sig.l <- tkframe(sig.frm, borderwidth = 2)
    sig.r <- tkframe(sig.frm, borderwidth = 2)
    sig.covlist <- tklistbox(sig.l, yscrollcommand = function(...) tkset(sig.covscr, 
        ...), selectmode = "multiple", width = 15, height = 4, 
        exportselection = 0)
    sig.covscr <- tkscrollbar(sig.l, orient = "vert", command = function(...) tkyview(sig.covlist, 
        ...))
    if (is.nothing) {
        for (i in 1:ncol(extRemesData$data)) tkinsert(sig.covlist, 
            "end", paste(colnames(extRemesData$data)[i]))
    }
    else tkinsert(sig.covlist, "end", "")
    tkpack(tklabel(sig.l, text = "Scale parameter (sigma):", padx = 4), side = "left")
    tkpack(sig.covlist, side = "left")
    tkpack(sig.covscr, side = "right", fill = "y")
    tkpack(tklabel(sig.r, text = "Link:"), side = "left")
    for (i in c("identity", "log")) {
        tmp <- tkradiobutton(sig.r, text = i, value = i, variable = sig.link)
        tkpack(tmp, anchor = "w")
    }
    tkpack(sig.r, side = "right")
    tkpack(sig.l, side = "right")
    gam.l <- tkframe(gam.frm, borderwidth = 2)
    gam.r <- tkframe(gam.frm, borderwidth = 2)
    gam.covlist <- tklistbox(gam.l, yscrollcommand = function(...) tkset(gam.covscr, 
        ...), selectmode = "multiple", width = 15, height = 4, 
        exportselection = 0)
    gam.covscr <- tkscrollbar(gam.l, orient = "vert", command = function(...) tkyview(gam.covlist, 
        ...))
    if (is.nothing) {
        for (i in 1:ncol(extRemesData$data)) tkinsert(gam.covlist, 
            "end", paste(colnames(extRemesData$data)[i]))
    }
    else tkinsert(gam.covlist, "end", "")
    tkpack(tklabel(gam.l, text = "Shape parameter (xi):", padx = 4), side = "left")
    tkpack(gam.covlist, side = "left")
    tkpack(gam.covscr, side = "right", fill = "y")
    tkpack(tklabel(gam.r, text = "Link:"), side = "left")
    for (i in c("identity", "log")) {
        tmp <- tkradiobutton(gam.r, text = i, value = i, variable = gam.link)
        tkpack(tmp, anchor = "w")
    }
    tkpack(gam.r, side = "right")
    tkpack(gam.l, side = "right")
    method.listbox <- tklistbox(method.frm, yscrollcommand = function(...) tkset(methodscr, 
        ...), selectmode = "single", width = 50, height = 1, 
        exportselection = 0)
    methodscr <- tkscrollbar(method.frm, orient = "vert", command = function(...) tkyview(method.listbox, 
        ...))
    tkinsert(method.listbox, "end", paste("Nelder-Mead"))
    tkinsert(method.listbox, "end", paste("BFGS quasi-Newton"))
    tkinsert(method.listbox, "end", paste("Conjugate Gradients"))
    tkinsert(method.listbox, "end", paste("L-BFGS-B"))
    tkinsert(method.listbox, "end", paste("Simulated Annealing (Belisle 1992)"))
    tkpack(tklabel(method.frm, text = "Optimization Method", 
        padx = 4), side = "left")
    tkpack(method.listbox, methodscr, side = "left")
    tkpack(method.frm)
    sub.but <- tkbutton(bot.frm, text = "OK", command = submit)
    tkbind(sub.but, "<Return>", submit)
    quit.but <- tkbutton(bot.frm, text = "Cancel", command = endprog)
    tkbind(quit.but, "<Return>", endprog)
    help.but <- tkbutton(bot.frm, text = "Help", command = rlarghelp)
    tkbind(help.but, "<Return>", rlarghelp)
    tkpack(sub.but, quit.but, side = "left")
    tkpack(help.but, side = "right")
    tkpack(top.frm, side = "top")
	tkpack( r.frm, fill="x")
    tkpack(mu.frm, fill = "x")
    tkpack(sig.frm, fill = "x")
    tkpack(gam.frm, fill = "x")
    tkpack(bot.frm, side = "bottom")
}
