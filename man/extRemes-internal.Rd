\name{extRemes internal }
\alias{bisearch}
\alias{extremalind.gui}
\alias{gev.ret}
\alias{gev.rlci}
\alias{gpd.ret}
\alias{gpd.rlci}
\alias{indicatorTransform.gui}
\alias{llhrt.gui}
\alias{plot.gev.fit}
\alias{plot.gpd.fit}
\alias{plot.gum.fit}
\alias{plot.pp.fit}
\alias{plot.rlarg.fit}
\alias{scatterplot.gui}
\alias{damage}  
\alias{Denmint}
\alias{Denver}
\alias{Flood}
\alias{ftcanmax}
\alias{FtCoPrec}
\alias{HEAT}
\alias{Peak}
\alias{PORTw}
\alias{Potomac}
\alias{Rsum}
\alias{SEPTsp}
\alias{Tphap}
\alias{YDarwin}
\alias{extremes.gui}
\alias{affine.gui}
\alias{ppfit.gui}
\alias{gevdiag.gui}
\alias{ppfitrange.gui}
\alias{gevf.gui}
\alias{gpdsim.gui}
\alias{gevparamCI.gui}
\alias{hist.gev.fit}
\alias{hist.gpd.fit}
\alias{histogram.gui}
\alias{hist.pp.fit}
\alias{rlargf.gui}
\alias{decluster.gui}
\alias{deviancestat}
\alias{rlplot.gui}
\alias{summary.gev.fit}
\alias{summary.gpd.fit}
\alias{load.data}
\alias{summary.pp.fit}
\alias{diagnostic.plots}
\alias{gevsim.gui}
\alias{logdr.gui}
\alias{summary.rlarg.fit}
\alias{gevsim.gui}
\alias{logtrans.gui}
\alias{trigtrans.gui}
\alias{gpddiag.gui}
\alias{mrlplot.gui}
\alias{view.data}
\alias{gpdf.gui}
\alias{negtrans.gui}
\alias{zzz}
\alias{fitdiag.gui}
\alias{gpdfitrange.gui}
\alias{fitsummary.gui}
\alias{gpdparamCI.gui}
\alias{plotTS.gui}
\alias{poisson.gui}

\title{
  extRemes internal and secondary functions
}

\description{
Listed below are supporting fucntions for the major methods in extRemes.
Most of the functions control GUI interfaces for the R package ismev.
}

\usage{
bisearch(x1, x2, f, tol = 1e-07, niter = 25, upcross.level = 0)
extremalind.gui( base.txt)
gev.ret(A, b, c, p = 100, ...)
gev.rlci(z, m, prange = NULL, conf = 0.95, nint = 100)
gpd.ret(z, p)
gpd.rlci(z, m, prange=NULL, npy = 365, conf = 0.95, nint = 100)
indicatorTransform.gui(base.txt)
llhrt.gui(base.txt)
plot.gev.fit(x, ...)
plot.gpd.fit(x, ...)
plot.gum.fit(x, ...)
plot.pp.fit(x, ...)
plot.rlarg.fit(x, ...)
scatterplot.gui(base.txt)
damage
Denmint
Denver
Flood
ftcanmax
FtCoPrec
HEAT
Peak
PORTw
Potomac
Rsum
SEPTsp
Tphap
YDarwin
extremes.gui()
affine.gui(base.txt)
ppfit.gui(base.txt)
gevdiag.gui(base.txt)
ppfitrange.gui(base.txt) 
gevf.gui(base.txt) 
gpdsim.gui(base.txt) 
gevparamCI.gui(base.txt) 
hist.gev.fit(x, breaks.method="Sturges", ...)
hist.gpd.fit(x, breaks.method="Sturges", ...)
hist.pp.fit(x, breaks.method="Sturges", ...)
rlargf.gui(base.txt)
rlplot.gui(base.txt)
decluster.gui(base.txt)
deviancestat(l1, l2, v, alpha=0.05)
histogram.gui(base.txt) 
summary.gev.fit(object, ...)
summary.gpd.fit(object, ...)
load.data(base.txt)
summary.pp.fit(object, ...)
diagnostic.plots 
gevsim.gui(base.txt) 
logdr.gui(base.txt) 
summary.rlarg.fit(object, ...)
logtrans.gui(base.txt)
trigtrans.gui(base.txt)
gpddiag.gui(base.txt)
mrlplot.gui(base.txt)
view.data(base.txt)
gpdf.gui(base.txt)
negtrans.gui(base.txt)
zzz 
fitdiag.gui(base.txt)
gpdfitrange.gui(base.txt)
fitsummary.gui(base.txt)
gpdparamCI.gui(base.txt)
plotTS.gui(base.txt)
poisson.gui(base.txt)
}
\keyword{internal}
\keyword{misc}
