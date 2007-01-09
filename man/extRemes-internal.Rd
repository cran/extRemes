\name{extRemes internal }
\alias{DataSummaryGUI}
\alias{extremalind.gui}
\alias{gev.ret}
\alias{gev.rlci}
\alias{gpd.ret}
\alias{gpd.rlci}
\alias{gpd.rl2}
\alias{indicatorTransform.gui}
\alias{llhrt.gui}
\alias{plot.gev.fit}
\alias{plot.gpd.fit}
\alias{plot.gum.fit}
\alias{plot.pp.fit}
\alias{plot.rlarg.fit}
\alias{rl.gev.fit}
\alias{rl.gpd.fit}
\alias{scatterplot.gui}
\alias{affine.gui}
\alias{ppfit.gui}
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
\alias{describe2}
\alias{gevsim.gui}
\alias{logdr.gui}
\alias{stats2}
\alias{summary.rlarg.fit}
\alias{gevsim.gui}
\alias{logtrans.gui}
\alias{trigtrans.gui}
\alias{mrlplot.gui}
\alias{view.data}
\alias{gpdf.gui}
\alias{negtrans.gui}
\alias{zzz}
\alias{fitdiag.gui}
\alias{gpdfitrange.gui}
\alias{fitsummary.gui}
\alias{gpdparamCI.gui}
\alias{poisson.gui}
\alias{scrubber.gui}
\alias{write.extRemesMainMessage}

\title{
  extRemes internal and secondary functions
}

\description{
Listed below are supporting fucntions for the major methods in extRemes.
Most of the functions control GUI interfaces for the R package ismev.
}

\usage{
DataSummaryGUI(base.txt)
extremalind.gui( base.txt)
gev.ret(A, b, c, p = 100, ...)
gev.rlci(z, m, prange = NULL, conf = 0.95, nint = 100)
gpd.ret(z, p)
gpd.rlci(z, m, prange=NULL, npy = 365, conf = 0.95, nint = 100)
gpd.rl2(a, u, la, n, npy, mat, dat, xdat, ci=0.05, add.ci=FALSE)
indicatorTransform.gui(base.txt)
llhrt.gui(base.txt)
\method{plot}{gev.fit}(x, ...)
\method{plot}{gpd.fit}(x, ...)
\method{plot}{gum.fit}(x, ...)
\method{plot}{pp.fit}(x, ...)
\method{plot}{rlarg.fit}(x, ...)
rl.gev.fit(a, mat, dat)
rl.gpd.fit(a, u, la, n, npy, mat, dat, xdat)
scatterplot.gui(base.txt)
affine.gui(base.txt)
ppfit.gui(base.txt)
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
\method{summary}{gev.fit}(object, ...)
\method{summary}{gpd.fit}(object, ...)
load.data(base.txt)
\method{summary}{pp.fit}(object, ...)
diagnostic.plots 
describe2(x)
gevsim.gui(base.txt) 
logdr.gui(base.txt) 
stats2(x, by)
\method{summary}{rlarg.fit}(object, ...)
logtrans.gui(base.txt)
trigtrans.gui(base.txt)
mrlplot.gui(base.txt)
view.data(base.txt)
gpdf.gui(base.txt)
negtrans.gui(base.txt)
zzz 
fitdiag.gui(base.txt)
gpdfitrange.gui(base.txt)
fitsummary.gui(base.txt)
gpdparamCI.gui(base.txt)
poisson.gui(base.txt)
scrubber.gui(base.txt)
write.extRemesMainMessage(txt)
}
\keyword{internal}
\keyword{misc}
