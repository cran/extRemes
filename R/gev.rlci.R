"gev.rlci" <-
function (z, m, prange=NULL, conf = 0.95, nint = 100) 
{
#   cat("If routine fails, try changing plotting interval", fill = TRUE)
p <- 1/m
v <- numeric(nint)
sol <- c(z$mle[2], z$mle[3])

if( is.null( prange)) {
	prange <- range( z$data, na.rm=TRUE)
	prange[1] <- describe(z$data)[5]
	prange[2] <- prange[2] + 5*qchisq(conf,1)
	}
x <- seq(prange[1], prange[2],,nint)

# Internal function begins here.
    gev.plik <- function(a) {
        if (p != 0) 
            mu <- xp - a[1]/a[2] * ((-log(1 - p))^(-a[2]) - 1)
        else mu <- xp + a[1]/a[2]
        if (abs(a[2]) < 10^(-6)) 
            l <- gum.lik(c(a[1], mu))
        else {
            y <- (z$data - mu)/a[1]
            y <- 1 + a[2] * y
            if (any(y <= 0) || a[1] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(y^(-1/a[2])) + 
                sum(log(y)) * (1/a[2] + 1)
        }
        l
    } # end of gev.plik internal function

for (i in 1:nint) {
        xp <- x[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v[i] <- opt$value
	} # end of for i loop

plot(x, -v, type = "l", xlab = "Return Level", ylab = " Profile Log-likelihood")
ma <- -z$nllh
abline(h = ma, col = 4)
abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
mah <- abs(v-rep(ma-0.5*qchisq(conf,1), length(v)))
abline( v=order(mah)[1:2], lty=2)
invisible()
}
