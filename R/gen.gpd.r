gen.gpd <- function(n=100, sigma=1, xi=0.2, u=0) {
if( xi != 0) out <- (sigma/xi)*((1-runif(n))^(-xi)-1)+u
else out <- rexp(n, rate=1/sigma)+u
return(out)
}
