deviancestat <- function( l1, l2, v, alpha=0.05) {
DS <- -2*(l2-l1)
c.alpha <- qchisq( alpha, df=v, lower.tail=FALSE)
p.val <- pchisq( abs( DS), df=v, lower.tail=FALSE)
return( list( DS=DS, p.value=p.val, c.alpha=c.alpha))
}
