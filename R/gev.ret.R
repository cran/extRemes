"gev.ret" <-
function (A, b, c, p = 100, ...) 
{
    if (nargs() != 2 | nargs() != 4) 
        (" You must either enter a gev or pp model object or a mu, sig and xi.")
    if (nargs() == 1) {
        mu <- A$mle[1]
        sig <- A$mle[2]
        xi <- A$mle[3]
    }
    if (nargs() == 2) {
        mu <- A$mle[1]
        sig <- A$mle[2]
        xi <- A$mle[3]
        p <- b
    }
    if (nargs() == 3) {
        mu <- A
        sig <- b
        xi <- c
    }
    if (nargs() == 4) {
        mu <- A
        sig <- b
        xi <- c
    }
    p <- 1/p
    z <- mu - (sig/xi) * (1 - (-log(1 - p))^(-xi))
    return(z)
}
