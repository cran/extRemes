tformRankFrechet <- function( x, na.action = na.fail, ... ) {

    x <- na.action( x )
    N <- length( x )

    res <- rank( x, ... ) / ( N + 1 )

    res <- -1 / log( res )

    return( res )

}
