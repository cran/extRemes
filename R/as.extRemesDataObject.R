as.extRemesDataObject <- function(x) {
out <- list()
if( is.matrix( x) | is.data.frame(x)) {
	out$data <- x
	if( is.null( colnames(x))) colnames( out$data) <- paste("x",1:length(x),sep="")
	else if( is.null( colnames(out$data))) colnames( out$data) <- colnames( x)
} else if( is.vector(x)) {
	out$data <- cbind( 1:length( x), x)
	if( length( as.character(substitute(x)))==1) colnames( out$data) <- c("obs", as.character(substitute(x)))
	else colnames( out$data) <- c("obs", "x")
	}
else stop("as.extRemesDataObject: x must be a data frame, matrix or vector.")
class( out) <- "extRemesDataObject"
return( out)
}
