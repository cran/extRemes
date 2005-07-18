clearlog <- function( base.txt) {
out <- paste( "Cleared log file: ", date(), sep="")
print( out)
write( out, file="extRemes.log", append=FALSE)
invisible()
}
