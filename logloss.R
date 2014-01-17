log.loss <- function( y, y.hat, n )
{
	return( -(1 / n) * sum( (y * log(y.hat)) + ( (1 - y) * log(1 - y.hat) ) ) )
}