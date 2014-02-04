
library(RJSONIO)

min.loss <- function(x) { min(x$loss) }

density.approximate <- function( x )
{
	x.stats <- list('mean' = mean(x), 'var' = var(x), 'min' = min(x), 'max' = max(x), 'n' = length(x))
	
	#
	#
	#  compare from pool of possible distributions
	#
	#
	
	# normalize x
	x.normalized <- (x - x.stats$mean) / sqrt(x.stats$var)
	
	comparisons <- list()	
	n.comparisons <- 100000
	
	# normal
	comparisons[[ 'normal' ]] <- list('params' = list('mean' = x.stats$mean, 'var' = x.stats$var ), 'loss' = density.difference( x, rnorm(n.comparisons, x.stats$mean, sqrt(x.stats$var) ) ) )

	# t
	comparisons[[ 't' ]] <- list('params' = list('mean' = x.stats$mean, 'var' = x.stats$var, 'df' = x.stats$n - 1), 'loss' = density.difference( x.normalized, rt(n.comparisons, x.stats$n - 1  ) ) )
	
	# chi-squared (ignore for now)
	# comparisons[[ 'chi.sq' ]] <- list('params' = list('mean' = x.stats$mean, 'var' = 2 * x.stats$mean, 'df' = x.stats$mean), 'loss' = density.difference( x,rchisq(n.comparisons, x.stats$mean) ) )
	
	# gamma (a = shape, b = rate) -- support [ 0 , Inf )
	if( x.stats$min > 0 )
	{
		b <- x.stats$mean / x.stats$var
		a <- x.stats$mean * b
	
		comparisons[[ 'gamma' ]] <- list('params' = list('a' = a, 'b' = b), 'loss' = density.difference( x, rgamma(n.comparisons, shape = a, rate = b) ) )
	}
	
	#
	#
	#  just return the best one
	#
	#
	
	best <- names(which.min( unlist(lapply(comparisons, min.loss)) ))
	
	return( list('dist' = best, 'loss' = comparisons[[ best ]][[ 'loss' ]], 'params' = comparisons[[ best ]][[ 'params' ]] ) )
}


density.difference <- function( x1, x2, loss.function = loss.scaled.sse )
{
	# make sure axes are aligned
	# density estimation 'tuning' parameters
	bw <- bw.nrd( x1 )
	cut <- 3
	
	ranges <- c( range(x1, na.rm = TRUE), range(x2, na.rm = TRUE) )
	
	density.min <- min( ranges )
	density.max <- max( ranges )
	
	# get the densities in the same window
	d1 <- density( x1, bw = bw, cut = cut, from = density.min, to = density.max )
	d2 <- density( x2, bw = bw, cut = cut, from = density.min, to = density.max )
	
	# return the loss
	return( loss.function( d1$y, d2$y ) )
}

#
#
#   loss functions
#
#

loss.scaled.sse <- function( y1, y2 )
{
	scaled.diff <- ( y1 - y2 ) / max( y1 )
	return(  sum( scaled.diff ^ 2 ) )
}


#
# saving
#

serialize.strip <- function( x )
{
	return( gsub("[\n ]", "", x) )
}


density.serialize <- function( x )
{
	return( serialize.strip( toJSON( density.approximate(x) ) ) )
}


#
# testing
#
"
setwd('~/Documents/kaggle/ncaa/')

system.time({ player <- read.csv('data/output/mcmc/2009/2/threem/26866') })
head(player)

comparisons <- compare.density( player$int )
"

